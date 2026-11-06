# Sign in with ChatGPT para StoryTrack (uso local / open source).
# Los tokens se guardan fuera del repositorio, en el perfil local de Windows.

storytrack_chatgpt_packages <- c("httr2", "openssl", "curl")

storytrack_chatgpt_ready <- function() {
  all(vapply(storytrack_chatgpt_packages, requireNamespace, logical(1), quietly = TRUE))
}

storytrack_auth_dir <- function() {
  base <- Sys.getenv("LOCALAPPDATA", unset = "")
  if (!nzchar(base)) base <- path.expand("~")
  dir <- file.path(base, "StoryTrack", "chatgpt")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

storytrack_profile_path <- function() file.path(storytrack_auth_dir(), "profile.json")
storytrack_host_path <- function() file.path(storytrack_auth_dir(), "host_id.txt")
storytrack_client_id_path <- function() file.path(storytrack_auth_dir(), "client_id.txt")

storytrack_read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

storytrack_write_json <- function(value, path) {
  temp <- tempfile(pattern = "storytrack-", tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  jsonlite::write_json(value, temp, auto_unbox = TRUE, pretty = FALSE, null = "null")
  if (file.exists(path)) unlink(path)
  if (!file.rename(temp, path)) stop("No pude guardar la sesión local de ChatGPT.")
  invisible(path)
}

storytrack_b64url <- function(raw) {
  x <- openssl::base64_encode(raw, linebreaks = FALSE)
  gsub("=+$", "", chartr("+/", "-_", x))
}

storytrack_b64url_decode <- function(x) {
  x <- chartr("-_", "+/", x)
  pad <- (4 - nchar(x) %% 4) %% 4
  openssl::base64_decode(paste0(x, strrep("=", pad)))
}

storytrack_random <- function(n = 32L) {
  storytrack_b64url(openssl::rand_bytes(n))
}

storytrack_host_id <- function() {
  path <- storytrack_host_path()
  if (file.exists(path)) {
    old <- trimws(readLines(path, warn = FALSE, n = 1L))
    if (grepl("^urn:uuid:[0-9a-f-]{36}$", old)) return(old)
  }
  b <- as.integer(openssl::rand_bytes(16L))
  b[7] <- bitwOr(bitwAnd(b[7], 15L), 64L)
  b[9] <- bitwOr(bitwAnd(b[9], 63L), 128L)
  h <- sprintf("%02x", b)
  uuid <- paste0(paste0(h[1:4], collapse = ""), "-",
                 paste0(h[5:6], collapse = ""), "-",
                 paste0(h[7:8], collapse = ""), "-",
                 paste0(h[9:10], collapse = ""), "-",
                 paste0(h[11:16], collapse = ""))
  value <- paste0("urn:uuid:", uuid)
  writeLines(value, path, useBytes = TRUE)
  value
}

storytrack_urlencode <- function(x) utils::URLencode(enc2utf8(x), reserved = TRUE)

storytrack_query_string <- function(values) {
  paste(paste0(names(values), "=", vapply(values, storytrack_urlencode, character(1))),
        collapse = "&")
}

storytrack_parse_query <- function(query) {
  if (is.null(query) || !nzchar(query)) return(list())
  # httpuv entrega la consulta con el prefijo "?" en este callback.
  query <- sub("^\\?", "", query)
  pieces <- strsplit(query, "&", fixed = TRUE)[[1]]
  out <- list()
  for (piece in pieces) {
    # Dividir solo en el primer "="; el valor OAuth puede contener otros.
    pos <- regexpr("=", piece, fixed = TRUE)[1]
    key <- if (pos < 0L) piece else substr(piece, 1L, pos - 1L)
    value <- if (pos < 0L) "" else substr(piece, pos + 1L, nchar(piece))
    key <- utils::URLdecode(key)
    value <- utils::URLdecode(gsub("+", " ", value, fixed = TRUE))
    out[[key]] <- value
  }
  out
}

storytrack_der_length <- function(n) {
  if (n < 128L) return(as.raw(n))
  bytes <- integer()
  while (n > 0) {
    bytes <- c(n %% 256L, bytes)
    n <- n %/% 256L
  }
  c(as.raw(128L + length(bytes)), as.raw(bytes))
}

storytrack_der <- function(tag, content) {
  c(as.raw(tag), storytrack_der_length(length(content)), content)
}

storytrack_jwk_rsa_pem <- function(jwk) {
  if (!identical(jwk$kty, "RSA") || is.null(jwk$n) || is.null(jwk$e)) {
    stop("OpenAI devolvió una llave de firma que StoryTrack no reconoce.")
  }
  der_integer <- function(value) {
    bytes <- storytrack_b64url_decode(value)
    bytes <- bytes[which(c(as.integer(bytes) != 0L, TRUE))[1]:length(bytes)]
    if (length(bytes) == 0L) bytes <- as.raw(0L)
    if (as.integer(bytes[1]) >= 128L) bytes <- c(as.raw(0L), bytes)
    storytrack_der(2L, bytes)
  }
  rsa_key <- storytrack_der(48L, c(der_integer(jwk$n), der_integer(jwk$e)))
  algorithm <- as.raw(c(48, 13, 6, 9, 42, 134, 72, 134, 247, 13, 1, 1, 1, 5, 0))
  spki <- storytrack_der(48L, c(algorithm, storytrack_der(3L, c(as.raw(0L), rsa_key))))
  b64 <- openssl::base64_encode(spki, linebreaks = FALSE)
  lines <- substring(b64, seq.int(1L, nchar(b64), by = 64L),
                     pmin(seq.int(64L, nchar(b64) + 63L, by = 64L), nchar(b64)))
  paste(c("-----BEGIN PUBLIC KEY-----", lines, "-----END PUBLIC KEY-----"), collapse = "\n")
}

storytrack_verify_id_token <- function(token, client_id, expected_nonce) {
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 3L) stop("El inicio de sesión devolvió un ID token inválido.")
  header <- jsonlite::fromJSON(rawToChar(storytrack_b64url_decode(parts[1])), simplifyVector = FALSE)
  claims <- jsonlite::fromJSON(rawToChar(storytrack_b64url_decode(parts[2])), simplifyVector = FALSE)
  if (!identical(header$alg, "RS256") || is.null(header$kid)) {
    stop("El ID token usa un método de firma no reconocido.")
  }

  discovery <- httr2::request("https://auth.openai.com/.well-known/openid-configuration") |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
  jwks <- httr2::request(discovery$jwks_uri) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
  key <- Filter(function(item) identical(item$kid, header$kid), jwks$keys)[[1]]
  pem <- storytrack_jwk_rsa_pem(key)
  pem_path <- tempfile(fileext = ".pem")
  on.exit(unlink(pem_path), add = TRUE)
  writeLines(pem, pem_path, useBytes = TRUE)
  public_key <- openssl::read_pubkey(pem_path)
  signed <- charToRaw(paste(parts[1], parts[2], sep = "."))
  signature <- storytrack_b64url_decode(parts[3])
  if (!isTRUE(openssl::signature_verify(
    signed, signature, hash = openssl::sha256, pubkey = public_key
  ))) {
    stop("No pude verificar la firma de la respuesta de inicio de sesión.")
  }

  audience <- claims$aud
  if (is.list(audience)) audience <- unlist(audience, use.names = FALSE)
  if (!identical(claims$iss, "https://auth.openai.com") ||
      !(client_id %in% audience) ||
      is.null(claims$exp) || as.numeric(claims$exp) <= as.numeric(Sys.time()) ||
      !identical(claims$nonce, expected_nonce)) {
    stop("La identidad o nonce de ChatGPT no pasó la validación de seguridad.")
  }
  claims
}

storytrack_begin_signin <- function(flow) {
  if (!storytrack_chatgpt_ready()) {
    stop("Faltan paquetes de R. En la consola ejecuta: install.packages(c('httr2', 'openssl', 'curl'))")
  }
  if (!is.null(flow$server)) stop("Ya hay un inicio de sesión esperando autorización.")

  profile <- storytrack_read_json(storytrack_profile_path())
  saved_client_id <- if (!is.null(profile$client_id)) profile$client_id else if (file.exists(storytrack_client_id_path())) {
    trimws(readLines(storytrack_client_id_path(), warn = FALSE, n = 1L))
  } else {
    NULL
  }
  port <- 1455L
  redirect_uri <- sprintf("http://127.0.0.1:%d/auth/callback", port)
  state <- storytrack_random(24L)
  nonce <- storytrack_random(24L)
  verifier <- storytrack_random(48L)
  challenge <- storytrack_b64url(openssl::sha256(charToRaw(verifier)))
  flow$pending <- list(
    client_id = if (is.null(saved_client_id) || !nzchar(saved_client_id)) "dynamic_agent_client" else saved_client_id,
    redirect_uri = redirect_uri,
    state = state,
    nonce = nonce,
    verifier = verifier
  )
  flow$result <- NULL

  callback_app <- list(call = function(req) {
    # El navegador también puede pedir /favicon.ico; nunca tratar esas
    # solicitudes secundarias como si fueran el retorno de OAuth.
    if (!identical(req$PATH_INFO, "/auth/callback")) {
      return(list(status = 404L,
                  headers = list("Content-Type" = "text/plain; charset=utf-8"),
                  body = "Ruta no encontrada."))
    }
    query <- storytrack_parse_query(req$QUERY_STRING)
    pending <- flow$pending
    body <- "StoryTrack recibió la respuesta. Puedes volver a la aplicación."
    if (!is.null(flow$result)) {
      body <- "StoryTrack ya recibió esta respuesta. Puedes volver a la aplicación."
    } else if (is.null(query$state)) {
      body <- "La respuesta no incluyó los datos de autorización esperados. Regresa a StoryTrack y vuelve a conectar."
    } else if (is.null(pending) || !identical(query$state, pending$state)) {
      flow$result <- list(error = "La verificación de seguridad falló. Intenta conectar de nuevo.")
      body <- "No pude validar este intento. Regresa a StoryTrack y vuelve a conectar."
    } else if (!is.null(query$error)) {
      flow$result <- list(error = paste("ChatGPT no autorizó la conexión:", query$error))
      body <- "La autorización no se completó. Regresa a StoryTrack."
    } else if (is.null(query$code) || !nzchar(query$code)) {
      campos <- paste(names(query), collapse = ", ")
      flow$result <- list(error = paste0(
        "ChatGPT devolvió una respuesta sin código de autorización. Parámetros recibidos: ",
        if (nzchar(campos)) campos else "ninguno", ". Intenta conectar de nuevo."
      ))
      body <- "La respuesta no incluyó un código de autorización. Regresa a StoryTrack."
    } else {
      flow$result <- query
    }
    list(status = 200L,
         headers = list("Content-Type" = "text/plain; charset=utf-8"),
         body = body)
  })
  flow$server <- httpuv::startServer("127.0.0.1", port, callback_app)

  params <- list(
    client_id = flow$pending$client_id,
    response_type = "code",
    redirect_uri = redirect_uri,
    scope = "openid profile email offline_access resource.invoke chatgpt.tokens.use.direct",
    resource = "https://api.openai.com/v1",
    state = state,
    nonce = nonce,
    code_challenge_method = "S256",
    code_challenge = challenge
  )
  if (identical(flow$pending$client_id, "dynamic_agent_client")) {
    params$agent_name_hint <- "StoryTrack"
    params$ext_agent_host_id <- storytrack_host_id()
  } else if (!is.null(profile$id_token)) {
    params$id_token_hint <- profile$id_token
  }
  auth_url <- paste0("https://auth.openai.com/api/accounts/authorize?", storytrack_query_string(params))
  utils::browseURL(auth_url)
  invisible(TRUE)
}

storytrack_finish_signin <- function(flow, callback) {
  pending <- flow$pending
  if (!is.null(callback$error)) stop(callback$error)
  if (is.null(callback$code)) stop("ChatGPT no devolvió un código de autorización.")
  client_id <- if (identical(pending$client_id, "dynamic_agent_client")) callback$client_id else pending$client_id
  if (is.null(client_id) || !nzchar(client_id) || identical(client_id, "dynamic_agent_client")) {
    stop("No recibí el identificador de registro de StoryTrack.")
  }
  # OpenAI crea el registro antes de intercambiar el código. Conservar su ID
  # permite reintentar la autorización si el código vence o falla el intercambio.
  writeLines(client_id, storytrack_client_id_path(), useBytes = TRUE)

  token_response <- httr2::request("https://auth.openai.com/api/accounts/oauth/token") |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      grant_type = "authorization_code",
      client_id = client_id,
      code = callback$code,
      code_verifier = pending$verifier,
      redirect_uri = pending$redirect_uri,
      resource = "https://api.openai.com/v1"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
  if (is.null(token_response$id_token) || is.null(token_response$access_token)) {
    stop("ChatGPT no devolvió los permisos necesarios. Vuelve a conectar y autoriza el uso del plan.")
  }
  claims <- storytrack_verify_id_token(token_response$id_token, client_id, pending$nonce)
  scopes <- strsplit(token_response$scope %||% "", "\\s+")[[1]]
  if (!("chatgpt.tokens.use.direct" %in% scopes)) {
    stop("La sesión inició, pero no se autorizó el uso del plan de ChatGPT para StoryTrack.")
  }
  profile <- list(
    client_id = client_id,
    subject = claims$sub,
    email = claims$email %||% "Cuenta de ChatGPT",
    id_token = token_response$id_token,
    access_token = token_response$access_token,
    refresh_token = token_response$refresh_token,
    expires_at = as.numeric(Sys.time()) + as.numeric(token_response$expires_in %||% 3600),
    scope = token_response$scope %||% ""
  )
  storytrack_write_json(profile, storytrack_profile_path())
  profile
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

storytrack_refresh_profile <- function(profile) {
  if (is.null(profile$access_token) ||
      as.numeric(profile$expires_at %||% 0) < as.numeric(Sys.time()) + 60) {
    if (is.null(profile$refresh_token)) stop("La conexión de ChatGPT venció. Pulsa Conectar con ChatGPT de nuevo.")
    refreshed <- httr2::request("https://auth.openai.com/api/accounts/oauth/token") |>
      httr2::req_method("POST") |>
      httr2::req_body_form(
        grant_type = "refresh_token",
        client_id = profile$client_id,
        refresh_token = profile$refresh_token,
        resource = "https://api.openai.com/v1"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = FALSE)
    profile$access_token <- refreshed$access_token
    profile$refresh_token <- refreshed$refresh_token %||% profile$refresh_token
    profile$expires_at <- as.numeric(Sys.time()) + as.numeric(refreshed$expires_in %||% 3600)
    profile$scope <- refreshed$scope %||% profile$scope
    storytrack_write_json(profile, storytrack_profile_path())
  }
  if (!grepl("(^|\\s)chatgpt.tokens.use.direct(\\s|$)", profile$scope %||% "")) {
    stop("La cuenta conectada no autorizó el uso del plan de ChatGPT para StoryTrack.")
  }
  profile
}

storytrack_chatgpt_models <- function(profile) {
  profile <- storytrack_refresh_profile(profile)
  response <- httr2::request("https://api.openai.com/v1/models") |>
    httr2::req_headers(Authorization = paste("Bearer", profile$access_token)) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
  models <- Filter(function(m) identical(m$visibility, "list"), response$models)
  if (!length(models)) stop("No hay modelos disponibles para esta cuenta en este momento.")
  list(profile = profile, model = models[[1]]$slug)
}

storytrack_api_error_text <- function(err) {
  if (is.null(err)) return("OpenAI no incluyó un código ni un mensaje de error.")
  if (is.character(err)) return(paste(err, collapse = " "))
  parts <- c(
    err$code %||% NULL,
    if (!is.null(err$param)) paste0("parámetro ", err$param),
    err$message %||% NULL
  )
  parts <- unlist(parts, use.names = FALSE)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts)) paste(parts, collapse = ": ") else "OpenAI devolvió un error sin detalles."
}

storytrack_chatgpt_response <- function(profile, instructions, input) {
  if (!storytrack_chatgpt_ready()) {
    stop("Faltan paquetes de R. En la consola ejecuta: install.packages(c('httr2', 'openssl', 'curl'))")
  }
  selected <- storytrack_chatgpt_models(profile)
  profile <- selected$profile
  payload <- jsonlite::toJSON(
    list(model = selected$model, instructions = instructions,
         input = list(list(role = "user", content = input)),
         store = FALSE, stream = TRUE),
    auto_unbox = TRUE, null = "null"
  )
  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    Authorization = paste("Bearer", profile$access_token),
    `Content-Type` = "application/json"
  )
  curl::handle_setopt(handle, customrequest = "POST", postfields = payload, failonerror = FALSE)

  buffer <- raw()
  response_body <- raw()
  answer <- ""
  completed <- FALSE
  stream_error <- NULL
  consume <- function(chunk) {
    response_body <<- c(response_body, chunk)
    buffer <<- c(buffer, chunk)
    txt <- rawToChar(buffer)
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    has_trailing_newline <- grepl("\n$", txt)
    if (has_trailing_newline) {
      buffer <<- raw()
    } else if (length(lines)) {
      buffer <<- charToRaw(tail(lines, 1L))
      lines <- head(lines, -1L)
    }
    for (line in lines) {
      if (!startsWith(line, "data: ")) next
      data <- substring(line, 7L)
      if (identical(data, "[DONE]")) next
      event <- tryCatch(jsonlite::fromJSON(data, simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(event$type)) next
      if (identical(event$type, "response.output_text.delta")) {
        answer <<- paste0(answer, event$delta %||% "")
      } else if (identical(event$type, "response.completed")) {
        completed <<- TRUE
      } else if (identical(event$type, "response.failed")) {
        err <- event$response$error %||% event$error
        stream_error <<- storytrack_api_error_text(err)
      } else if (identical(event$type, "error")) {
        err <- event$error %||% event
        stream_error <<- storytrack_api_error_text(err)
      }
    }
  }
  response <- curl::curl_fetch_stream(
    "https://api.openai.com/v1/responses", consume, handle = handle
  )
  if (response$status_code < 200L || response$status_code >= 300L) {
    body_text <- if (length(response_body)) rawToChar(response_body) else ""
    body_json <- tryCatch(jsonlite::fromJSON(body_text, simplifyVector = FALSE), error = function(e) NULL)
    err <- body_json$error %||% NULL
    if (is.null(err) && !is.null(body_json$detail)) {
      detail <- body_json$detail
    } else if (!is.null(err)) {
      detail <- storytrack_api_error_text(err)
    } else {
      detail <- if (nzchar(body_text)) body_text else "OpenAI no incluyó detalles en el cuerpo de la respuesta."
    }
    stop(paste0("OpenAI respondió HTTP ", response$status_code, ": ", detail))
  }
  if (!is.null(stream_error)) stop(stream_error)
  if (!completed) stop("La respuesta se interrumpió antes de completarse; vuelve a intentarlo.")
  if (!nzchar(trimws(answer))) stop("ChatGPT devolvió una respuesta vacía.")
  answer
}
