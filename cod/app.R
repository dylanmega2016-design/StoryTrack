# =============================================================================
# Storytrack - Interfaz Shiny (solo la parte visual)
# -----------------------------------------------------------------------------
# Esta app todavía NO está conectada a SpoilerGuard ni a la IA.
# Los dos puntos de conexión están marcados con "CONECTAR AQUÍ":
#   1. generar_recap()       -> resumen hasta el último episodio visto
#   2. responder_pregunta()  -> respuesta sobre personajes, hasta ese episodio
# Para conectar: reemplazar el cuerpo de esas funciones por la llamada real.
# Reciben: la serie (input$serie), la temporada y el episodio límite
# (el último capítulo marcado como visto) y la pregunta (input$pregunta).
#
# Ejecutar: abrir este archivo en RStudio y pulsar "Run App".
# Paquetes necesarios: shiny
# =============================================================================

library(shiny)

# -----------------------------------------------------------------------------
# Catálogo de prueba
# Estructura: serie -> portada + temporadas -> episodios (número = título)
# Para agregar series, copiar el bloque y poner la portada en data/portadas/
# -----------------------------------------------------------------------------
catalogo <- list(
  "The Big Bang Theory" = list(
    portada = "the_big_bang_theory.jpg",  # archivo dentro de data/portadas/
    anio = "2007-2019",
    nota = 8.1,                           # nota de prueba
    duracion = 21,                        # minutos por capítulo (aproximado)
    temporadas = list(
      "1" = c(
        "1"  = "Pilot",
        "2"  = "The Big Bran Hypothesis",
        "3"  = "The Fuzzy Boots Corollary",
        "4"  = "The Luminous Fish Effect",
        "5"  = "The Hamburger Postulate",
        "6"  = "The Middle-Earth Paradigm",
        "7"  = "The Dumpling Paradox",
        "8"  = "The Grasshopper Experiment",
        "9"  = "The Cooper-Hofstadter Polarization",
        "10" = "The Loobenfeld Decay",
        "11" = "The Pancake Batter Anomaly",
        "12" = "The Jerusalem Duality",
        "13" = "The Bat Jar Conjecture",
        "14" = "The Nerdvana Annihilation",
        "15" = "The Pork Chop Indeterminacy",
        "16" = "The Peanut Reaction",
        "17" = "The Tangerine Factor"
      ),
      # Temporadas 2 a 12: por ahora solo con número de episodio
      "2"  = setNames(paste("Episodio", 1:23), 1:23),
      "3"  = setNames(paste("Episodio", 1:23), 1:23),
      "4"  = setNames(paste("Episodio", 1:24), 1:24),
      "5"  = setNames(paste("Episodio", 1:24), 1:24),
      "6"  = setNames(paste("Episodio", 1:24), 1:24),
      "7"  = setNames(paste("Episodio", 1:24), 1:24),
      "8"  = setNames(paste("Episodio", 1:24), 1:24),
      "9"  = setNames(paste("Episodio", 1:24), 1:24),
      "10" = setNames(paste("Episodio", 1:24), 1:24),
      "11" = setNames(paste("Episodio", 1:24), 1:24),
      "12" = setNames(paste("Episodio", 1:24), 1:24)
    )
  ),
  # Series de prueba. Si falta la portada se muestra un degradado con "colores".
  # "temporadas" puede ser un vector de cantidades de episodios por temporada.
  "Breaking Bad"     = list(portada = "breaking_bad.jpg", anio = "2008-2013", duracion = 47, nota = 9.5, colores = c("#1f6e43", "#c8a23a"), temporadas = c(7, 13, 13, 13, 16)),
  "Dark"             = list(portada = "dark.jpg", anio = "2017-2020", duracion = 55, nota = 8.7, colores = c("#0e1a24", "#5a6b78"), temporadas = c(10, 8, 8)),
  "Stranger Things"  = list(portada = "stranger_things.jpg", anio = "2016-2025", duracion = 52, nota = 8.6, colores = c("#5c0d12", "#e1342a"), temporadas = c(8, 9, 8, 9, 8)),
  "The Office"       = list(portada = "the_office.jpg", anio = "2005-2013", duracion = 22, nota = 9.0, colores = c("#2b4a7a", "#a9c3e6"), temporadas = c(6, 22, 25, 19, 28, 26, 26, 24, 25)),
  "Chernobyl"        = list(portada = "chernobyl.jpg", anio = "2019",      duracion = 65, nota = 9.3, colores = c("#2f3a2a", "#b5b06a"), temporadas = c(5)),
  "La casa de papel" = list(portada = "la_casa_de_papel.jpg", anio = "2017-2021", duracion = 50, nota = 8.2, colores = c("#7a0f0f", "#e8472f"), temporadas = c(9, 6, 8, 8, 10)),
  "Friends"          = list(portada = "friends.jpg", anio = "1994-2004", duracion = 22, nota = 8.9, colores = c("#4b2a6b", "#e9b949"), temporadas = c(24, 24, 25, 24, 24, 25, 24, 24, 24, 18))
)

# Progreso de cada capítulo al abrir la app (valores de prueba).
# Clave "serie|temporada" -> proporción vista por episodio (1 = completo).
# Solo los capítulos completos cuentan en las barras y en el recap.
vistos_iniciales <- list("The Big Bang Theory|1" = c("1" = 1, "2" = 1, "3" = 1, "4" = 10 / 21))

# Formato mm:ss a partir de segundos
formato_tiempo <- function(seg) {
  seg <- round(seg)
  sprintf("%d:%02d", as.integer(seg %/% 60), as.integer(seg %% 60))
}

# Duración de los capítulos de una serie (minutos)
duracion_de <- function(serie) {
  d <- catalogo[[serie]]$duracion
  if (is.null(d)) 30 else d
}

# Devuelve las temporadas de una serie como lista: "1" = c("1" = "titulo", ...)
temporadas_de <- function(serie) {
  t <- catalogo[[serie]]$temporadas
  if (is.list(t)) return(t)
  out <- lapply(t, function(n) setNames(paste("Episodio", seq_len(n)), seq_len(n)))
  setNames(out, seq_along(t))
}

# -----------------------------------------------------------------------------
# CONECTAR AQUÍ (1): recap sin spoilers
# Debe devolver un texto (character). Puede tener varios párrafos separados
# por "\n\n".
# -----------------------------------------------------------------------------
generar_recap <- function(serie, temporada, episodio) {
  paste0(
    "[Texto de prueba] Aquí aparecerá el resumen de ", serie,
    " hasta la temporada ", temporada, ", episodio ", episodio,
    ". SpoilerGuard solo usará información de los episodios ya vistos.",
    "\n\n",
    "Cuando la función real esté lista, reemplazar el cuerpo de generar_recap() ",
    "en app.R y este texto desaparecerá."
  )
}

# -----------------------------------------------------------------------------
# CONECTAR AQUÍ (2): preguntas sobre personajes
# Debe devolver un texto (character).
# -----------------------------------------------------------------------------
responder_pregunta <- function(serie, temporada, episodio, pregunta) {
  paste0(
    "[Respuesta de prueba] La IA responderá a \"", pregunta, "\" usando solo ",
    "lo ocurrido en ", serie, " hasta T", temporada, ":E", episodio, "."
  )
}

# -----------------------------------------------------------------------------
# Ubicar la carpeta de portadas sin depender de la carpeta de trabajo
# -----------------------------------------------------------------------------
dir_portadas <- Filter(dir.exists, c(
  file.path(getwd(), "..", "data", "portadas"),   # app.R en cod/ (Run App)
  file.path(getwd(), "data", "portadas"),         # desde la raíz del proyecto
  file.path(getwd(), "www", "portadas")
))
if (length(dir_portadas) > 0) {
  addResourcePath("sg-portadas", normalizePath(dir_portadas[1]))
}

ruta_portada <- function(archivo) {
  if (is.null(archivo) || length(dir_portadas) == 0) return(NULL)
  if (!file.exists(file.path(dir_portadas[1], archivo))) return(NULL)
  paste0("sg-portadas/", utils::URLencode(archivo))
}

# Convierte un texto con párrafos en etiquetas <p>
texto_a_parrafos <- function(texto) {
  partes <- strsplit(texto, "\n\n", fixed = TRUE)[[1]]
  tagList(lapply(partes, tags$p))
}

# -----------------------------------------------------------------------------
# Estilos (van dentro del archivo para que siempre carguen)
# -----------------------------------------------------------------------------
estilos_css <- r"---(
/* Estética oscura con acento verde menta (inspirada en plataformas de cine) */
:root{
  --bg:#07070f;
  --surface:#11111c;
  --surface-2:#1a1a28;
  --line:#262636;
  --fg:#f4f4f8;
  --muted:#9a9ab0;
  --accent:#1ff2a4;
  --accent-ink:#04140d;
  --font-display:"Archivo","Arial Black",system-ui,sans-serif;
  --font-body:"Figtree",system-ui,-apple-system,"Segoe UI",sans-serif;
}
*{box-sizing:border-box}
html,html body{background:var(--bg);color:var(--fg);font-family:var(--font-body);font-size:15px;line-height:1.5;margin:0}
.container-fluid{padding:0}

/* Barra superior */
.sg-header{position:sticky;top:0;z-index:5;background:var(--bg);border-bottom:1px solid var(--line)}
.sg-bar{max-width:1180px;margin:0 auto;padding:18px 24px;display:flex;align-items:center;gap:28px;flex-wrap:wrap}
.sg-logo{font-family:var(--font-display);font-weight:900;font-stretch:72%;font-size:30px;line-height:1;text-transform:uppercase;color:var(--accent);letter-spacing:-.5px}
.sg-logo span{color:var(--fg)}
.sg-tag{color:var(--muted);font-weight:600;flex:1}

/* Contenido */
.sg-main{max-width:1180px;margin:0 auto;padding:36px 24px 64px;display:grid;grid-template-columns:300px 1fr;gap:36px}
.sg-eyebrow{font-size:12px;letter-spacing:.12em;text-transform:uppercase;color:var(--muted);font-weight:600}
.sg-main h1,.sg-main h2,.sg-main h3{font-family:var(--font-display);margin:0;text-wrap:balance;color:var(--fg)}
.sg-main h1{font-size:34px;font-weight:800}
.sg-main h2{font-size:20px;font-weight:800}

/* Panel lateral */
.sg-side{display:flex;flex-direction:column;gap:20px}
.sg-poster{width:100%;aspect-ratio:2/3;border-radius:6px;background:var(--surface) center/cover no-repeat;display:flex;align-items:flex-end;padding:14px;overflow:hidden}
.sg-poster-vacio{background:linear-gradient(160deg,#2a6f9e,#07070f)}
.sg-poster-vacio h3{font-weight:900;font-stretch:80%;font-size:26px;text-transform:uppercase;line-height:.95}
.sg-panel{background:var(--surface);border-radius:6px;padding:20px;display:flex;flex-direction:column;gap:4px}

/* Controles de Shiny */
.form-group{margin-bottom:14px}
.control-label{font-size:12px;letter-spacing:.1em;text-transform:uppercase;color:var(--muted);font-weight:600;margin-bottom:6px;display:block}
.sg-main select.form-control,.sg-main .form-control{width:100%;background:var(--bg);color:var(--fg);border:1.5px solid var(--line);border-radius:4px;padding:9px 10px;font:inherit;height:auto;box-shadow:none}
.sg-main select.form-control:focus,.sg-main .form-control:focus{border-color:var(--accent);outline:none;box-shadow:none;background:var(--bg);color:var(--fg)}
.sg-main textarea.form-control{resize:vertical;min-height:90px}
.btn.sg-btn{width:100%;border-radius:4px;padding:11px 14px;font-family:var(--font-display);font-weight:800;font-size:13px;letter-spacing:.06em;text-transform:uppercase;border:2px solid var(--accent);background:var(--accent);color:var(--accent-ink)}
.btn.sg-btn:hover,.btn.sg-btn:focus,.btn.sg-btn:active{filter:brightness(1.1);color:var(--accent-ink);background:var(--accent);border-color:var(--accent)}
.btn.sg-btn.sg-btn-outline{background:none;color:var(--fg);border-color:var(--fg);width:auto}
.btn.sg-btn.sg-btn-outline:hover,.btn.sg-btn.sg-btn-outline:focus{background:none;color:var(--fg);border-color:var(--accent)}
.sg-aviso{font-size:13px;color:var(--muted);margin:6px 0 0}

/* Columna principal */
.sg-content{display:flex;flex-direction:column;gap:28px;min-width:0}
.sg-head p{color:var(--muted);max-width:62ch;margin:8px 0 0}
.sg-limite{display:inline-flex;align-items:center;gap:8px;background:var(--surface-2);border:1px solid var(--line);border-radius:4px;padding:6px 10px;font-size:13px;font-weight:600;margin-top:12px}
.sg-limite i{width:8px;height:8px;border-radius:50%;background:var(--accent);display:inline-block}
.sg-card{background:var(--surface);border-radius:6px;padding:24px;display:flex;flex-direction:column;gap:14px}
.sg-card-head{display:flex;justify-content:space-between;align-items:baseline;gap:12px;flex-wrap:wrap}
.sg-recap{color:var(--fg);max-width:70ch}
.sg-recap p{margin:0 0 10px}
.sg-vacio{color:var(--muted);border:1.5px dashed var(--line);border-radius:6px;padding:28px;text-align:center}
.sg-pregunta-fila{display:grid;grid-template-columns:1fr auto;gap:12px;align-items:end}
.sg-pregunta-fila .form-group{margin:0}
.sg-respuesta{border-left:3px solid var(--accent);background:var(--surface-2);padding:14px 16px;border-radius:0 4px 4px 0}
.sg-respuesta .sg-q{color:var(--muted);font-size:13px;margin-bottom:6px}
.sg-chip{font-size:11px;font-weight:800;letter-spacing:.06em;text-transform:uppercase;padding:3px 7px;border-radius:3px;background:var(--line);color:var(--muted)}
.sg-chip.ok{background:var(--accent);color:var(--accent-ink)}

.sg-footer{border-top:1px solid var(--line);color:var(--muted);font-size:13px}
.sg-footer div{max-width:1180px;margin:0 auto;padding:22px 24px}

@media (max-width:820px){
  .sg-main{grid-template-columns:1fr}
  .sg-poster{max-width:220px}
  .sg-pregunta-fila{grid-template-columns:1fr}
  .btn.sg-btn.sg-btn-outline{width:100%}
}

:focus-visible{outline:2px solid var(--accent);outline-offset:2px}

/* Catálogo de series: tarjetas horizontales */
.sg-catalogo{max-width:1180px;margin:0 auto;padding:32px 24px 8px}
.sg-cat-head{display:flex;justify-content:space-between;align-items:center;gap:16px;flex-wrap:wrap;margin-bottom:22px}
.sg-cat-head h2{font-family:var(--font-display);font-size:22px;font-weight:800;margin:0;color:var(--fg)}
.sg-orden{border:1.5px solid var(--muted);border-radius:4px;padding:8px 14px;font-weight:600;color:var(--fg)}
.sg-orden span{color:var(--muted)}
.sg-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(250px,1fr));gap:16px}
.sg-tile{position:relative;aspect-ratio:16/9;border:0;padding:0;border-radius:4px;overflow:hidden;cursor:pointer;text-align:left;color:#fff;background:var(--surface) center 30%/cover no-repeat;transition:transform .2s, box-shadow .2s}
.sg-tile:hover{transform:translateY(-3px)}
.sg-tile.activa{box-shadow:0 0 0 3px var(--accent)}
.sg-tile::after{content:"";position:absolute;inset:0;background:linear-gradient(to top,rgba(0,0,0,.85) 0%,rgba(0,0,0,.35) 45%,rgba(0,0,0,0) 70%)}
.sg-tile-tipo{position:absolute;top:12px;left:14px;z-index:1;background:rgba(0,0,0,.85);color:#fff;font-size:12px;font-weight:800;letter-spacing:.04em;padding:3px 7px;border-radius:2px;text-transform:uppercase}
.sg-tile-info{position:absolute;left:16px;right:70px;bottom:12px;z-index:1;display:flex;flex-direction:column}
.sg-tile-info b{font-family:var(--font-display);font-size:18px;font-weight:800;line-height:1.15;text-shadow:0 1px 6px rgba(0,0,0,.5)}
.sg-tile-info span{font-size:14px;color:rgba(255,255,255,.75);font-weight:500}
.sg-tile-nota{position:absolute;right:14px;bottom:14px;z-index:1;background:rgba(70,70,80,.9);color:#fff;font-weight:800;font-size:14px;min-width:34px;text-align:center;padding:4px 6px;border-radius:2px;font-variant-numeric:tabular-nums}
#detalle{scroll-margin-top:80px}
.sg-main option{background:var(--bg);color:var(--fg)}

/* Barras de progreso */
.sg-prog{height:6px;background:var(--line);border-radius:3px;overflow:hidden}
.sg-prog i{display:block;height:100%;background:var(--accent);border-radius:3px;transition:width .3s}
.sg-prog-fila{display:flex;justify-content:space-between;gap:12px;font-size:13px;color:var(--muted);margin-bottom:6px;font-variant-numeric:tabular-nums}
.sg-prog-fila b{color:var(--fg);font-weight:700}
.sg-progreso{display:flex;flex-direction:column;gap:2px;max-width:520px;margin-top:16px}
.sg-tile .sg-tile-prog{position:absolute;left:0;right:0;bottom:0;height:4px;z-index:2;background:rgba(255,255,255,.18)}
.sg-tile .sg-tile-prog i{display:block;height:100%;background:var(--accent)}

/* Progreso de toda la serie (panel lateral) */
.sg-serie-prog{background:var(--surface);border-radius:6px;padding:20px;display:flex;flex-direction:column;gap:16px}
.sg-oculto{display:none}
.sg-barra{display:flex;flex-direction:column;gap:8px}
.sg-barra + .sg-barra{border-top:1px solid var(--line);padding-top:16px}
.sg-barra-fila{display:flex;justify-content:space-between;align-items:baseline;gap:10px}
.sg-barra-tit{font-size:12px;letter-spacing:.1em;text-transform:uppercase;color:var(--muted);font-weight:600}
.sg-barra-pct{font-family:var(--font-display);font-size:26px;font-weight:800;line-height:1;font-variant-numeric:tabular-nums}
.sg-barra-det{font-size:13px;color:var(--muted);font-variant-numeric:tabular-nums}
.sg-barra .sg-prog{height:8px;border-radius:4px}

/* Pestañas de temporada */
.sg-temps{display:flex;gap:8px;flex-wrap:wrap}
.sg-temp{background:none;border:1.5px solid var(--line);border-radius:4px;color:var(--muted);font:inherit;font-weight:600;font-size:13px;padding:6px 12px;cursor:pointer}
.sg-temp:hover{border-color:var(--muted);color:var(--fg)}
.sg-temp.activa{border-color:var(--accent);color:var(--fg)}

/* Barra horizontal de capítulos (cuadros que se llenan como un vaso) */
.sg-caps{background:var(--surface);border-radius:6px;padding:20px 24px;display:flex;flex-direction:column;gap:14px;min-width:0}
.sg-caps-head{display:flex;justify-content:space-between;align-items:baseline;gap:12px;flex-wrap:wrap}
.sg-caps-head h2{font-family:var(--font-display);font-size:20px;font-weight:800;margin:0}
.sg-caps-tools{display:flex;gap:16px;flex-wrap:wrap}
.sg-caps-tools .action-button{color:var(--accent);font-weight:600;font-size:13px;text-decoration:none;background:none;border:0;padding:0}
.sg-caps-tools .action-button:hover{text-decoration:underline}
.sg-eps{display:flex;gap:10px;overflow-x:auto;padding:4px 2px 12px;scrollbar-width:thin;scrollbar-color:var(--line) transparent}
.sg-ep{position:relative;overflow:hidden;flex:none;width:64px;height:64px;display:flex;flex-direction:column;align-items:center;justify-content:center;gap:2px;background:var(--bg);border:2px solid var(--line);border-radius:6px;color:var(--muted);cursor:pointer;font:inherit;padding:0;transition:border-color .15s,color .15s,transform .15s}
.sg-ep:hover{border-color:var(--muted);transform:translateY(-2px)}
.sg-ep > .sg-ep-sub,.sg-ep > .sg-ep-num{position:relative;z-index:2}
.sg-ep-num{font-family:var(--font-display);font-weight:800;font-size:20px;line-height:1;font-variant-numeric:tabular-nums}
.sg-ep-sub{font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase}
.sg-ep.completo{border-color:var(--accent);color:var(--fg)}
.sg-ep.completo .sg-ep-sub{color:var(--accent)}
.sg-ep.completo:hover{border-color:var(--accent)}
.sg-ep.parcial{color:var(--fg)}
.sg-ep.limite{box-shadow:0 0 0 3px rgba(31,242,164,.18)}

/* Líquido: dos olas que giran; su altura es la proporción vista (--p) */
.sg-ola{position:absolute;z-index:1;left:-50%;width:200%;height:200%;top:calc((100 - var(--p)) * 1% - 5px);border-radius:42%;background:rgba(31,242,164,.34);animation:sg-girar 6s linear infinite;pointer-events:none}
.sg-ola.b{border-radius:38%;background:rgba(31,242,164,.22);animation-duration:9s;animation-direction:reverse}
.sg-ep.completo .sg-ola{top:-6px}
.sg-ep.recien .sg-ola{animation:sg-girar 6s linear infinite, sg-llenar 1.8s cubic-bezier(.3,.7,.3,1)}
.sg-ep.recien .sg-ola.b{animation:sg-girar 9s linear infinite reverse, sg-llenar 2.2s cubic-bezier(.3,.7,.3,1)}
@keyframes sg-girar{to{transform:rotate(360deg)}}
@keyframes sg-llenar{from{top:100%}}
@media (prefers-reduced-motion:reduce){.sg-ola,.sg-ep.recien .sg-ola{animation:none}}

/* Deslizador de minutos */
.sg-tiempo{display:flex;flex-direction:column;gap:10px}
.sg-tiempo-fila{display:flex;justify-content:space-between;align-items:baseline;gap:12px}
.sg-tiempo-valor{font-family:var(--font-display);font-size:30px;font-weight:800;line-height:1;font-variant-numeric:tabular-nums}
.sg-tiempo-det{font-size:13px;color:var(--muted);font-variant-numeric:tabular-nums}
.sg-rango{-webkit-appearance:none;appearance:none;width:100%;height:8px;border-radius:4px;outline:none;cursor:pointer;
  background:linear-gradient(to right,var(--accent) 0 var(--r,50%),var(--line) var(--r,50%) 100%)}
.sg-rango::-webkit-slider-thumb{-webkit-appearance:none;appearance:none;width:22px;height:22px;border-radius:50%;background:var(--fg);border:4px solid var(--accent);box-shadow:0 2px 8px rgba(0,0,0,.5)}
.sg-rango::-moz-range-thumb{width:16px;height:16px;border-radius:50%;background:var(--fg);border:4px solid var(--accent)}
.sg-rango:focus-visible{outline:2px solid var(--accent);outline-offset:4px}
.sg-tiempo-marcas{display:flex;justify-content:space-between;font-size:11px;color:var(--muted);font-variant-numeric:tabular-nums}
.sg-pasos{display:flex;gap:8px;flex-wrap:wrap}
.sg-paso{background:none;border:1.5px solid var(--line);border-radius:4px;color:var(--fg);font:inherit;font-size:13px;font-weight:600;padding:4px 10px;cursor:pointer}
.sg-paso:hover{border-color:var(--accent)}

/* Ventana de "¿lo viste completo?" */
.modal-backdrop.in{opacity:.75}
.modal-content{background:var(--surface);color:var(--fg);border:1px solid var(--line);border-radius:8px;box-shadow:0 20px 60px rgba(0,0,0,.6);font-family:var(--font-body)}
.modal-header,.modal-footer{border-color:var(--line)}
.modal-title{font-family:var(--font-display);font-weight:800;font-size:20px}
.modal-footer{display:flex;justify-content:space-between;align-items:center;gap:12px}
.modal-footer .btn-default{background:none;color:var(--fg);border:1.5px solid var(--line);border-radius:4px}
.sg-modal-sub{color:var(--muted);margin:0 0 18px}
.sg-modal-op{display:flex;flex-direction:column;gap:12px}
.sg-modal-op + .sg-modal-op{border-top:1px solid var(--line);margin-top:18px;padding-top:18px}
.sg-modal-op h4{margin:0;font-size:15px;font-weight:700}
.sg-modal-fila{display:grid;grid-template-columns:1fr auto;gap:12px;align-items:end}
.sg-modal-fila .form-group{margin:0}
.modal .form-control{width:100%;background:var(--bg);color:var(--fg);border:1.5px solid var(--line);border-radius:4px;padding:9px 10px;font:inherit;height:auto;box-shadow:none}
.modal .form-control:focus{border-color:var(--accent);box-shadow:none}
.modal .control-label{font-size:12px;letter-spacing:.1em;text-transform:uppercase;color:var(--muted);font-weight:600}
.modal .action-button.sg-quitar{color:var(--muted);font-weight:600;font-size:13px}
.modal .action-button.sg-quitar:hover{color:var(--fg)}
.sg-caps-info{font-size:13px;color:var(--muted);margin:0}
.sg-caps-info b{color:var(--fg);font-weight:600}
.btn.sg-btn.sg-btn-auto{width:auto;align-self:flex-start;padding-inline:22px}

)---"

# -----------------------------------------------------------------------------
# Interfaz
# -----------------------------------------------------------------------------
ui <- fluidPage(
  title = "Storytrack",
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Archivo:wdth,wght@62..125,400..900&family=Figtree:wght@400;500;600;700&display=swap"
    ),
    tags$style(HTML(estilos_css)),
    # Deslizador de minutos: actualiza el texto al moverlo y avisa a Shiny
    tags$script(HTML("
      function sgFormato(s){ var m = Math.floor(s/60), r = Math.round(s%60); return m + ':' + (r<10?'0':'') + r; }
      function sgMoverTiempo(el, total){
        var s = +el.value, min = +el.min, max = +el.max;
        el.style.setProperty('--r', (100*(s-min)/(max-min)) + '%');
        document.getElementById('sg-tiempo-valor').textContent = sgFormato(s);
        document.getElementById('sg-tiempo-det').textContent = 'de ' + sgFormato(total) + ' \u00b7 ' + Math.round(100*s/total) + '%';
        Shiny.setInputValue('ep_segundos', {s: s, n: Math.random()}, {priority: 'event'});
      }
      function sgSumarTiempo(delta, total){
        var el = document.getElementById('sg-rango'); if(!el) return;
        el.value = Math.min(+el.max, Math.max(+el.min, +el.value + delta));
        sgMoverTiempo(el, total);
      }
    "))
  ),

  # Barra superior
  tags$header(
    class = "sg-header",
    div(
      class = "sg-bar",
      div(class = "sg-logo", "STORY", tags$span("TRACK")),
      div(class = "sg-tag", "Recaps y preguntas sin spoilers")
    )
  ),

  # ---- Catálogo de series ----
  tags$section(
    class = "sg-catalogo",
    div(
      class = "sg-cat-head",
      h2(paste(length(catalogo), "series"))
    ),
    uiOutput("catalogo")
  ),

  div(
    class = "sg-main", id = "detalle",

    # ---- Panel lateral: portada, selectores y capítulos ----
    tags$aside(
      class = "sg-side",
      uiOutput("portada"),
      uiOutput("progreso_serie"),
      # Selectores ocultos: la serie se elige en el catálogo y la temporada
      # con las pestañas. Se mantienen para que input$serie e input$temporada
      # sigan disponibles al conectar SpoilerGuard.
      div(
        class = "sg-oculto",
        selectInput("serie", "Serie", choices = names(catalogo), selectize = FALSE),
        selectInput("temporada", "Temporada", choices = NULL, selectize = FALSE)
      )
    ),

    # ---- Columna principal: progreso, recap y preguntas ----
    div(
      class = "sg-content",
      div(
        class = "sg-head",
        span(class = "sg-eyebrow", textOutput("eyebrow", inline = TRUE)),
        h1(textOutput("titulo_serie", inline = TRUE)),
        uiOutput("limite")
      ),

      # Capítulos vistos: barra horizontal de cuadros
      div(
        class = "sg-caps",
        div(
          class = "sg-caps-head",
          h2("Capítulos vistos"),
          div(
            class = "sg-caps-tools",
            actionLink("marcar_todos", "Marcar temporada"),
            actionLink("desmarcar_todos", "Desmarcar todo")
          )
        ),
        uiOutput("temporadas_tabs"),
        uiOutput("capitulos"),
        uiOutput("capitulos_info")
      ),

      # Recap
      div(
        class = "sg-card",
        div(
          class = "sg-card-head",
          h2("Recap"),
          uiOutput("estado_recap", inline = TRUE)
        ),
        actionButton("generar", "Generar recap", class = "sg-btn sg-btn-auto"),
        uiOutput("recap")
      ),

      # Preguntas sobre personajes
      div(
        class = "sg-card",
        div(class = "sg-card-head", h2("Pregunta sobre los personajes")),
        div(
          class = "sg-pregunta-fila",
          textAreaInput(
            "pregunta", NULL,
            placeholder = "Ej.: ¿Qué relación tienen Leonard y Penny hasta ahora?",
            width = "100%"
          ),
          actionButton("preguntar", "Preguntar", class = "sg-btn sg-btn-outline")
        ),
        uiOutput("respuesta")
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Servidor
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Capítulos vistos ----
  vistos <- reactiveVal(vistos_iniciales)
  clave <- function(serie, temporada) paste(serie, temporada, sep = "|")

  # Proporción vista de cada capítulo de una temporada (vector con nombres)
  eps_progreso <- function(serie, temporada) {
    v <- vistos()[[clave(serie, temporada)]]
    if (is.null(v)) numeric(0) else v
  }

  # Solo los capítulos vistos completos
  eps_vistos <- function(serie, temporada) {
    v <- eps_progreso(serie, temporada)
    names(v)[v >= 1]
  }

  guardar_progreso <- function(serie, temporada, prog) {
    v <- vistos()
    v[[clave(serie, temporada)]] <- prog
    vistos(v)
  }

  # Cambia el progreso de un capítulo (0 = quitar)
  fijar_capitulo <- function(ep, proporcion) {
    prog <- eps_progreso(input$serie, input$temporada)
    if (proporcion <= 0) prog <- prog[names(prog) != ep] else prog[[ep]] <- min(proporcion, 1)
    guardar_progreso(input$serie, input$temporada, prog)
    recien(ep)
  }

  # Capítulo recién cambiado: solo ese se anima al llenarse
  recien <- reactiveVal(NULL)
  ep_abierto <- reactiveVal(NULL)
  seg_elegidos <- reactiveVal(NULL)   # segundos marcados en el deslizador

  observeEvent(input$ep_segundos, seg_elegidos(input$ep_segundos$s))

  # Conteo de vistos / total para una serie
  progreso_de <- function(serie) {
    temps <- temporadas_de(serie)
    total <- sum(lengths(temps))
    vist <- sum(vapply(names(temps), function(t) {
      length(intersect(eps_vistos(serie, t), names(temps[[t]])))
    }, numeric(1)))
    c(vistos = vist, total = total)
  }

  # Último episodio visto (en orden): es el límite del recap
  limite <- reactive({
    req(input$serie)
    temps <- temporadas_de(input$serie)
    res <- NULL
    for (t in names(temps)) {
      v <- eps_vistos(input$serie, t)
      if (length(v) > 0) {
        eps <- names(temps[[t]])
        res <- list(temporada = t, episodio = eps[max(match(v, eps), na.rm = TRUE)])
      }
    }
    res
  })

  # Al tocar un capítulo se pregunta si se vio completo o hasta qué minuto
  observeEvent(input$toggle_ep, {
    req(input$temporada)
    ep <- as.character(input$toggle_ep$ep)
    ep_abierto(ep)
    titulo <- temporadas_de(input$serie)[[input$temporada]][[ep]]
    dur <- duracion_de(input$serie)
    actual <- eps_progreso(input$serie, input$temporada)[ep]
    total_seg <- dur * 60
    paso <- 15                                   # el deslizador avanza de 15 en 15 segundos
    inicial <- if (!is.na(actual) && actual < 1) round(actual * total_seg / paso) * paso
               else round(total_seg / 2 / paso) * paso
    seg_elegidos(inicial)

    showModal(modalDialog(
      title = paste0("T", input$temporada, ":E", ep, " · ", titulo),
      easyClose = TRUE,
      tags$p(class = "sg-modal-sub", paste("Duración aproximada:", dur, "min")),
      div(class = "sg-modal-op",
          tags$h4("¿Viste el capítulo completo?"),
          actionButton("ep_completo", "Sí, lo vi completo", class = "sg-btn")),
      div(class = "sg-modal-op",
          tags$h4("No, lo dejé a medias"),
          div(
            class = "sg-tiempo",
            div(class = "sg-tiempo-fila",
                span(class = "sg-tiempo-valor", id = "sg-tiempo-valor", formato_tiempo(inicial)),
                span(class = "sg-tiempo-det", id = "sg-tiempo-det",
                     paste0("de ", formato_tiempo(total_seg), " · ", round(100 * inicial / total_seg), "%"))),
            tags$input(
              type = "range", class = "sg-rango", id = "sg-rango",
              min = paso, max = total_seg - paso, step = paso, value = inicial,
              `aria-label` = "Minuto donde quedé",
              style = sprintf("--r:%s%%", round(100 * (inicial - paso) / (total_seg - 2 * paso), 1)),
              oninput = sprintf("sgMoverTiempo(this, %d);", total_seg)
            ),
            div(class = "sg-tiempo-marcas",
                span(formato_tiempo(paso)),
                span(formato_tiempo(total_seg / 2)),
                span(formato_tiempo(total_seg - paso))),
            # Botones para ajustar fino
            div(class = "sg-pasos",
                tags$button(type = "button", class = "sg-paso", onclick = sprintf("sgSumarTiempo(-60, %d);", total_seg), "−1 min"),
                tags$button(type = "button", class = "sg-paso", onclick = sprintf("sgSumarTiempo(-15, %d);", total_seg), "−15 s"),
                tags$button(type = "button", class = "sg-paso", onclick = sprintf("sgSumarTiempo(15, %d);", total_seg), "+15 s"),
                tags$button(type = "button", class = "sg-paso", onclick = sprintf("sgSumarTiempo(60, %d);", total_seg), "+1 min")),
            actionButton("ep_parcial", "Guardar minuto", class = "sg-btn sg-btn-outline")
          )),
      footer = tagList(
        actionLink("ep_quitar", "Marcar como no visto", class = "sg-quitar"),
        modalButton("Cancelar")
      )
    ))
  })

  observeEvent(input$ep_completo, {
    req(ep_abierto())
    fijar_capitulo(ep_abierto(), 1)
    removeModal()
  })

  observeEvent(input$ep_parcial, {
    req(ep_abierto())
    total_seg <- duracion_de(input$serie) * 60
    s <- seg_elegidos()
    req(s)
    if (s >= total_seg) {
      fijar_capitulo(ep_abierto(), 1)   # llegó al final: cuenta como completo
    } else {
      fijar_capitulo(ep_abierto(), s / total_seg)
    }
    removeModal()
  })

  observeEvent(input$ep_quitar, {
    req(ep_abierto())
    fijar_capitulo(ep_abierto(), 0)
    removeModal()
  })

  observeEvent(input$marcar_todos, {
    req(input$temporada)
    eps <- names(temporadas_de(input$serie)[[input$temporada]])
    guardar_progreso(input$serie, input$temporada, setNames(rep(1, length(eps)), eps))
    recien("todos")
  })

  observeEvent(input$desmarcar_todos, {
    req(input$temporada)
    guardar_progreso(input$serie, input$temporada, numeric(0))
    recien(NULL)
  })

  # ---- Selectores ----
  observeEvent(input$temporada, recien(NULL), priority = 10)

  observeEvent(input$serie, {
    temps <- names(temporadas_de(input$serie))
    updateSelectInput(session, "temporada",
                      choices = setNames(temps, paste("Temporada", temps)),
                      selected = temps[1])
  })

  # ---- Catálogo ----
  output$catalogo <- renderUI({
    seleccion <- input$serie
    nombres <- sort(names(catalogo))
    div(class = "sg-grid", lapply(nombres, function(nombre) {
      s <- catalogo[[nombre]]
      p <- ruta_portada(s$portada)
      fondo <- if (!is.null(p)) sprintf("background-image:url('%s')", p) else {
        col <- if (is.null(s$colores)) c("#2a2a3a", "#55556a") else s$colores
        sprintf("background-image:linear-gradient(135deg,%s,%s)", col[2], col[1])
      }
      n_temp <- length(temporadas_de(nombre))
      prog <- progreso_de(nombre)
      pct <- round(100 * prog[["vistos"]] / prog[["total"]])
      tags$button(
        class = paste("sg-tile", if (identical(nombre, seleccion)) "activa"),
        style = fondo,
        `aria-label` = paste0("Abrir ", nombre, ", ", pct, "% visto"),
        onclick = sprintf(
          "Shiny.setInputValue('serie_tarjeta', %s, {priority: 'event'}); document.getElementById('detalle').scrollIntoView({behavior: 'smooth'});",
          jsonlite::toJSON(nombre, auto_unbox = TRUE)
        ),
        span(class = "sg-tile-tipo", "Serie"),
        div(class = "sg-tile-info",
            tags$b(nombre),
            span(paste(n_temp, if (n_temp == 1) "temporada" else "temporadas"))),
        if (!is.null(s$nota)) span(class = "sg-tile-nota", format(s$nota, nsmall = 1)),
        if (pct > 0) div(class = "sg-tile-prog", tags$i(style = sprintf("width:%s%%", pct)))
      )
    }))
  })

  observeEvent(input$serie_tarjeta, {
    updateSelectInput(session, "serie", selected = input$serie_tarjeta)
  })

  # ---- Lista de capítulos con casillas ----
  output$capitulos <- renderUI({
    req(input$temporada)
    eps <- temporadas_de(input$serie)[[input$temporada]]
    req(eps)
    prog <- eps_progreso(input$serie, input$temporada)
    dur <- duracion_de(input$serie)
    lim <- limite()
    animar <- isolate(recien())
    div(class = "sg-eps", role = "group", `aria-label` = "Capítulos de la temporada",
      lapply(seq_along(eps), function(i) {
        n <- names(eps)[i]
        frac <- if (n %in% names(prog)) prog[[n]] else 0
        completo <- frac >= 1
        parcial <- frac > 0 && !completo
        es_limite <- !is.null(lim) && lim$temporada == input$temporada && lim$episodio == n
        nuevo <- identical(animar, n) || (identical(animar, "todos") && completo)
        estado <- if (completo) "visto completo"
                  else if (parcial) paste0("visto hasta ", formato_tiempo(frac * dur * 60))
                  else "sin ver"
        tags$button(
          type = "button",
          class = paste("sg-ep",
                        if (completo) "completo",
                        if (parcial) "parcial",
                        if (es_limite) "limite",
                        if (nuevo) "recien"),
          style = sprintf("--p:%s", round(100 * frac)),
          role = "checkbox",
          `aria-checked` = if (completo) "true" else if (parcial) "mixed" else "false",
          title = paste0("E", n, " - ", eps[[i]], " (", estado, ")"),
          onclick = sprintf(
            "Shiny.setInputValue('toggle_ep', {ep: '%s', n: Math.random()}, {priority: 'event'});", n
          ),
          if (frac > 0) tagList(span(class = "sg-ola"), span(class = "sg-ola b")),
          span(class = "sg-ep-sub", if (parcial) formato_tiempo(frac * dur * 60) else "Cap"),
          span(class = "sg-ep-num", n)
        )
      }))
  })

  # Texto bajo la barra: último capítulo visto
  output$capitulos_info <- renderUI({
    lim <- limite()
    if (is.null(lim)) {
      tags$p(class = "sg-caps-info", "Toca un cuadro para marcar el capítulo como visto.")
    } else {
      titulo <- temporadas_de(input$serie)[[lim$temporada]][[lim$episodio]]
      tags$p(class = "sg-caps-info", "Último visto: ",
             tags$b(paste0("T", lim$temporada, ":E", lim$episodio, " · ", titulo)))
    }
  })

  # ---- Progreso ----
  # Dos barras: la temporada elegida y toda la serie
  output$progreso_serie <- renderUI({
    req(input$serie, input$temporada)
    eps_t <- names(temporadas_de(input$serie)[[input$temporada]])
    req(eps_t)
    vistos_t <- length(intersect(eps_vistos(input$serie, input$temporada), eps_t))
    p <- progreso_de(input$serie)

    barra <- function(titulo, vistos, total) {
      pct <- if (total > 0) round(100 * vistos / total) else 0
      div(class = "sg-barra",
          div(class = "sg-barra-fila",
              span(class = "sg-barra-tit", titulo),
              span(class = "sg-barra-pct", paste0(pct, "%"))),
          div(class = "sg-prog", role = "progressbar",
              `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100,
              `aria-label` = titulo,
              tags$i(style = sprintf("width:%s%%", pct))),
          span(class = "sg-barra-det", paste(vistos, "de", total, "capítulos")))
    }

    div(class = "sg-serie-prog",
        barra(paste("Temporada", input$temporada), vistos_t, length(eps_t)),
        barra("Toda la serie", p[["vistos"]], p[["total"]]))
  })

  # Pestañas de temporada sobre la barra de capítulos
  output$temporadas_tabs <- renderUI({
    req(input$temporada)
    temps <- names(temporadas_de(input$serie))
    div(class = "sg-temps", lapply(temps, function(t) {
      tags$button(
        type = "button",
        class = paste("sg-temp", if (t == input$temporada) "activa"),
        onclick = sprintf("Shiny.setInputValue('temporada_tab', '%s', {priority: 'event'});", t),
        paste("Temporada", t)
      )
    }))
  })

  observeEvent(input$temporada_tab, {
    updateSelectInput(session, "temporada", selected = input$temporada_tab)
  })

  # ---- Encabezado ----
  output$eyebrow <- renderText(paste("Serie ·", catalogo[[input$serie]]$anio))
  output$titulo_serie <- renderText(input$serie)
  output$limite <- renderUI({
    lim <- limite()
    texto <- if (is.null(lim)) "Marca los capítulos que ya viste"
             else paste0("Sin spoilers después de T", lim$temporada, ":E", lim$episodio)
    div(class = "sg-limite", tags$i(), texto)
  })

  output$portada <- renderUI({
    p <- ruta_portada(catalogo[[input$serie]]$portada)
    if (!is.null(p)) {
      div(class = "sg-poster", role = "img",
          `aria-label` = paste("Portada de", input$serie),
          style = sprintf("background-image:url('%s')", p))
    } else {
      div(class = "sg-poster sg-poster-vacio", h3(input$serie))
    }
  })

  # ---- Recap: se genera solo al pulsar el botón ----
  recap <- reactiveVal(NULL)
  observeEvent(input$generar, {
    lim <- limite()
    if (is.null(lim)) {
      recap(list(texto = NULL, hasta = NULL))
      return()
    }
    recap(list(
      texto = generar_recap(input$serie, lim$temporada, lim$episodio),
      hasta = paste0("T", lim$temporada, ":E", lim$episodio)
    ))
  })
  # Si cambia la serie o los capítulos vistos, el recap anterior deja de ser válido
  observeEvent(list(input$serie, limite()), recap(NULL), ignoreInit = TRUE)

  output$estado_recap <- renderUI({
    r <- recap()
    if (is.null(r) || is.null(r$hasta)) span(class = "sg-chip", "Sin generar")
    else span(class = "sg-chip ok", paste("Hasta", r$hasta))
  })

  output$recap <- renderUI({
    r <- recap()
    if (is.null(r)) {
      div(class = "sg-vacio",
          "Marca los capítulos que ya viste y pulsa “Generar recap”.")
    } else if (is.null(r$texto)) {
      div(class = "sg-vacio", "Marca al menos un capítulo como visto para generar el recap.")
    } else {
      div(class = "sg-recap", texto_a_parrafos(r$texto))
    }
  })

  # ---- Preguntas ----
  respuesta <- reactiveVal(NULL)
  observeEvent(input$preguntar, {
    q <- trimws(input$pregunta)
    lim <- limite()
    if (q == "") {
      respuesta(list(q = NULL, texto = "Escribe una pregunta antes de pulsar “Preguntar”."))
      return()
    }
    if (is.null(lim)) {
      respuesta(list(q = q, texto = "Marca al menos un capítulo como visto para poder responder sin spoilers."))
      return()
    }
    respuesta(list(
      q = q,
      texto = responder_pregunta(input$serie, lim$temporada, lim$episodio, q)
    ))
  })

  output$respuesta <- renderUI({
    r <- respuesta()
    if (is.null(r)) {
      div(class = "sg-vacio", "La respuesta aparecerá aquí.")
    } else {
      div(class = "sg-respuesta",
          if (!is.null(r$q)) div(class = "sg-q", paste("Pregunta:", r$q)),
          div(r$texto))
    }
  })
}

shinyApp(ui, server)
