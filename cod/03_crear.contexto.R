crear.contexto <- function(datos.filtrados) {
  
  lineas.contexto <- paste0(
    "[T",
    datos.filtrados$temporada,
    "E",
    datos.filtrados$episodio,
    " | ",
    datos.filtrados$inicio,
    " - ",
    datos.filtrados$fin,
    "] ",
    ifelse(
      is.na(datos.filtrados$personaje),
      "",
      paste0(datos.filtrados$personaje, ": ")
    ),
    datos.filtrados$texto
  )
  
  contexto <- paste(
    lineas.contexto,
    collapse = "\n"
  )
  
  contexto
}
