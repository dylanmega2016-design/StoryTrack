leer.subtitulos <- function(ruta.archivo) {
  lineas <- readLines (ruta.archivo, encoding = "UTF-8")
  separadores <- which(lineas== "")
  bordes <- c(0, separadores, length(lineas)+1) 
  subtitulos <- data.frame() #Se crea subtitulos como un data.frame vacio
  nombre.archivo <- basename(ruta.archivo)
  temporada <- regmatches(nombre.archivo, regexpr("S[0-9]{2}", nombre.archivo))
  episodio <- regmatches(nombre.archivo, regexpr("E[0-9]{2}", nombre.archivo))
  temporada <- as.integer(substr(temporada, 2, 3)) #Se extrae el dato númerico
  episodio <- as.integer(substr(episodio, 2, 3)) #Igual que el anterior
  serie <- sub(" S[0-9]{2}E[0-9]{2}.*", "", nombre.archivo)
  for (i in 1: (length(bordes)-1)){ #Se encarga de recorrer bloque por bloque extrayendo las variables requeridas
    bloque.actual <- lineas[(bordes[i]+1):(bordes[i+1]-1)] #Todos los bloques mantienen el mismo formato
    numero.bloque <- as.integer(bloque.actual[1])
    tiempo <- bloque.actual[2]
    tiempos <- strsplit(tiempo, " --> ")[[1]]
    inicio.sub <- tiempos[1]
    fin.sub <- tiempos[2]
    lineas.dialogo <- bloque.actual[3:length(bloque.actual)]
    personaje.actual <- NA_character_
    texto.actual <- ""
    for (linea in lineas.dialogo) {
      nuevo.personaje <- grepl(
        "^[[:space:]]*-?[[:space:]]*[^:]+:[[:space:]]*",
        linea
      )
      
      if (nuevo.personaje) {
        
        if (nzchar(trimws(texto.actual))) {
          
          fila.sub <- data.frame(
            serie = serie,
            temporada = temporada,
            episodio = episodio,
            bloque = numero.bloque,
            inicio = inicio.sub,
            fin = fin.sub,
            personaje = personaje.actual,
            texto = trimws(texto.actual),
            archivo = nombre.archivo
          )
          
          subtitulos <- rbind(subtitulos, fila.sub)
        }
        
        personaje.actual <- sub(
          "^[[:space:]]*-?[[:space:]]*([^:]+):.*$",
          "\\1",
          linea
        )
        
        personaje.actual <- trimws(personaje.actual)
        
        texto.actual <- sub(
          "^[[:space:]]*-?[[:space:]]*[^:]+:[[:space:]]*",
          "",
          linea
        )
        
      } else {
        
        texto.actual <- paste(
          texto.actual,
          linea
        )
      }
    }
    if (nzchar(trimws(texto.actual))) {
      
      fila.sub <- data.frame(
        serie = serie,
        temporada = temporada,
        episodio = episodio,
        bloque = numero.bloque,
        inicio = inicio.sub,
        fin = fin.sub,
        personaje = personaje.actual,
        texto = trimws(texto.actual),
        archivo = nombre.archivo
      )
      
      subtitulos <- rbind(subtitulos, fila.sub)
    }
  }  
  subtitulos
}
#primer.bloque <- lineas[(bordes[1]+1):(bordes[2]-1)]
#numero.bloque <- as.integer(primer.bloque[1])
#tiempo <- primer.bloque[2]
#tiempos <- strsplit(tiempo," --> ")[[1]]
#inicio.sub <- tiempos[1]
#fin.sub <- tiempos[2]
#texto.sub <- paste(primer.bloque[3:length(primer.bloque)], collapse = " ")
#fila.sub <- data.frame(bloque =numero.bloque, inicio = inicio.sub, fin = fin.sub, texto= texto.sub)
