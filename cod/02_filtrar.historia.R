filtrar.historia <- function(
    datos,
    serie.seleccionada,
    temporada.seleccionada,
    episodio.seleccionado,
    tiempo.seleccionado
) {
  temporada.seleccionada <- as.integer(temporada.seleccionada)
  episodio.seleccionado <- as.integer(episodio.seleccionado)
  
  convertir.segundos <- function(tiempo) {
    
    partes <- strsplit(tiempo, "[:,]")
    
    sapply(partes, function(x) {
      
      # Si viene de un subtítulo: 00:10:25,300
      if (length(x) == 4) {
        
        horas <- as.numeric(x[1])
        minutos <- as.numeric(x[2])
        segundos <- as.numeric(x[3])
        milisegundos <- as.numeric(x[4])
        
        horas * 3600 +
          minutos * 60 +
          segundos +
          milisegundos / 1000
        
        # Si viene de Shiny: 10:00
      } else {
        
        minutos <- as.numeric(x[1])
        segundos <- as.numeric(x[2])
        
        minutos * 60 + segundos
      }
    })
  }
  
  datos$fin.segundos <- convertir.segundos(datos$fin)
  
  limite.segundos <- convertir.segundos(tiempo.seleccionado)
  
  datos.filtrados <- datos[
    datos$serie == serie.seleccionada &
      (
        datos$temporada < temporada.seleccionada |
          
          (
            datos$temporada == temporada.seleccionada &
              datos$episodio < episodio.seleccionado
          ) |
          
          (
            datos$temporada == temporada.seleccionada &
              datos$episodio == episodio.seleccionado &
              datos$fin.segundos <= limite.segundos
          )
      ),
  ]
  
  datos.filtrados$fin.segundos <- NULL
  
  rownames(datos.filtrados) <- NULL
  
  datos.filtrados
}
