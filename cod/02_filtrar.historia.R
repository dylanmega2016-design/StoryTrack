filtrar.historia <- function(
    datos,
    serie.seleccionada,
    temporada.seleccionada,
    episodio.seleccionado
) {
  
  datos.filtrados <- datos[
    datos$serie == serie.seleccionada &
      (
        datos$temporada < temporada.seleccionada |
          (
            datos$temporada == temporada.seleccionada &
              datos$episodio <= episodio.seleccionado
          )
      ),
  ]
  
  rownames(datos.filtrados) <- NULL
  
  datos.filtrados
}
