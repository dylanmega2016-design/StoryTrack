leer.subtitulos <- function(ruta.archivo) {
lineas <- readLines (ruta.archivo, encoding = "UTF-8")
separadores <- which(lineas== "")
bordes <- c(0, separadores, length(lineas)+1)
primer.bloque <- lineas[(bordes[1]+1):(bordes[2]-1)]
numero.bloque <- as.integer(primer.bloque[1])
tiempo <- primer.bloque[2]
tiempos <- strsplit(tiempo," --> ")[[1]]
inicio.sub <- tiempos[1]
fin.sub <- tiempos[2]
texto.sub <- paste(primer.bloque[3:length(primer.bloque)], collapse = " ")
fila.sub <- data.frame(bloque =numero.bloque, inicio = inicio.sub, fin = fin.sub, texto= texto.sub)
}