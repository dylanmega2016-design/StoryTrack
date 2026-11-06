# StoryTrack
# Proyecto grupal - Herramientas de Ciencia de Datos I
# Explicacion General

## Objetivo



## Estructura del proyecto

- cod/: scripts de R e informe Quarto.
- data/: bases de datos utilizadas.
- info/: documentación y referencias.
- config/: parámetros y paquetes a utilizar.
- res/: gráficos, tablas e informe final.

## Cómo ejecutar el proyecto

1. Abrir el proyecto en RStudio.
2. Ejecutar los scripts en el orden indicado.
3. Generar el documento informe_final.qmd.
4. Consultar el PDF en res/.

## Conectar ChatGPT en StoryTrack (uso local)

StoryTrack puede solicitar autorización para usar el plan de ChatGPT en esta
instalación local. No hace falta crear una clave de API. La persona que use la
app debe iniciar sesión y conceder el permiso desde el navegador.

1. En la consola de RStudio, instalar las dependencias una vez:

   ```r
   install.packages(c("httr2", "openssl", "curl"))
   ```

2. Reiniciar RStudio, abrir `StoryTrack.Rproj` y ejecutar `cod/app.R`.
3. Pulsar **Conectar con ChatGPT** en la app.
4. En el navegador, iniciar sesión y autorizar el uso del plan para StoryTrack.
5. Volver a Shiny y generar el recap o hacer una pregunta.

La autorización y los tokens se guardan en la carpeta local de datos de la
cuenta de Windows, fuera de este repositorio. No se deben copiar al código ni
subir a GitHub. Esta integración está pensada para ejecutar el proyecto
localmente; publicarlo en un servidor remoto requiere revisar las condiciones
de acceso correspondientes.
