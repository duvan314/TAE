library(shiny)
library(tidyr)
library(rlang)
library(dplyr)
library(caret)
source("Predicciones.R")

#ui <-                                                     


#server <- function(input, output) 

# Funcionamiento de la app
#shinyApp(ui = ui, server = server)

if (FALSE) {
  # Start app in the current working directory
  runApp()
  
  # Start app in a subdirectory called myapp
  runApp("myapp")
}

## Only run this example in interactive R sessions
if (interactive()) {
  options(device.ask.default = FALSE)
  
  # Apps can be run without a server.r and ui.r file
  runApp(list(
    ui = fluidPage(title = "Accidentalidad en Medellín",
                   # Algunos cambios en el css de algunos elementos
                   tags$head(
                     tags$style(type="text/css", "body {padding-top: 70px;}"),
                     tags$style(type="text/css","#imagen img {max-width: 100%; width: 100%; height: auto; max-height: 100%}"),
                     tags$style("#texto_hijos{
                                 font-size: 20px;
                                 font-style: bold;
                                 text-align: center;
                                 }"),
                     tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                   ),
                   # Navegacion
                   navbarPage("Accidentalidad en Medellín", inverse = TRUE, position = "fixed-top",
                              
                              # Tab Modelo
                              tabPanel("Visualización",
                                       fluidRow(
                                         column(8,
                                                # Descripcion
                                                wellPanel(
                                                  fluidRow(
                                                    column(12,
                                                           tags$p("La siguiente es una aplicación web de un modelo de predicción de la ocurrencia de incidentes viales en la ciudad de Medellín, 
                                                        con base en los datos publicados por la Alcaldía de Medellín en el portal MeData."),
                                                           tags$h4("Modo de uso:"),
                                                           tags$p("Para visualizar los datos, complete los campos con la temporalidad y el tipo de accidente. Luego presiona el botón: Enviar datos")
                                                    )
                                                  ),
                                                ),
                                         ),
                                       ),
                              ),
                              # Espacio de enlaces 
                              tabPanel("Predicción",
                                       tags$h4("Enlace al reporte técnico"),
                                       tags$a(href="https://rpubs.com/Alexitouno19/TAE-01-NatalidadColombia", icon("book"), "Reporte técnico", class = "btn btn-primary"),
                                       tags$h4("Enlace al video promocional"),
                                       tags$a(href="https://youtu.be/h2PU-UY6-TY", icon("youtube"), "Video promocional", class = "btn btn-danger"),
                                       tags$h4("Enlace al respositorio del proyecto"),
                                       tags$a(href="https://github.com/juanescendales/TAE-01-NatalidadColombia", icon("github"), "Repositorio del proyecto", class = "btn", style = "background-color:#000000; color:#ffffff;"),
                                       hr(),
                                       # Referencia a los iconos usados con los hijos
                                       HTML('<div>Iconos diseñados por <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.es/" title="Flaticon">www.flaticon.es</a></div>')
                              ),
                              
                              tabPanel("Mapa",
                                       tags$h4("Enlace al reporte técnico"),
                                       tags$a(href="https://rpubs.com/Alexitouno19/TAE-01-NatalidadColombia", icon("book"), "Reporte técnico", class = "btn btn-primary"),
                                       tags$h4("Enlace al video promocional"),
                                       tags$a(href="https://youtu.be/h2PU-UY6-TY", icon("youtube"), "Video promocional", class = "btn btn-danger"),
                                       tags$h4("Enlace al respositorio del proyecto"),
                                       tags$a(href="https://github.com/juanescendales/TAE-01-NatalidadColombia", icon("github"), "Repositorio del proyecto", class = "btn", style = "background-color:#000000; color:#ffffff;"),
                                       hr(),
                                       # Referencia a los iconos usados con los hijos
                                       HTML('<div>Iconos diseñados por <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.es/" title="Flaticon">www.flaticon.es</a></div>')
                              )
                   ),
    ),
      #bootstrapPage(
      #numericInput('n', 'Number of obs', 100),
      #plotOutput('plot')
    #),
    server = function(input, output) {
      output$plot <- renderPlot({ hist(runif(input$n)) })
    }
  ))
  
  
  # Running a Shiny app object
  app <- shinyApp(
    ui = fluidPage(title = "Accidentalidad en Medellín",
                   # Algunos cambios en el css de algunos elementos
                   tags$head(
                     tags$style(type="text/css", "body {padding-top: 70px;}"),
                     tags$style(type="text/css","#imagen img {max-width: 100%; width: 100%; height: auto; max-height: 100%}"),
                     tags$style("#texto_hijos{
                                 font-size: 20px;
                                 font-style: bold;
                                 text-align: center;
                                 }"),
                     tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                   ),
                   # Navegacion
                   navbarPage("Predicción número de hijos", inverse = TRUE, position = "fixed-top",
                              
                              # Tab Modelo
                              tabPanel("Visualización",
                                       fluidRow(
                                         column(8,
                                                # Descripcion
                                                wellPanel(
                                                  fluidRow(
                                                    column(12,
                                                           tags$p("La siguiente es una aplicación web de un modelo de predicción del numero de hijos en un hogar colombiano, 
                                                        usando la más reciente encuesta de calidad de vida del DANE (ECV 2019)."),
                                                           tags$h4("Modo de uso:"),
                                                           tags$p("Para usarla debe rellenar los campos o caracteristicas que se observan a continuación y luego presionar el botón de enviar datos"),
                                                           tags$p("Las opciones deben ser rellenadas con la información del hogar y del Jefe del hogar.")
                                                    )
                                                  ),
                                                ),
                                         ),
                                       ),
                              ),
                              # Espacio de enlaces 
                              tabPanel("Predicción",
                                       tags$h4("Enlace al reporte técnico"),
                                       tags$a(href="https://rpubs.com/Alexitouno19/TAE-01-NatalidadColombia", icon("book"), "Reporte técnico", class = "btn btn-primary"),
                                       tags$h4("Enlace al video promocional"),
                                       tags$a(href="https://youtu.be/h2PU-UY6-TY", icon("youtube"), "Video promocional", class = "btn btn-danger"),
                                       tags$h4("Enlace al respositorio del proyecto"),
                                       tags$a(href="https://github.com/juanescendales/TAE-01-NatalidadColombia", icon("github"), "Repositorio del proyecto", class = "btn", style = "background-color:#000000; color:#ffffff;"),
                                       hr(),
                                       # Referencia a los iconos usados con los hijos
                                       HTML('<div>Iconos diseñados por <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.es/" title="Flaticon">www.flaticon.es</a></div>')
                              ),
                              
                              tabPanel("Mapa",
                                       tags$h4("Enlace al reporte técnico"),
                                       tags$a(href="https://rpubs.com/Alexitouno19/TAE-01-NatalidadColombia", icon("book"), "Reporte técnico", class = "btn btn-primary"),
                                       tags$h4("Enlace al video promocional"),
                                       tags$a(href="https://youtu.be/h2PU-UY6-TY", icon("youtube"), "Video promocional", class = "btn btn-danger"),
                                       tags$h4("Enlace al respositorio del proyecto"),
                                       tags$a(href="https://github.com/juanescendales/TAE-01-NatalidadColombia", icon("github"), "Repositorio del proyecto", class = "btn", style = "background-color:#000000; color:#ffffff;"),
                                       hr(),
                                       # Referencia a los iconos usados con los hijos
                                       HTML('<div>Iconos diseñados por <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.es/" title="Flaticon">www.flaticon.es</a></div>')
                              )
                   ),
    ),
    #bootstrapPage(
    # numericInput('n', 'Number of obs', 100),
    #  plotOutput('plot')
    #),
    server = function(input, output) {
      output$plot <- renderPlot({ hist(runif(input$n)) })
    }
  )
  runApp(app)
}
shiny::runApp


