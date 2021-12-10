library(shiny)
library(tidyr)
library(rlang)
library(dplyr)
library(caret)
source("Modelo.R")

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
                   wellPanel(
                     tags$h3("Selección de Datos"),
                     fluidRow(
                       column(6,
                              selectInput(inputId = "inputPeriodicidad", width = "100%",
                                          label = "Periodicidad de la serie de tiempo", 
                                          choices = c("Anual" ="0",
                                                      "Mensual" = "1",
                                                      "Diaria" = "2")),
                       )
                     ),
                     fluidRow(
                       column(6,
                              selectInput(inputId = "inputTipoAcc", width = "100%",
                                          label = "Tipo de Accidente", 
                                          choices = c("Atropello" ="0",
                                                      "Caída de Ocupante" = "1",
                                                      "Choque" = "2",
                                                      "Otro" = "3")),
                       )
                     ),
                     dateInput("date1", "Fecha de inicio:", min = "2014-08-01", max = "2020-08-30", format = "yy/mm/dd"
                     ),
                     dateInput("date2", "Fecha de fin:", min = "2014-08-01", max = "2020-08-30", format = "yy/mm/dd"
                     ),
                  ),
                   
          ),
          # Espacio de enlaces 
          tabPanel("Predicción",
                   fluidRow(
                     column(8,
                            # Descripcion
                            wellPanel(
                              fluidRow(
                                column(12,
                                       tags$p("La siguiente es una aplicación web de un modelo de predicción de la ocurrencia de incidentes viales en la ciudad de Medellín, 
                                    con base en los datos publicados por la Alcaldía de Medellín en el portal MeData."),
                                       tags$h4("Modo de uso:"),
                                       tags$p("Para generar la predicción de los datos deseados, complete los campos con la temporalidad y el tipo de accidente. Luego presiona el botón: Enviar datos")
                                )
                              ),
                            ),
                     ),
                   ),
                   # Campos a rellenar
                   wellPanel(
                     tags$h3("Características")),
          ),
          
          tabPanel("Mapa",
                   tags$h4("Enlace al reporte tÃ©cnico"),
                   tags$a(href="https://rpubs.com/Alexitouno19/TAE-01-NatalidadColombia", icon("book"), "Reporte tÃ©cnico", class = "btn btn-primary"),
                   tags$h4("Enlace al video promocional"),
                   tags$a(href="https://youtu.be/h2PU-UY6-TY", icon("youtube"), "Video promocional", class = "btn btn-danger"),
                   tags$h4("Enlace al respositorio del proyecto"),
                   tags$a(href="https://github.com/juanescendales/TAE-01-NatalidadColombia", icon("github"), "Repositorio del proyecto", class = "btn", style = "background-color:#000000; color:#ffffff;"),
                   hr(),
                   # Referencia a los iconos usados con los hijos
                   HTML('<div>Iconos diseÃ±ados por <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.es/" title="Flaticon">www.flaticon.es</a></div>')
          ),
          
          tabPanel("Enlaces",
                   tags$h4("Enlace al reporte técnico"),
                   tags$a(href="https://rpubs.com/Alexitouno19/TAE-01-NatalidadColombia", icon("book"), "Reporte tÃ©cnico", class = "btn btn-primary"),
                   tags$h4("Enlace al video promocional"),
                   tags$a(href="https://youtu.be/h2PU-UY6-TY", icon("youtube"), "Video promocional", class = "btn btn-danger"),
                   tags$h4("Enlace al respositorio del proyecto"),
                   tags$a(href="https://github.com/juanescendales/TAE-01-NatalidadColombia", icon("github"), "Repositorio del proyecto", class = "btn", style = "background-color:#000000; color:#ffffff;"),
                   hr(),
                   # Referencia a los iconos usados con los hijos
                   HTML('<div>Iconos diseÃ±ados por <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.es/" title="Flaticon">www.flaticon.es</a></div>')
          )
         ),
    ),
  
    server = function(input, output) {
      output$plot <- renderPlot({ hist(runif(input$n)) })
    }
  ))
  
  
  # Running a Shiny app object
  app <- shinyApp(
    ui <- fluidPage(title = "Incidentes viales en Medellín"
      
    ),
    server = function(input, output) {
      output$plot <- renderPlot({ hist(runif(input$n)) })
    }
  )
  runApp(app)
}
shiny::runApp


