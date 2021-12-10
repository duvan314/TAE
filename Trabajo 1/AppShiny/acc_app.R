library(shiny)
library(tidyr)
library(rlang)
library(dplyr)
library(caret)
library(sf)
library(leaflet)

source("Predicciones.R", encoding = "UTF-8")
codigoBarrios <- read.csv("codigoBarrios.csv", encoding = "latin")
barrios <- st_read("Barrio_Vereda.shp", quiet = T)

ui <- fluidPage(
      title = "Accidentalidad en Medellín",
      # Algunos cambios en el css de algunos elementos
      tags$head(
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        tags$style(type = "text/css", "#imagen img {max-width: 100%; width: 100%; height: auto; max-height: 100%}"),
        tags$style("#texto_hijos{
                                 font-size: 20px;
                                 font-style: bold;
                                 text-align: center;
                                 }"),
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
      ),
      # Navegacion
      navbarPage("Accidentalidad en Medellín",
        inverse = TRUE, position = "fixed-top",

        # Tab Modelo
        tabPanel(
          "Visualización",
          fluidRow(
            column(
              8,
              # Descripcion
              wellPanel(
                fluidRow(
                  column(
                    12,
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
        tabPanel(
          "Predicción",
          tags$h4("Enlace al reporte técnico"),
          tags$a(href = "https://rpubs.com/Alexitouno19/TAE-01-NatalidadColombia", icon("book"), "Reporte técnico", class = "btn btn-primary"),
          tags$h4("Enlace al video promocional"),
          tags$a(href = "https://youtu.be/h2PU-UY6-TY", icon("youtube"), "Video promocional", class = "btn btn-danger"),
          tags$h4("Enlace al respositorio del proyecto"),
          tags$a(href = "https://github.com/juanescendales/TAE-01-NatalidadColombia", icon("github"), "Repositorio del proyecto", class = "btn", style = "background-color:#000000; color:#ffffff;"),
          hr(),
          # Referencia a los iconos usados con los hijos
          HTML('<div>Iconos diseñados por <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.es/" title="Flaticon">www.flaticon.es</a></div>')
        ),
        tabPanel(
          "Mapa",
          fluidRow(
            column(
              8,
              # Descripcion
              wellPanel(
                fluidRow(
                  column(
                    12,
                    tags$h4("Modo de uso:"),
                    tags$p("Para visualizar la información de cada barrio y el grupo al que pertenece, escoja el barrio desde el menú desplegable y haga clic en el botón \"Ver información\"."),
                    selectInput(
                      inputId = "codigoBarrio", width = "100%",
                      label = "Seleccione el barrio",
                      choices = setNames(codigoBarrios$CODIGO, codigoBarrios$BARRIO)
                    ),
                    tags$h3("Mapa"),
                    textOutput("codBarrio"),
                    leafletOutput("leaflet")
                  )
                )
              )
            )
          )
        )
      )
    )
 
server <- function(input, output) {
      output$plot <- renderPlot({
        hist(runif(input$n))
      })
      
      output$codBarrio <- renderText({
        print(paste("Codigo del barrio: ", input$codigoBarrio))
      })

      output$leaflet <- renderLeaflet({
        barr <- barrios %>%
          filter(CODIGO == input$codigoBarrio)

        # Graficamos el mapa resultante
        leaflet(barr) %>%
          addTiles() %>%
          addPolygons()
      })
    }

shinyApp(ui, server)
