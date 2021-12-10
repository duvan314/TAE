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
infoBarrios <- read.csv("../datos/caracteristicas_barrios.csv", encoding = "latin")

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
               wellPanel(
                 tags$h3("Selección de Datos"),
                 fluidRow(
                   column(
                     6,
                     selectInput(
                       inputId = "inputPeriodicidad", width = "100%",
                       label = "Periodicidad de la serie de tiempo",
                       choices = c(
                         "Anual" = "0",
                         "Mensual" = "1",
                         "Diaria" = "2"
                       )
                     ),
                   )
                 ),
                 fluidRow(
                   column(
                     6,
                     selectInput(
                       inputId = "inputTipoAcc", width = "100%",
                       label = "Tipo de Accidente",
                       choices = c(
                         "Atropello" = "0",
                         "Caída de Ocupante" = "1",
                         "Choque" = "2",
                         "Otro" = "3"
                       )
                     ),
                   )
                 ),
                 dateInput("date1", "Fecha de inicio:", min = "2014-08-01", max = "2020-08-30", format = "yy/mm/dd"),
                 dateInput("date2", "Fecha de fin:", min = "2014-08-01", max = "2020-08-30", format = "yy/mm/dd"),
                 actionButton(inputId = "enviar", width = "100%",
                              label = "Enviar datos",
                              class = "btn-success"),
               ),
             ),
             # Espacio de Predicción
             tabPanel(
               "Predicción",
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
                         tags$p("Para visualizar la predicción de los datos, complete los campos con la temporalidad, el tipo de accidente y las fechas (Enero-2018, Diciembre 2025). Luego presiona el botón: Enviar datos")
                       )
                     ),
                   ),
                 ),
               ),
               wellPanel(
                 tags$h3("Selección de Datos"),
                 fluidRow(
                   column(
                     5,
                     selectInput(
                       inputId = "inputPeriodicidadPred", width = "100%",
                       label = "Periodicidad de la serie de tiempo",
                       choices = c(
                         "Anual" = "0",
                         "Mensual" = "1",
                         "Diaria" = "2"
                       )
                     ),
                   )
                 ),
                 fluidRow(
                   column(
                     5,
                     selectInput(
                       inputId = "inputTipoAccPred", width = "100%",
                       label = "Tipo de Accidente",
                       choices = c(
                         "Atropello" = "0",
                         "Caída de Ocupante" = "1",
                         "Choque" = "2",
                         "Otro" = "3"
                       )
                     ),
                   )
                 ),
                 fluidRow(
                   column(
                     5,
                     dateInput("date3", "Fecha de inicio:", min = "2018-01-01", max = "2025-12-31", format = "yy/mm/dd"),
                     dateInput("date4", "Fecha de fin:", min = "2018-01-01", max = "2025-12-31", format = "yy/mm/dd"),
                     actionButton(inputId = "enviar2", width = "100%",
                                  label = "Enviar datos",
                                  class = "btn-success"),
                   )
                 ),
               ),
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
                         tags$p("Para visualizar la información de cada barrio y el grupo al que pertenece, escoja el barrio desde el menú desplegable."),
                         selectInput(
                           inputId = "nombreBarrio", width = "100%",
                           label = "Seleccione el barrio",
                           choices = setNames(codigoBarrios$BARRIO, codigoBarrios$BARRIO)
                         ),
                         tags$h3("Mapa"),
                         textOutput("codBarrio"),
                         leafletOutput("leaflet")
                       )
                     )
                   )
                 ),
                 column(
                   4,
                   # Información
                   wellPanel(
                     fluidRow(
                       column(
                         8,
                         tags$h4("Información del barrio."),
                         tableOutput("infoBarr")
                       )
                     )
                   )
                 )
               )
             )
  )
)

server <- function(input, output) {
  observeEvent(input$enviar2, {
    output$prediccion <- renderPlot({
      plot_prediccion(input$inputTipoAccPred,input$inputPeriodicidadPred, input$date3, input$date4)
    }
    )
  }
  )
  
  
  output$plot <- renderPlot({
    hist(runif(input$n))
  })
  
  output$table <- renderDataTable(iris,
                                  options = list(
                                    pageLength = 5,
                                    initComplete = I("function(settings, json) {alert('Done.');}")
                                  )
  )
  
  output$infoBarr <- renderTable({
    if (input$nombreBarrio == "TODOS") {
      unique(infoBarrios %>% select(grupo, categoria, color))
    } else {
      barr <- infoBarrios %>% filter(BARRIO == input$nombreBarrio)
      data.frame(Variable = colnames(infoBarrios), Valor = t(barr))
    }
  })
  
  output$leaflet <- renderLeaflet({
    if (input$nombreBarrio == "TODOS") {
      leaflet(barrios) %>%
        addTiles() %>%
        addPolygons(color = infoBarrios$color, popup = paste(infoBarrios$BARRIO, "- Grupo:", infoBarrios$grupo))
    } else {
      infoBarr <- infoBarrios %>%
        filter(BARRIO == input$nombreBarrio)
      
      barr <- barrios %>%
        filter(NOMBRE == input$nombreBarrio)
      
      leaflet(barr) %>%
        addTiles() %>%
        addPolygons(color = infoBarr$color, popup = paste(input$nombreBarrio, "- Grupo:", infoBarr$grupo))
    }
  })
}

shinyApp(ui, server)
