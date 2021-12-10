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
