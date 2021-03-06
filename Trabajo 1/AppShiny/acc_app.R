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
               
                   # Descripcion
                   wellPanel(
                     fluidRow(
                       column(
                         8,
                         tags$h1("Causa y gravedad en accidentes"),
                         imageOutput("plotAccidentes"),
                         tags$p("Se encuentra que la mayor causa de accidentes es por choques."),
                         tags$p(("Los accidentes con heridos son mayormente causados por choques y otras causas."),
                         tags$p("La frecuencia de accidentes donde involucra muertes es muy inferior comparativamente 
                                con las demás categorías de gravedad.")
                         
                       )
                     )

                 )
               ),
               wellPanel(
                   fluidRow(
                     column(
                       8,
                       tags$h1("Comportamiento de accidentes segun fechas"),
                       imageOutput("plotPeriodicidad"),
                       tags$p("Se visualiza que se presenta una mayor accidentabilidad los días Martes,Miércoles,jueves
                       y viernes.Se infiere que el día viernes se presenta mayor accidentabilidad porque comienza fin de 
                       semana provocando más movilidad en la ciudad de Medellín y el día domingo disminuye notablemente 
                       la accidentabilidad en el cual muchas personas no laboran y hay menos flujo vehicular."),
                       tags$p("En el mes de agosto y julio es donde el número de accidentes es superior , en julio 
                       esto puede darse por el periodo de vacaciones y en agosto por algunos eventos relacionados 
                       con feria de flores.En enero, abril y junio se presenta la menor accidentabilidad."),
                       tags$p("En el año 2016 se presentó la mayor accidentabilidad, El número de accidentes para 
                       el 2020 es bajo debido a la crisis sanitaria que afrontaba el mundo por motivos del COVID-19 
                       y se debe tener en cuenta que los registros en este año solo son hasta el 31 de agosto."),
                       tags$p("Durante el dia, el mayor número de accidentes ocurren en las hora pico, alrededor
                              de las 6am, 12pm y 6pm. ")
                              
                       )
                     )
                   )
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
                         tags$p("Para visualizar la predicción de los datos, complete los campos con la temporalidad, el tipo de accidente y las fechas (Enero 2018 - Diciembre 2019). Luego presiona el botón: Enviar datos")
                       )
                     ),
                   ),
                 ),
               ),
               sidebarLayout(
               sidebarPanel(
                 tags$h3("Selección de Datos"),
                 fluidRow(
                   column(
                     8,
                     selectInput(
                       inputId = "inputPeriodicidadPred", width = "100%",
                       label = "Periodicidad de la serie de tiempo",
                       choices = c(
                         "Anual" = "anual",
                         "Mensual" = "mensual",
                         "Semanal" = "semanal",
                         "Diaria" = "diaria"
                       )
                     ),
                   )
                 ),
                 fluidRow(
                   column(
                     8,
                     selectInput(
                       inputId = "inputTipoAccPred", width = "100%",
                       label = "Tipo de Accidente",
                       choices = c(
                         "Atropello" = "Atropello",
                         "Caída de Ocupante" = "Caída Ocupante",
                         "Choque" = "Choque",
                         "Otro" = "Otro"
                       )
                     ),
                   )
                 ),
                 fluidRow(
                   column(
                     8,
                     dateInput("date3", "Fecha de inicio:", min = "2018-01-01", max = "2019-12-31", format = "yyyy-mm-dd", startview = "decade"),
                     dateInput("date4", "Fecha de fin:", min = "2018-01-01", max = "2019-12-31", format = "yyyy-mm-dd", startview = "decade"),
                     actionButton(inputId = "enviar2", width = "100%",
                                  label = "Enviar datos",
                                  class = "btn-success"),
                   )
                 ),
               ),
               mainPanel(
                 fluidRow(
                   column(
                     10,
                     tags$h3(textOutput("Resultado")),
                     plotOutput("prediccion")
                   )
                 )
               )
               )
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
             ),
             tabPanel(
               "Enlaces",
               tags$h4("Enlace al reporte técnico"),
               tags$a(href = "https://rpubs.com/duvan/AccidentabilidadMedellinTAE", icon("book"), "Reporte técnico", class = "btn btn-primary"),
               tags$h4("Enlace al video promocional"),
               tags$a(href = "https://youtu.be/h2PU-UY6-TY", icon("youtube"), "Video promocional", class = "btn btn-danger"),
               tags$h4("Enlace al respositorio del proyecto"),
               tags$a(href = "https://github.com/duvan314/TAE/tree/main/Trabajo%201", icon("github"), "Repositorio del proyecto", class = "btn", style = "background-color:#000000; color:#ffffff;"),
               hr(),
               # Referencia a los iconos usados con los hijos
               HTML('<div>Iconos diseñados por <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.es/" title="Flaticon">www.flaticon.es</a></div>')
             ),
  )
)

server <- function(input, output) {
  observeEvent(input$enviar2, {
    output$prediccion <- renderPlot({
      plot_prediccion(input$inputTipoAccPred,input$inputPeriodicidadPred, input$date3, input$date4)
    })
  })
  
  output$plotAccidentes <- renderImage({
    return(list(
        src = "../imagenes/plotTipoDeAccidente.png",
        contentType = "image/png",
        height="100%",
        width="100%",
        alt = "Accidentes")
        )
    }, deleteFile = FALSE)
  
  output$plotPeriodicidad <- renderImage({
    return(list(
      src = "../imagenes/plotPeriodicidad.png",
      contentType = "image/png",
      height="100%",
      width="100%",
      alt = "Accidentes")
    )
  }, deleteFile = FALSE)
  
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
