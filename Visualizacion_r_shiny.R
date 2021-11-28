#--------------------------
# Visualizacion de rendimiento académico universitario entre 2017 y 2019
# Herramientas: Shiny con R 
# Lic. Rocio Radulescu
# rsradulescu@gmail.com
# 2021-11
#--------------------------

library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(lubridate)
library(plotly)
library(RColorBrewer)


#leo y formateo datos de docentes y estudiantes
df_examen <-read_csv('dataset_consulta.csv')  %>% 
  mutate(persona= as.numeric(persona),
         propuesta= as.numeric(cod_propuesta),
         nota= as.numeric(nota),
         long= as.numeric(longitud),
         lat= as.numeric(latitud)
         )

#agrego atributos adicionales para futuras observaciones: 
# edad
df_examen$edad = (Sys.Date()-df_examen$fecha_nacimiento)/365.25 
#año de examen 
df_examen$anio_examen = format(df_examen$fecha_examen, format = "%Y")

# UI
ui <- bootstrapPage(
  
  navbarPage(theme = shinytheme("flatly"), #otra opcion lumen 
             "Visualización de Datos - Actividad Académica", id="nav",
             
             tabPanel("Mapa", 
                      
                      leafletOutput("map" ,width="100%",height="500px"),
                      absolutePanel( fixed = TRUE,
                                     top = 80, left = "auto", right = 20, 
                                     bottom = "auto",
                                     width = 200, height = "auto",
                                     p("Distribución geográfica de notas por sede de la Universidad (2017-2019)"),
                                     #primer parametro: el selector de situaciones
                                     selectInput(inputId = "resultado", #id
                                                 label = "Resultado de examen",#lo que figura
                                                 choices = c('Todos', 'Aprobado', 'Desaprobado', 'Ausente'),
                                                 selected = 1) #default?
                      )
             ),#close tabpanel "mapa"
             tabPanel("Notas por Sede",
                      absolutePanel( fixed = TRUE,
                                     top = 80, left = "auto", right = 10, 
                                     bottom = "auto",
                                     width = 200, height = "auto",
                                     
                                     selectInput(inputId = "sedePlot", #id
                                                 label = "Sede",#lo que figura
                                                 choices = c('Comodoro Rivadavia','Puerto Madryn', 'Esquel', 'Trelew'),
                                                 selected = 1), #default?
                                     hr()
                                     #downloadButton(outputId = 'download_graf_barra', label = 'Descarga')
                      ),
                      
                      plotOutput ("graficobarra", width = "95%") #id
             )#close tabpanel "plot"
  )#close navpanel
)# close ui

# -----------------------------------------------------------
# Server
server <- function(input, output,session) {
  
  subset_resultado = reactive({
    if (input$resultado!="Todos") { #casos notas especificas 
      df_examen %>% 
        filter(resultado %in% input$resultado) %>% #obtengo la cantidad del total
        group_by(nombre,lat,long) %>% #obtengo la cantidad del total
        summarise(count_resultado=n()) %>% 
        ungroup()   
    } else #si elige todas
      df_examen %>% 
        group_by(nombre,lat,long) %>% #obtengo la cantidad del total
        summarise(count_resultado=n()) %>% 
        ungroup()
  })
  
  #---- panel mapa
  output$map <- renderLeaflet({
    
    colorEscala <- colorNumeric(
      palette = "RdYlBu",
      domain = subset_resultado()$count_resultado,
      alpha = FALSE,
      reverse = FALSE
    )
    
    leaflet(df_examen) %>%
      addTiles() %>%
      # set view to Argentina
      setView(lng = -66.61, lat = -44.41, zoom = 6)%>%
      addCircles(~subset_resultado()$long,
                 ~subset_resultado()$lat ,
                 color = ~colorEscala(subset_resultado()$count_resultado),
                 popup = paste("<b>",subset_resultado()$nombre,"</b>",
                               "<br/>",
                               "Cantidad:",
                               subset_resultado()$count_resultado,
                               input$resultado),
                 weight = ~sqrt(as.numeric(subset_resultado()$count_resultado)) 
                 
      )%>%
      addLegend("bottomright", 
                title= "Cantidad",
                pal = colorEscala, 
                values = ~subset_resultado()$count_resultado,
                bins = 6,
                opacity = 1)
  })
  
  #---- panel barplots
  subset_resultado_plot = reactive({
    if (input$sedePlot!="Todos") { #casos particulares
      df_examen %>%
        filter(nombre %in% input$sedePlot) %>%
        filter(nota != -1) #excluyo ausentes
    } else #caso "todos"
      df_examen %>%
      filter(nota != -1)#excluyo ausentes
      
  })

  output$graficobarra <- renderPlot({
    par(mar=c(9,4,3,3)) #margenes del grafico

    graficoplot <- hist(subset_resultado_plot()$nota,
                        breaks = 10)
    colorcut <- cut(graficoplot$breaks, c(-Inf, 4, Inf))
    
    plot(graficoplot, 
         col=c("orange","skyblue3")[colorcut],
         xlab = "Notas de examen", 
         ylab = "Frecuencia",
         main = "Histográma de frecuencias de notas por Sede Universitaria",
    )
    legend("topleft", 
           c("Aprobados", "Desaprobados"), 
           fill=c("orange","skyblue3")
           )
  })
}

# App: esta es la parte donde todo corre
shinyApp(ui = ui, server = server)
