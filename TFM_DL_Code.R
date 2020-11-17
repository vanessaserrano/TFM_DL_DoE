# TFM Daniela Lama - DoE

install.packages("shiny")
library(shiny)

#El paquete tidyverse tiene que cargarse así:
if(!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")}

#variable con lista de paquetes deseados
paqs<-c("tidyverse","shiny")

#variable de paquetes a instalar: son los deseados que NO están en los resultados de la columna 1 de la libreria. Si no especificaramos [,1], aparecería más información de los paquetes con library()$results. 
paqs2Install<-paqs[!(paqs%in%library()$results[,1])]

#variable de paquetes a cargar:son los deseados que NO están cargados en la libería de R. Especificar all.available=TRUE para ver los paquetes cargados. 
paqs2Load<-paqs[!(paqs%in%.packages())]

## PREGUNTA: los cargados son los m ismos instalados... por tanto... no es redundante???

#para aquellos paquetes en paquetes a instalar, instalar desde el repositorio. tydiverse no está disponible como type="binary".
for(pckg in paqs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/", quiet=TRUE,type="source")}

## PREGUNTA: verificar type="binary" o type="source" en help, y quiet=TRUE.

#para aquellos paquetes en paquetes a cargar, 
for(pckg in paqs2Load) {library(pckg,character.only=TRUE)}

#COEFICIENTES PAPER IMANDI

b0 <-(-1118.981953125)
b1 <-(1208.68854166667)
b2 <-(19.9901145833333)
b3 <-(855.068229166668)
b4 <-(395.516145833336)
b11 <-(-839.114583333334)
b12 <-(-3.981875)
b13 <-(-513.468750000001)
b14 <-(-39.2187500000027)
b22 <-(-0.112123958333333)
b23 <-(-2.99031250000001)
b24 <-(-1.26531250000001)
b33 <-(-264.653645833333)
b34 <-(-190.453125000001)
b44 <-(-124.841145833333)
desvestax1 <-(0.000607274)
desvestax2 <-(0.141697155)	
desvestax3<-(0.001416972)
desvestax4<-(0.001214547)
minvar1<-(0.08)
minvar2<-(48)
minvar3<-(0.26)
minvar4<-(0.16)
maxvar1<-(0.52)
maxvar2<-(92)
maxvar3<-(1.14)
maxvar4<-(1.04)

## USER INTERFASE SHINY

app_ui<-fluidPage(
  titlePanel("Bienvenido a AppDOE"),
  #Tipo de plantilla 
  sidebarLayout(
    sidebarPanel(
      width=5,
      titlePanel(h3("Seleccione el caso de preferencia")),
      sidebarLayout(
        sidebarPanel(
          width=3,
          actionButton(inputId="caso1",label="Caso libre"),
          actionButton(inputId="caso2",label="Caso propuesto"),
          actionButton(inputId="caso3",label="Caso simulado")),
        mainPanel(
          textOutput("descrip_caso")
        )
      )
    ),
    mainPanel(
      width=7,
      #plotOutput("distPlot"), #si quisiera tener el plot por fuera
      #h es el tamaño de fuente
      tabsetPanel(id="tabs1",type="tabs",
                  tabPanel(title="Caso",wellPanel(h4("Descripción de cada caso"))),
                  tabPanel(title="Datos",flowLayout(tableOutput("data"))),
                  tabPanel(title="Matriz"),
                  tabPanel(title="Análisis"))
    )
  )
)

## SERVER SHINY

app_server<-function(input,output){
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')})
  
  output$data<-renderTable(head(iris))
  
  
  
  tipo_caso<-reactiveValues(libre=0,paper=0,simul=0)
  
  observeEvent(input$caso1,{
    tipo_caso$libre<-1
    tipo_caso$paper<-0
    tipo_caso$simul<-0
  })
  
  observeEvent(input$caso2,{
    tipo_caso$libre<-0
    tipo_caso$paper<-1
    tipo_caso$simul<-0
  })
  
  observeEvent(input$caso3,{
    tipo_caso$libre<-0
    tipo_caso$paper<-0
    tipo_caso$simul<-1
  })
  
  output$descrip_caso<-renderText({
    if(tipo_caso$libre)
      paste("Descripción caso libre")
    else
      if(tipo_caso$paper)
        paste("Descripcion caso paper")
    else
      if(tipo_caso$simul)
        paste("Descripcion caso simulacion")
    else
      return()
  })
  
}


## CORRER APLICACION
shinyApp(ui=app_ui,server=app_server)