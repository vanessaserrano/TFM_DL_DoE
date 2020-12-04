# TFM Daniela Lama - DoE

#variable con lista de paquetes deseados
paqs<-c("tidyverse","shiny","DT","rhandsontable")

#variable de paquetes a instalar: son los deseados que NO est?n en los resultados de la columna 1 de la libreria. Si no especificaramos [,1], aparecer?a m?s informaci?n de los paquetes con library()$results. 
paqs2Install<-paqs[!(paqs%in%library()$results[,1])]

#variable de paquetes a cargar:son los deseados que NO est?n cargados en la libería de R. Especificar all.available=TRUE para ver los paquetes cargados. 
paqs2Load<-paqs[!(paqs%in%.packages())]

## PREGUNTA: los cargados son los m ismos instalados... por tanto... no es redundante???

#para aquellos paquetes en paquetes a instalar, instalar desde el repositorio. tydiverse no est? disponible como type="binary".
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
      verticalLayout(
        fileInput(inputId="cargar_archivo",label="Cargar caso"),
        radioButtons(inputId="caso_x",label="Empezar un caso nuevo",choices=c("Caso libre","Caso propuesto","Caso simulado")),
        wellPanel(style= "background: lightblue",
                  textOutput("descrip_caso")
        ),
        wellPanel(
          tags$head(
            tags$style(HTML('#intdatos{background-color:lightblue}'))
          ),
          div(style="display:inline-block;width:32%;text-align: left;",
              actionButton(inputId="ayuda",label="Ayuda")),
          div(style="display:inline-block;width:32%;text-align: center;",
              actionButton(inputId="autor",label="Autor")),
          div(style="display:inline-block;width:32%;text-align: right;",
              actionButton(inputId="intdatos",label="Empezar"))
        )
      )
    ),
    mainPanel(
      width=7,
      #plotOutput("distPlot"), #si quisiera tener el plot por fuera
      #h es el tamaño de fuente
      tabsetPanel(id="tabs1",type="tabs",
                  tabPanel(title="Guía",wellPanel(h4(""))),
                  tabPanel(title="Datos",
                           splitLayout(
                             numericInput(inputId="num_factores",label="Número de factores",value=0,min=1,step=1,max=20),
                             numericInput(inputId="num_interac",label="Número de interacciones",value=0,min=0,step=1)),
                           splitLayout(
                             DT::dataTableOutput("tabla_factores",width=250),
                             DT::dataTableOutput("tabla_interacciones",width=250)),
                           verticalLayout(
                             actionButton(inputId="borrar_datos",label="Reset"),
                             actionButton(inputId="calcmatriz",label="Generar diseño"),
                             textOutput("diseño_sugerido")),
                           tags$style(HTML('#irmatriz{background-color:lightblue}')),
                           div(style="display:inline-block;width:32%;text-align: right;",
                               actionButton(inputId="irmatriz",label="Continuar"))),
                  tabPanel(title="Matriz",actionButton(inputId="iranalisis",label="Analizar")),
                  tabPanel(title="Análisis"))
    )
  )
)

## SERVER SHINY

app_server<-function(input,output,session){
  
  observeEvent(input$intdatos,{
    updateTabsetPanel(session,"tabs1",selected="Datos")
  })
  
  observeEvent(input$irmatriz,{
    updateTabsetPanel(session,"tabs1",selected="Matriz")
  })
  
  observeEvent(input$iranalisis,{
    updateTabsetPanel(session,"tabs1",selected="Análisis")
  })
  
  observeEvent(input$ayuda,{
    showModal(modalDialog(
      title="AYUDA",renderText("Aquí no sé cómo hacer espacios entre líneas")
    ))
  })
  
  observeEvent(input$autor,{
    showModal(modalDialog(
      title="Información del autor",renderText("Misma historia que en ayuda")
    ))
  })
  
  tabla_dom=reactive({
    nombre_columna_fac<-c("Nombre Factor","Nivel")
    # d1=as.data.frame(matrix(nrow=input$num_factores,ncol=3))
    d1=data.frame(matrix(nrow=input$num_factores,ncol=2))
    if(input$num_factores==1){
      row.names(d1)<-c("A")
    }
    if(input$num_factores==2){
      row.names(d1)<-c("A","B")
    }
    if(input$num_factores==3){
      row.names(d1)<-c("A","B","C")
    }
    if(input$num_factores==4){
      row.names(d1)<-c("A","B","C","D")
    }
    if(input$num_factores==5){
      row.names(d1)<-c("A","B","C","D","E")
    }
    if(input$num_factores==6){
      row.names(d1)<-c("A","B","C","D","E","F")
    }
    if(input$num_factores==7){
      row.names(d1)<-c("A","B","C","D","E","F","G")
    }
    if(input$num_factores==8){
      row.names(d1)<-c("A","B","C","D","E","F","G","H")
    }
    if(input$num_factores==9){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I")
    }
    if(input$num_factores==10){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J")
    }
    if(input$num_factores==11){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K")
    }
    if(input$num_factores==12){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L")
    }
    if(input$num_factores==13){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M")
    }
    if(input$num_factores==14){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N")
    }
    if(input$num_factores==15){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")
    }
    if(input$num_factores==16){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
    }
    if(input$num_factores==17){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q")
    }
    if(input$num_factores==18){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R")
    }
    if(input$num_factores==19){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S")
    }
    if(input$num_factores==20){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T")
    }
    if(input$num_factores==21){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")
    }
    if(input$num_factores==22){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V")
    }
    if(input$num_factores==23){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W")
    }
    if(input$num_factores==24){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X")
    }
    if(input$num_factores==25){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y")
    }
    if(input$num_factores==26){
      row.names(d1)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
    }
    colnames(d1)<-nombre_columna_fac
    d1=as.data.frame(d1)
  })
  
  output$tabla_factores<-renderDataTable(tabla_dom(),editable=TRUE)
  
  
  tabla_inter=reactive({
    nombre_columna_inter<-c("Factor 1","Factor 2")
    d2=as.data.frame(matrix(nrow=input$num_interac,ncol=2))
    colnames(d2)<-nombre_columna_inter
    d2<-as.data.frame(d2)
  })
  
  output$tabla_interacciones<-renderDataTable(tabla_inter(),editable=TRUE)
  
  # tipo_caso<-reactiveValues(libre=0,paper=0,simul=0)
  # 
  # observeEvent(input$caso1,{
  #   tipo_caso$libre<-1
  #   tipo_caso$paper<-0
  #   tipo_caso$simul<-0
  # })
  # 
  # observeEvent(input$caso2,{
  #   tipo_caso$libre<-0
  #   tipo_caso$paper<-1
  #   tipo_caso$simul<-0
  # })
  # 
  # observeEvent(input$caso3,{
  #   tipo_caso$libre<-0
  #   tipo_caso$paper<-0
  #   tipo_caso$simul<-1
  # })
  
  observeEvent(input$borrar_datos,{
    updateNumericInput(session,"num_factores",value=0)
    updateNumericInput(session,"num_interac",value=0)
  })
  
  observeEvent(input$calcmatriz,{
    output$diseño_sugerido<-renderText("Diseño sugerido, debe depender de alguna variable de calc")
  })
  
  observeEvent(input$caso_x,{
    output$descrip_caso<-renderText({
      if(input$caso_x=="Caso libre")
        paste("Descripción caso libre")
      else
        if(input$caso_x=="Caso propuesto")
          paste("Descripcion caso paper")
      else
        if(input$caso_x=="Caso simulado")
          paste("Descripcion caso simulacion")
      else
        return()
    })}) 
  
}


## CORRER APLICACION
shinyApp(ui=app_ui,server=app_server)