load(url("https://github.com/RUsersGroup-Ecuador/M3/raw/master/data2.Rdata"))
data$CIIU4 <- as.character(data$CIIU4)
library(shiny)
library(dplyr)
library(highcharter)
library(DT)
ui <- fluidPage(
   tabsetPanel(type = "tabs",
               tabPanel("Principal",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("n",
                                        "Registros",min = 1,max = 30,value = 10),
                            selectInput("variable","Seleccione la variable:",
                                        choices = names(data)[-c(1:3,6,7,12)],selected = "TIPO_COMPANIA"),
                            
                            tableOutput("tabla_resumen")
                          ),
                          mainPanel(
                            tabsetPanel(type ="pills",
                                        tabPanel("Tabla",dataTableOutput("tabla"),
                                                 downloadButton("descarga","Descargar")
                                                 ),
                                        tabPanel("Grafico",
                                                 conditionalPanel(condition = 'input.variable!="ESPUBLICA"',
                                                                  highchartOutput("grafico")                 
                                                 )
                                                 )
                                        )
                            )
                          )
                        ),
               tabPanel("Secundario",
                        h3("Consulta de RUC"),
                        textInput("ruc", "Ingrese el RUC"),
                        actionButton("buscar", "BUSCAR"),br(),br(),br(),
                        textOutput("resultado"),br(),
                        tableOutput("resultado2"),br(),
                        verbatimTextOutput("resultado1")
                        ),
               tabPanel("Tercero",
                        selectInput("cia", "Seleccione la Compañía", 
                                    choices = data$DENOMINACION)
               )
               )
   )
server <- function(input, output) {
  
  buscado <- eventReactive(input$buscar,{
    if(is.na(as.numeric(input$ruc))==F & 
       nchar(input$ruc)==13) {
      data %>% filter(RUC==input$ruc)
    } else {
      data.frame(0)
    }
  })
  
  #buscado <- function() {d}
  output$resultado <- renderText({
    # input<-list(0)
    # input$ruc<-"1111111111111"
    ifelse(nrow(buscado())==1,
      paste("El RUC buscado corresponde a",buscado()$DENOMINACION),
      "RUC no encontrado")
    })
    
  output$resultado2 <- renderTable({
    buscado()[,-which(names(buscado())=="CIIU4")]
  })
  
  CIIU4 <- reactive(buscado()$CIIU4)
  
  output$resultado1 <- renderText(paste("CIIU del RUC",CIIU4()))
  
    output$descarga <- downloadHandler(
    filename = function() {"data.csv"},
    content = function(file){
      write.csv2(data, file)
      })
  
   output$tabla <- renderDataTable({
      datatable(head(data[,c("RUC", "DENOMINACION",input$variable)],input$n),
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ))
   })
   
   output$tabla_resumen <- renderTable({
     table(data[, c(input$variable)])
     #group_by(data,input$variable) %>% summarize(Numero=n())
   })
   output$grafico <- renderHighchart({
     hchart(data[c(1:input$n),c(input$variable)], colorByPoint = TRUE, name = "variable")
     #group_by(data,input$variable) %>% summarize(Numero=n())
   })
}
shinyApp(ui = ui, server = server)

