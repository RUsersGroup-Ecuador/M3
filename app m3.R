load(url("https://github.com/RUsersGroup-Ecuador/M3/raw/master/data2.Rdata"))
library(shiny)
library(dplyr)
library(highcharter)
library(DT)
ui <- fluidPage(
   titlePanel("SUPERCIAS"),
   tabsetPanel(type = "tabs",
               tabPanel("Principal",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("n",
                                        "Registros",min = 1,max = 30,value = 10),
                            selectInput("variable","Seleccione la variable:",
                                        choices = names(data)[-c(1:3)],selected = "CIIU4"),
                            
                            tableOutput("tabla_resumen")
                          ),
                          mainPanel(
                            highchartOutput("grafico"),
                            downloadButton("descarga","Descargar"),
                            tabsetPanel(type ="pills",
                                        tabPanel("Tabla",dataTableOutput("tabla")
                                                 ),
                                        tabPanel("Grafico",helpText("Texto")
                                                 )
                                        )
                            )
                          )
                        ),
               tabPanel("Secundario")
               )
   )
server <- function(input, output) {

    output$descarga <- downloadHandler(
    filename = function() {"data.csv"},
    content = function(file){
      write.csv2(data, file)
      })
  
   output$tabla <- renderDataTable({
      datatable(head(data[,c("RUC", "DENOMINACION",input$variable)],input$n))
   })
   
   output$tabla_resumen <- renderTable({
     table(data[, c(input$variable)])
     #group_by(data,input$variable) %>% summarize(Numero=n())
   })
   output$grafico <- renderHighchart({
     hchart(data[,c(input$variable)], colorByPoint = TRUE, name = "variable")
     #group_by(data,input$variable) %>% summarize(Numero=n())
   })
}
shinyApp(ui = ui, server = server)

