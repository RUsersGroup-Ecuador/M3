#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

load(url("https://github.com/jonathantapia/EstadisticaModulo3/raw/master/Train.Rdata"))

library(shiny)
library(dplyr)
library(highcharter)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Modelamiento"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("n",
                    "Numero de registros:",
                    min = 1,
                    max = 30,# nrow(data), numero de filas
                    value = 10),
        selectInput("variable","Seleccione a variable:", choices = (names(data))[-c(1:3)],selected = "Outlet_Type"),
        tableOutput("tablaResumen"),
        downloadButton("descarga",label = "descargar")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        helpText("Modelamiento de ventas vs MRP"),
        tabsetPanel(type="pills",
                    tabPanel("tabla",
                             dataTableOutput("tabla")
                    ),
                    tabPanel("grafico",
                             
                             highchartOutput ("graf")
                             
                    )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  output$descarga <- downloadHandler(filename = function(){"data.csv"},
                                     content = function(file){write.csv2(data,file)}
  )
  
  #output$tabla <- renderTable({
    # generate bins based on input$bins from ui.R
    #head(data,input$n)
    #head(data[,c("Item_MRP","Item_Outlet_Sales",input$variable)],input$n)
  #})
  output$tabla <- renderDataTable({
    
    datatable(head(data[,c("Item_MRP", "Item_Outlet_Sales",input$variable)],input$n)
              ,
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              )
  
    )
  })
  
  output$tablaResumen <- renderTable({
    # group_by(data,input$variable)%>% summarize(Numero=n())
    table(data[,c(input$variable)])
  })
  
  output$graf <- renderHighchart({
    # group_by(data,input$variable)%>% summarize(Numero=n())
    hchart(data[,c(input$variable)], colorByPoint=TRUE,name="variable")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
