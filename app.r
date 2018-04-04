#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput("txt",label = "Texto"),
         numericInput("bins",
                     "Number of bins:", value = 10),
         tableOutput("distPlot")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("txt"),
         plotOutput("graf")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$txt <- renderText({input$txt})
  
   output$distPlot <- renderTable({
      # generate bins based on input$bins from ui.R
      x    <- faithful 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      head(x,input$bins)
   })
   
   output$graf <- renderPlot({
     mp <- data(mpg, package="ggplot2")
     # mpg <- read.csv("http://goo.gl/uEeRGu")
     mp <- head(mp, input$bins*5)
     # Scatterplot
     theme_set(theme_bw())  # pre-set the bw theme.
     mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
     g <- ggplot(mpg, aes(cty, hwy)) + 
       geom_count() + 
       geom_smooth(method="lm", se=F)
     
     ggMarginal(g, type = "histogram", fill="transparent")
     ggMarginal(g, type = "boxplot", fill="transparent")
     # ggMarginal(g, type = "density", fill="transparent")
     
   })
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

