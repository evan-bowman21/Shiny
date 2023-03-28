# Evan Bowman

library(shiny)
library(ggplot2)

mtcars <- mtcars
#head(mtcars)


ui <- fluidPage(
  varSelectInput("var1", "X variable", data = mtcars),
  radioButtons("plot", "Choose a plot type", 
               choices = c("Density Plot", "Histogram", "Frequency Polygon")),
  plotOutput("plot")
)

server <- function(input, output) {
  
     output$plot <- renderPlot({
       myplot <- ggplot(mtcars, aes(x = !!input$var1))
       
       if (input$plot == "Density Plot") {
         
      myplot <- myplot + geom_density()
      myplot
       } else if (input$plot == "Histogram") {
        
        myplot <- myplot + geom_histogram()
        myplot
       } else {
        
        myplot <- myplot + geom_freqpoly()
        myplot
      }
    })
  } 

shinyApp(ui = ui, server = server)