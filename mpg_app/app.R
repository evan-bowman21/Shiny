# Evan Bowman

library(shiny)
library(ggplot2)

mpg <- mpg
#head(mpg)
mpg$cyl = as.factor(mpg$cyl)

ui <- fluidPage(
  varSelectInput("var1", "Variable 1", data = mpg, selected = "cty"),
  varSelectInput("var2", "Variable 2", data = mpg, selected = "hwy"),
  varSelectInput("var3", "Variable 3", data = mpg, selected = "class"),
  plotOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(mpg, aes(x = !!(input$var1), y = !!input$var2)) +
      geom_point(aes(color = !!input$var3))
  })
}

shinyApp(ui = ui, server = server)