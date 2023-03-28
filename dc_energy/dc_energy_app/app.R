# Evan Bowman

library(shiny)
library(tidyverse)
library(bslib)
library(broom)

energy <- read_rds("../data/energy_year.rds")

energy$Report_Year = as.factor(energy$Report_Year)
energy$Type_SS = as.factor(energy$Type_SS)
energy$Type_EPA = as.factor(energy$Type_EPA)
energy$Metered_Energy = as.factor(energy$Metered_Energy)
energy$Metered_Water = as.factor(energy$Metered_Water)
energy$Ward = as.factor(energy$Ward)

energy <- mutate(energy, Era = case_when(
          Built < 1900 ~ "Pre-1900",
          Built >= 1900 & Built < 1951 ~ "Early-Mid 20th",
          Built >= 1951 & Built < 2000 ~ "Late 20th",
          Built >= 2000 & Built < 2011 ~ "Aughts",
          Built >= 2011 ~ "Teens and later"))
energy$Era = factor(energy$Era, levels = c("Pre-1900", "Early-Mid 20th", "Late 20th", "Aughts", "Teens and later"), ordered = TRUE)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Analyzing Building Energy Performance", windowTitle = "Analyzing Building Energy Performance"),
  
  mainPanel(
    tabsetPanel(
      type = "pills",
      tabPanel("Univariate",
               sidebarLayout(
                 sidebarPanel(
                   varSelectInput("uni_var", "Variable?", data = energy, selected = "Energy_Star_Score"),
                   checkboxGroupInput("uni_year", "Which Report Years?", choices = c("2011":"2020"), selected = "2020"),
                   checkboxInput("uni_flip", "Flip Coordinates on Factors?"),
                   checkboxInput("uni_log", "Log Transform?"),
                   sliderInput("uni_bins", "Number of Bins?", min = 1, max = 100, value = 40),
                   numericInput("uni_null", "Null Value", min = 0, max = 100, value = 0, step = .05)
                 ),
                 mainPanel(plotOutput("plot1"),
                           tableOutput("table1"))
               )
               ), # End tabPanel1
      tabPanel("Bivariate",
               sidebarLayout(
                 sidebarPanel(
                   varSelectInput("bi_x", "X Variable?", data = energy, selected = "Source_EUI"),
                   checkboxInput("bi_x_log", "Log Transform?"),
                   varSelectInput("bi_y", "Y Variable?", data = energy, selected = "Site_EUI"),
                   checkboxInput("bi_y_log", "Log Transform?"),
                   checkboxInput("bi_OLS", "Fit OLS model?"),
                   checkboxGroupInput("bi_year", "Which Report Years?", choices = c("2011":"2020"), selected = "2020")
                 ),
                 mainPanel(plotOutput("plot2"))
               )), # End tabPanel2
      tabPanel("Spreadsheet",
               checkboxInput("num_fac", "Numeric or Factor Only?"),
               dataTableOutput("dynamic"))
    ) # End tabsetPanel3
  ) # End mainPanel
  
) # End fluidPage

server <- function(input, output) {
 
  energy_uni <- reactive(
    
    energy %>%
    filter(Report_Year %in% input$uni_year)
 )

  output$plot1 <- renderPlot({
    plot1 <- ggplot(data = energy_uni(), aes(x = !!input$uni_var))
    
    if (is.numeric(energy[[input$uni_var]])) {
      if (input$uni_log == TRUE) {
        
        plot1 <- plot1 + geom_histogram(bins = input$uni_bins) + scale_x_log10() + facet_grid(~ Report_Year)
        plot1
      } else {
      plot1 <- plot1 + geom_histogram(bins = input$uni_bins) + facet_grid(~ Report_Year)
      plot1
      }
    }
    
    else if (is.factor(energy[[input$uni_var]])) {
      if (input$uni_flip == TRUE) {
        
        plot1 <- plot1 + geom_bar() + coord_flip() + facet_grid(~ Report_Year)
        plot1
      } else {
      plot1 <- plot1 + geom_bar() + facet_grid(~ Report_Year)
      plot1
      } 
    }
    
    else {
      validate("Error: Variable not of class type numeric or factor")
    }
  }) # End of rendPlot function
  
  output$table1 <- renderTable({
    if(!is.numeric(energy[[input$uni_var]])){
      tribble(~"data", "Variable is not numeric")
    } else if (input$uni_log == TRUE){
      t.test(log(energy_uni()[[input$uni_var]]),
             mu = input$uni_null,conf.level = 0.95) %>%
        tidy() -> temp
      tribble(~"P-value", ~"Estimate", ~"95% Lower", ~"95% Upper",
              temp$p.value[[1]], temp$estimate[[1]], temp$conf.low[[1]],
              temp$conf.high[[1]])
    } else {
      t.test(energy_uni()[[input$uni_var]], mu = input$uni_null,conf.level = 0.95)%>%
        tidy() -> temp 
      tribble(~"P-value", ~"Estimate", ~"95% Lower", ~"95% Upper",
              temp$p.value[[1]], temp$estimate[[1]], temp$conf.low[[1]],
              temp$conf.high[[1]])
    }
  }) #End of renderTable function
  
  energy_bi <- reactive(
    
    energy %>%
      filter(Report_Year %in% input$bi_year),
  )
  
  output$plot2 <- renderPlot({
    plot2 <- ggplot(energy_bi(), aes(x = !!input$bi_x, y = !!input$bi_y))
    
    if ((is.numeric(energy[[input$bi_x]]) & is.numeric(energy[[input$bi_y]]))) {
      if (input$bi_x_log == TRUE) {
           if (input$bi_OLS == TRUE) {
               plot2 <- plot2 + geom_point(aes(color = Report_Year)) + scale_x_log10() + geom_smooth(method = "lm", se = FALSE)
                plot2
      
         } else 
             if (input$bi_OLS == TRUE) {
             plot2 <- plot2 + geom_point(aes(color = Report_Year)) + scale_x_log10()
            plot2
          
        } else {
          plot2 <- plot2 + geom_point(aes(color = Report_Year)) + scale_x_log10()
          plot2
        }
          
      } else if (input$bi_y_log == TRUE) {
        if (input$bi_OLS == TRUE) {
          plot2 <- plot2 + geom_point(aes(color = Report_Year)) + scale_y_log10() + geom_smooth(method = "lm", se = FALSE)
          plot2
        } else {
          plot2 <- plot2 + geom_point(aes(color = Report_Year)) + scale_y_log10()
          plot2
        }
      } else if (input$bi_x_log == TRUE & input$bi_y_log == TRUE) {
          if (input$bi_OLS == TRUE) {
            plot2 <- plot2 + geom_point(aes(color = Report_Year)) + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm", se = FALSE)
            plot2
          } else {
            plot2 <- plot2 + geom_point(aes(color = Report_Year)) + geom_smooth(method = "lm", se = FALSE)
            plot2
          }
        
      } else {
        if (input$bi_OLS == TRUE) {
          plot2 <- plot2 + geom_point(aes(color = Report_Year)) + geom_smooth(method = "lm", se = FALSE)
          plot2
        } else {
          plot2 <- plot2 + geom_point(aes(color = Report_Year))
          plot2
        }
      }
      
    } else if (is.factor(energy[[input$bi_x]]) & is.numeric(energy[[input$bi_y]])) {
      
      if (input$bi_x_log == TRUE) {
        validate("Error: Cannot log a variable of class factor")
     
     } else if (input$bi_y_log == TRUE) {
       plot2 <- plot2 + geom_boxplot(aes(color = Report_Year)) + scale_y_log10() + coord_flip()
       plot2
      
     } else {
       plot2 <- plot2 + geom_boxplot(aes(color = Report_Year)) + coord_flip()
       plot2
     }
    
  } else if ( is.numeric(energy[[input$bi_x]]) & is.factor(energy[[input$bi_y]])) {
      
      if (input$bi_x_log == TRUE) {
        plot2 <- plot2 + geom_boxplot(aes(color = Report_Year)) + scale_x_log10() + coord_flip()
        plot2
      
      } else if (input$bi_y_log == TRUE) {
        validate("Error: Cannot log a variable of class factor") 
      
      } else {
        plot2 <- plot2 + geom_boxplot(aes(color = Report_Year)) + coord_flip()
        plot2
      }
    
  } else if (is.character(energy[[input$bi_x]]) & is.numeric(energy[[input$bi_y]])) {
    validate("Please Choose Numeric or Factor Variable for X")


  } else if (is.numeric(energy[[input$bi_x]]) & is.character(energy[[input$bi_y]])){
    validate("Please Choose Numeric or Factor Variable for Y ")

  } else if (is.factor(energy[[input$bi_x]]) & is.character(energy[[input$bi_y]])) {
    validate("Please Choose Numeric or Factor Variable for Y")

  } else if (is.character(energy[[input$bi_x]]) & is.factor(energy[[input$bi_y]])) {
    validate("Please Choose Numeric or Factor Variable for X")
    
  } else {
    plot2 <- plot2 + geom_jitter(aes(color = Report_Year)) + coord_flip()
    plot2
  }
  }) # End of Render function for plot2
  
  output$dynamic <- renderDataTable({  # Start of tab3 output
    if (input$num_fac == TRUE) {
      energy %>%
        keep(~ (is.numeric(.) | is.factor(.)))
    } else {
        energy
    }
  },
  options = list(pageLength = 20))  #End render function for tab3,
} # End server function

# Run the application 
shinyApp(ui = ui, server = server)
