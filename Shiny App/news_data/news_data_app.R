#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list_of_vars_n <- c("shares", "n_tokens_title", "n_tokens_content", 
                  "n_unique_tokens", "num_hrefs", "num_imgs", 
                  "num_videos", "average_token_length")

n_data_choices <- c("No. of shares" =  "shares",
                    "No. of words in title" = "n_tokens_title",
                    "No. of words in text" = "n_tokens_content",
                    "No. of unique words in text" = "n_unique_tokens",
                    "No. of links" = "num_hrefs",
                    "No. of images" = "num_imgs",
                    "No. of videos" = "num_videos",
                    "Average word length" = "average_token_length")



# list_of_vars_e <- c("Appliances", "lights", "total_energy",
#                     "T_overall", "H_overall", "T_bedroom", 
#                     "H_bedroom", "T_shared", "H_shared", 
#                     "T_diff", "H_diff")



e_data_y <- c("total_energy", "Appliances", "lights")
e_data_x <- c("T_overall", "H_overall", "T_bedroom", "H_bedroom", 
              "T_shared", "H_shared", "T_diff", "H_diff")

e_data_x_choices <- c("Average indoor temperature" = "T_overall",
                      "Average indoor humidity" = "H_overall",
                      "Average bedroom temperature" = "T_bedroom",
                      "Average bedroom humidity" = "H_bedroom",
                      "Average temperature in shared rooms" = "T_shared",
                      "Average humidity in shared rooms" = "H_shared",
                      "Difference between indoor and outdoor temp." = "T_diff",
                      "Difference between indoor and outdoor humidity" = "H_diff")

e_data_y_choices <- c("Total energy used" = "total_energy",
                      "Energy used by appliances" = "Appliances",
                      "Energy used by lights" = "lights")





library(shiny)
library(shinyjs)
library(rhandsontable)
library(shinycssloaders)
library(shinyWidgets)


# library(ggplot2)
# library(broom)
# library(dplyr)
# library(ggthemes)
# library(latex2exp)
# library(gridExtra)
# library(tidyverse)

# library(shinythemes)


# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    h1("Model Diagnostics"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "y_var",
                    label = "Y Variable:",
                    choices = e_data_y_choices),

        selectInput(inputId = "x_var",
                    label = "X Variable:",
                    choices = e_data_x_choices),

        # selectInput(inputId = "y_var",
        #             label = "Y Variable:",
        #             choices = n_data_choices),
        # selectInput(inputId = "x_var",
        #             label = "X Variable:",
        #             choices = n_data_choices),
        
        sliderInput(inputId = "sample_size",
                    label = "Select sample size:",
                    min = 500,
                    max = 5000, 
                    value = 1000),

        # selectInput("sample_size", "Select Sample Size:",
        #             choices = c(500, 1000, 2500, 5000),
        #             selected = 1000),
        
        radioButtons("plot_type", 
                     label = h4("Type of diagnostic plot"),
                     c("Residuals vs. Fitted values" = "resid.fitted",
                       "Scale-Location" = "scale.location",
                       "Normal Q-Q plot" = "qq"),
                     selected = "resid.fitted"),
        # 
        radioButtons(inputId = "plot_options",
                     label = "Plot Options (for the Residuals vs. Fitted and Scale-Location plots only):",
                     choices = c("Show red line" = "wRL",
                                 "Show confidence interval" = "wCI",
                                 "Show data only" = "woRL")),
        
        actionButton("resample", "Generate new sample")
  ),
  mainPanel(fluidRow(
    splitLayout(cellWidths = c("50%", "50%"),
                withSpinner(plotOutput("fittedLine")),
                withSpinner(plotOutput("origPlot")))
    )
    )
  )
  )

  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # ENERGY DATA (CHANGE PATH HERE)
  data <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/energy_data_cleaned.csv")
  
  # NEWS DATA
  # data <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/news_data_cleaned.csv")
  

  reg_data <- reactive({
    if (input$resample >= 0) {
      data_sample <- sample_n(data, as.numeric(input$sample_size))
      return(data_sample[, c(input$x_var, input$y_var)])
    }
  })
  
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
  })
  
  # output$fittedEqn <- renderTable({
  #   mod() %>%
  #     tidy() %>%
  #     select(Term, Estimate)
  # })
  
  output$fittedLine <- renderPlot({
    reg_data() %>%
      ggplot(aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(shape = 1) +
      labs(title = "Data Fit",
           x = names(e_data_x_choices[which(e_data_x_choices == input$x_var)]),
           y = names(e_data_y_choices[which(e_data_y_choices == input$y_var)])) +
      geom_smooth(method = "lm", se = FALSE, col="blue")
  })
  
  output$origPlot <- renderPlot({
    obsData <- augment(mod())
    dataPlot <- switch(
      input$plot_type,
      resid.fitted = switch(input$plot_options,
                            wRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
                              geom_hline(yintercept = 0, linetype = 2, color = "black") +
                              geom_point(shape = 1) +
                              geom_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5) +
                              labs(title = "Residuals vs. Fitted",
                                   x = "Fitted values",
                                   y = "Residuals"),
                            wCI = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
                              geom_hline(yintercept = 0, linetype = 2, color = "black") +
                              geom_point(shape = 1) +
                              geom_smooth(col="red", method = "loess", se=!FALSE, linewidth = 0.5) +
                              labs(title = "Residuals vs. Fitted",
                                   x = "Fitted values",
                                   y = "Residuals"),
                            woRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
                              geom_hline(yintercept = 0, linetype = 2, color = "black") +
                              geom_point(shape = 1) +
                              labs(title = "Residuals vs. Fitted",
                                   x = "Fitted values",
                                   y = "Residuals")
                            ),
      
      scale.location = switch(input$plot_options,
                              wRL = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                                geom_hline(yintercept = 0, linetype = 2, color = "black") +
                                geom_point(shape = 1) +
                                geom_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5) +
                                labs(title= "Scale-Location",
                                     x ="Fitted values",
                                     y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
                              wCI = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                                geom_hline(yintercept = 0, linetype = 2, color = "black") +
                                geom_point(shape = 1) +
                                geom_smooth(col="red", method = "loess", se=!FALSE, linewidth = 0.5) +
                                labs(title= "Scale-Location",
                                     x ="Fitted values",
                                     y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
                              woRL = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                                geom_hline(yintercept = 0, linetype = 2, color = "black") +
                                geom_point(shape = 1) +
                                labs(title= "Scale-Location",
                                     x ="Fitted values",
                                     y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
                              ),
                       
                       qq = obsData %>%
                         ggplot(aes(sample = .std.resid)) +
                         geom_qq_line() +
                         geom_qq() +
                         labs(title = "Normal Q-Q Plot", x = "N(0, 1) quantiles", y = "Standardized residuals"),
                       dataPlot)
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)