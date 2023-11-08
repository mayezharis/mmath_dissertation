#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

news_data <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/OnlineNewsPopularity.csv")

list_of_vars <- c("shares", "n_tokens_title", "n_tokens_content", "n_unique_tokens", "num_hrefs", "num_imgs", "num_videos", "average_token_length")

news_data <- news_data %>%
  filter(n_tokens_content < 8474) %>% 
  filter(n_unique_tokens < 701) %>%
  select(all_of(list_of_vars))



library(shiny)
library(shinyjs)
library(rhandsontable)
library(shinycssloaders)
# library(shinythemes)

# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    h1("Model Diagnostics"),
    sidebarLayout(
      sidebarPanel(
        selectInput("y_var", "Y Variable:", c("No. of shares" =  "shares",
                                              "No. of words in title" = "n_tokens_title",
                                              "No. of words in text" = "n_tokens_content",
                                              "No. of unique words in text" = "n_unique_tokens",
                                              "No. of vinks" = "num_hrefs",
                                              "No. of vmages" = "num_imgs",
                                              "No. of videos" = "num_videos",
                                              "Average word length" = "average_token_length")),
        selectInput("x_var", "X Variable:", c("No. of shares" =  "shares",
                                              "No. of words in title" = "n_tokens_title",
                                              "No. of words in text" = "n_tokens_content",
                                              "No. of unique words in text" = "n_unique_tokens",
                                              "No. of vinks" = "num_hrefs",
                                              "No. of vmages" = "num_imgs",
                                              "No. of videos" = "num_videos",
                                              "Average word length" = "average_token_length")),
        radioButtons("plot", label = h4("Type of residual plot"),
                     c("Residuals vs. fitted values" = "resid.fitted",
                       "Residuals vs. x" = "resid.x",
                       "Normal Q-Q plot" = "qq"),
                     selected = "resid.fitted")
          ),
    mainPanel("main_panel",
              column(2,
                     fluidRow(
                       splitLayout(cellWidths = c("50%", "50%"), 
                                   withSpinner(plotOutput("fittedLine")), 
                                   withSpinner(plotOutput("origPlot"))
                                   )
                       )
                     )
              )
    )
    )

  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv <- reactiveValues(show = FALSE)
  
  reg_data <- reactive({news_data[, c(input$x_var, input$y_var)]
  })
  
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
  })
  
  output$fittedEqn <- renderTable({
    mod() %>%
      tidy() %>%
      select(Term, Estimate)
  })
  
  output$fittedLine <- renderPlot({
    reg_data() %>%
      ggplot(aes_string(input$x_var, input$y_var)) +
      geom_point(shape = 1) +
      geom_smooth(method = "lm", se = FALSE)
  })
  
  output$origPlot <- renderPlot({
    obsData <- augment(mod())
    dataPlot <- switch(input$plot,
                       resid.fitted = obsData %>%
                         ggplot() +
                         geom_hline(yintercept = 0, linetype = 2, color = "black") +
                         geom_point(aes(x = .fitted, y = .resid), shape = 1) +
                         labs(x = "Fitted values", y = "Residuals"),
                       resid.x = obsData %>%
                         ggplot() +
                         geom_hline(yintercept = 0, linetype = 2, color = "black") +
                         geom_point(aes_string(x = input$x_var, y = ".resid"), shape = 1) +
                         labs(x = input$x_var, y = "Residuals"),
                       qq = obsData %>%
                         ggplot(aes(sample = .std.resid)) +
                         geom_qq_line() +
                         geom_qq() +
                         labs(x = "N(0, 1) quantiles", y = "Standardized residuals")
    )
    dataPlot +
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
