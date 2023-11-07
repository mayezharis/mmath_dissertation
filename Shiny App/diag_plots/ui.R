#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Model Diagnostics"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            checkboxInput(inputID = "which_plot",
                        label = "Which plot do you want to see?",
                        choices = c("Residuals vs Fitted", "Q-Q", "Scale-Location", "Residuals vs Leverage"))

        )
    )
)
