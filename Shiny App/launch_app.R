
# Found at https://github.com/goodekat/redres/blob/master/R/launch_app.R

#' Shiny App for 'redressing' linear mixed models
#'
#' @description
#' This is a shiny app for model diagnostic of mixed model using \code{lmer} function from lme4 package.
#' This app includes a residual plot computed from six types of residuals (conditional raw, Pearson, and
#' studentized, marginal raw, Pearson, and studentized) and normal quantile plot using of random effect
#' and error term. This app can also be used to do model selection through pairwise comparison
#' two models from different linear mixed models using lmer function.
#'
#' @param model A model (or two models wrapped in a list) fit using \code{lmer}.
#'
#' @return A Shiny app with multiple tabs showing diagnostic plots.
#'
#' @usage launch_redres(model)
#'
#' @importFrom cowplot plot_grid
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom ggplot2 ggtitle labs
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @export launch_redres
#'
#' @examples
#' \dontrun{
#' # fits a linear mixed effects model
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' launch_redres(model = fm1)

#' # comparing two different linear mixed effects models
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy)
#' cmbd <- c(fm1,fm2)
#' launch_redres(model = cmbd)
#' }


# Function for running shiny app
launch_redres <- function(model) {

  # create the app
  redresApp <- create_app(model)

  # run the app
  shiny::runApp(redresApp, display.mode = "normal")

}

# Function to create the app
create_app <- function(model){

  # error checks for input model(s)
  if (length(model) == 1){
    checkmate::expect_class(model, "lmerMod",
                            info = "The input model is not accepted by redres. Model must be fit using 'lmer'.")
  } else if (length(model) == 2){
    checkmate::expect_class(model[[1]], "lmerMod",
                            info = "The first input model is not accepted by redres. Model must be fit using 'lmer'.")
    checkmate::expect_class(model[[2]], "lmerMod",
                            info = "The second input model is not accepted by redres. Model must be fit using 'lmer'.")
  } else {
    stop("launch_redres currently only accepts 1 or 2 models.")
  }

  # create ui and server with the input model
  ui <- create_ui(model)
  server <- create_server(model)

  # create the app
  redresApp <- shinyApp(ui, server)

  # return the app
  return(redresApp)

}

# function to create the app ui given the model
create_ui <- function(model){

  navbarPage(
    title = "redres app",
    windowTitle = "redres app",
    theme = "yeti",

    tabPanel("Overview",
             div(tags$header(p("Diagnostic Plots for Linear Mixed-effects Model",
                               style="font-size:40px")),
                 align = "center",
                 style="color:#ffffff; background-color: #4d728d"),

             fluidRow(column(width = 6,
                             h3("App Overview"),
                             if (length(model) == 1) {
                               p("The redres Shiny app can be used to interactively view
                               diagnostic plots for linear mixed models. This tab includes a table
                               of the data and the model input to launch_redres. The other two
                               tabs contain residual and quantile plots of the model.")
                             } else {
                               p("The redres Shiny app can be used to interactively view
                               diagnostic plots for linear mixed models. This tab includes a table
                                 of the data and the models input to launch_redres. The other two
                                 tabs contain residual and quantile plots of the models.")
                             }
                             ,
                             h3("Input Data"),
                             if (length(model) == 1) {
                               p("The data used to fit the model are included in the table below.
                                 The columns can be sorted by clicking on the variable name, and
                                 the search box can be used to filter the data.")
                             } else {
                               p("The datasets used to fit the two models have been joined (if they
                                 contain different variables). These are included in the table below.
                                 The columns can be sorted by clicking on the variable name, and
                                 the search box can be used to filter the data.")
                             },
                             div(style = 'overflow-x: scroll', DT::dataTableOutput("view"))),
                      column(width = 6,
                             h3("Model 1"),
                             if (length(model) != 1) {
                               p("A printout of the call of the model input to launch_redres is shown below.")
                             } else {
                               p("A printout of the call of the first model input to launch_redres is shown below.")
                             }
                             ,
                             verbatimTextOutput("model1"),
                             if (length(model) != 1) h3("Model 2"),
                             if (length(model) != 1) p("A printout of the call of the second model input to launch_redres is shown below."),
                             if (length(model) != 1) verbatimTextOutput("model2")
                             ))),

    tabPanel("Residual Plot",
               sidebarPanel(
                 selectInput(inputId = "residual_type",
                             label = "Residual type",
                             choices = c("raw_cond", "raw_mar",
                                         "pearson_cond", "pearson_mar",
                                         "std_cond", "std_mar"),
                             selected = "raw_cond"),
                 if (length(model) == 1){
                   selectInput(inputId = "xvar",
                               label = "X-axis variable",
                               choices = c("Fitted values", names(model@frame)))
                 } else {
                   selectInput(inputId = "xvar1",
                               label = "Model 1 x-axis variable",
                               choices = c("Fitted values", names(model[[1]]@frame)))
                 },
                 if (length(model) != 1){
                   selectInput(inputId = "xvar2",
                               label = "Model 2 x-axis variable",
                               choices = c("Fitted values", names(model[[2]]@frame)))
                 }
                 ),
               mainPanel(plotOutput("resid"))
             ),

    tabPanel("Quantile Plots",
               mainPanel(
                 tabsetPanel(
                   tabPanel("Random Effects Quantile Plot",
                            plotOutput("rand_eff_quantile")),
                   tabPanel("Residual Quantile Plot",
                            plotOutput("quantile"))
                 ))
             )
  )

}

# function to create the app server given the model
create_server <- function(model){

  shiny::shinyServer(function(input, output) {

    output$view <- DT::renderDataTable(
      if (length(model) == 1){
        model@frame
      } else {
        dplyr::full_join(model[[1]]@frame, model[[2]]@frame)
      },
      options = list(pageLength = 10)
    )

    # Print the model
    output$model1 <- renderPrint(
      if (length(model) == 1) {
        model@call
      } else {
        model[[1]]@call
      }
    )

    # Print the second model if two models are input
    if (length(model) == 2) {
      output$model2 <- renderPrint(model[[2]]@call)
    }

    output$resid <- renderPlot({

      if (length(model) == 1){
        if(input$xvar != "Fitted values"){
          plot_redres(model, input$residual_type, xvar = input$xvar) +
            xlab(input$xvar)
        }
        else{
          plot_redres(model, input$residual_type) +
            xlab("Fitted values")
        }
      }
      else{
        if(input$xvar1 != "Fitted values"){
          m1_resid <- plot_redres(model[[1]], input$residual_type, input$xvar1) +
            xlab(input$xvar1) +
            ggtitle("Model 1")
        }
        else{
          m1_resid <- plot_redres(model[[1]], input$residual_type) +
            xlab("Fitted values") +
            ggtitle("Model 1")
        }

        if (input$xvar2 != "Fitted values"){
          m2_resid <- plot_redres(model[[2]], input$residual_type, input$xvar2) +
            xlab(input$xvar2) +
            ggtitle("Model 2")
        }
        else{
          m2_resid <- plot_redres(model[[2]], input$residual_type) +
            xlab(input$xvar2) +
            ggtitle("Model 2")
        }
        plot_grid(m1_resid, m2_resid)
      }
    })

    output$rand_eff_quantile <- renderPlot({
      if (length(model) == 1){
        plot_ranef(model)
      } else {
        m1_rand_quant <- plot_ranef(model[[1]]) + labs(tag = "Model 1")
        m2_rand_quant <- plot_ranef(model[[2]]) + labs(tag = "Model 2")
        plot_grid(m1_rand_quant, m2_rand_quant, nrow = 2)
      }
    })

    output$quantile <- renderPlot({
      if (length(model) == 1){
        plot_resqq(model)
      } else {
        m1_qq <- plot_resqq(model[[1]]) + xlab("Model 1")
        m2_qq <- plot_resqq(model[[2]]) + xlab("Model 2")
        plot_grid(m1_qq, m2_qq)
      }
    })
  })

}

