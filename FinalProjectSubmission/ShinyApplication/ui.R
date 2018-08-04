#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

jsCode <- '
shinyjs.updateInput = function(params) {
 var defaultParams = {
  val : "",
  btn : "1"
 };
 params = shinyjs.getParams(params, defaultParams);
 $("#ngram").val(params.val + $("#button_" + params.btn).text()).focus();
}'

# Define UI for application that draws a map
shinyUI(fluidPage(
        useShinyjs(),
        extendShinyjs(text = jsCode, functions = c("updateInput")),
        # Application title
        titlePanel("Predict Next Words"),
        # Show input form
        fluidRow(column(12,
                        wellPanel(
                                # Show predicted next words
                                uiOutput("nextWordBtn"),
                                textInput("ngram",
                                          "",
                                          value = "What a ")
                        ))),

        # Show predicted plot
        tabsetPanel(
                type = "tabs",
                tabPanel("Bar Plot",
                         plotOutput("distPlot", height = "650px")),
                tabPanel("Word Cloud",
                         plotOutput("wordCloudPlot", height = "650px")),
                tabPanel("Sentiment Word Cloud",
                         plotOutput("sentimentPlot", height = "650px"))
        ),
        
        # Appendix
        h3("Appendix"),
        p(
                tags$span("server.R and ui.R code on "),
                tags$a(href = "https://github.com/dr-orange/Data-Science-Capstone/tree/master/FinalProjectSubmission/ShinyApplication", "GitHub")
        ),
        p(
                tags$span("Overview on "),
                tags$a(href = "http://rpubs.com/dr_orange_jr/406377", "RPubs")
        )
        
))
