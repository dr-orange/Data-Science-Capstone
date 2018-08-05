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
                btn : "1",
                sep : "",
        };
        params = shinyjs.getParams(params, defaultParams);
        console.log(params)
        $("#ngram").val(
                params.val
                + ((params.val != "")?params.sep:"")
                + $("#button_" + params.btn).text()
                + " "
        ).focus().change();
}'

# Define UI for application that draws a map
shinyUI(fluidPage(
        useShinyjs(),
        extendShinyjs(text = jsCode, functions = c("updateInput")),
        # Application title
        titlePanel("Smart Keyboard"),
        # Show input form
        fluidRow(column(9,
                        wellPanel(
                                # Show predicted next words
                                uiOutput("nextWordBtn"),
                                textInput("ngram",
                                          "",
                                          value = "What a ")
                        )),
                 column(3,
                        wellPanel(
                                h4("Features: "),
                                span("- Prediction."),br(),
                                span("- Autocomplete."),br(),
                                span("- Autocorrect."),br(),
                                style = "background: grey; color: white"
                        ))),
        hr(),
        h3("Inside of the prediction model", style = "color: grey"),
        # Show predicted plot
        tabsetPanel(
                type = "tabs",
                tabPanel("Bar Plot",
                         span(intToUtf8(as.integer("0x25A0")), style = "color: orangered"),
                         span("Predictedion.", style = "color: grey"),
                         span(intToUtf8(as.integer("0x25A0")), style = "color: limegreen"),
                         span("Autocomplete.", style = "color: grey"),
                         span(intToUtf8(as.integer("0x25A0")), style = "color: royalblue"),
                         span("Autocorrect.", style = "color: grey"),
                         plotOutput("distPlot", height = "400px")),
                tabPanel("Word Cloud",
                         plotOutput("wordCloudPlot", height = "650px")),
                tabPanel("Sentiment Word Cloud",
                         p("The aim is to quantify the emotions of the input words and weighted the predicted next word. However, it is not done", style = "color: grey"),
                         plotOutput("sentimentPlot", height = "650px"))
        ),
        
        # Appendix
        h3("Appendix", style = "color: grey"),
        p(
                tags$span("server.R and ui.R code on "),
                tags$a(href = "https://github.com/dr-orange/Data-Science-Capstone/tree/master/FinalProjectSubmission/ShinyApplication", "GitHub")
        ),
        p(
                tags$span("Overview on "),
                tags$a(href = "http://rpubs.com/dr_orange_jr/406377", "RPubs")
        )
        
))
