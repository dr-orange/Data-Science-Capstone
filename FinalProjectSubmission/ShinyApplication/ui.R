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
        $("#ngram").val(
                params.val
                + ((params.val != "")?params.sep:"")
                + $("#button_" + params.btn).text()
                + " "
        ).focus().change();
};
shinyjs.clearInput = function() {
        $("#ngram").val("").focus().change();
};
shinyjs.init = function() {
        $(window).keydown(function(event) { // keypress() does not work for TAB key
                if ( event.which == 9 ) { // TAB code
                        event.preventDefault();
                        if ($("#button_n1").val() !== undefined) {
                                $("#button_n1").click();
                        } else if ($("#button_1").val() !== undefined) {
                                $("#button_1").click();
                        }
                }
        })
};
'

# Define UI for application that draws a map
shinyUI(fluidPage(
        useShinyjs(),
        extendShinyjs(text = jsCode, functions = c("updateInput", "clearInput", "init")),
        # Application title
        titlePanel("Smart Keyboard"),
        # Show input form
        fluidRow(column(8,
                        wellPanel(
                                # Show predicted next words
                                div(id = "loading_page", h1("Loading... (about 10sec)", style = "color: #33aa00")),
                                uiOutput("nextWordBtn"),
                                div(textInput("ngram",
                                          "",
                                          value = "What a "), style="display: inline-block;vertical-align:center;width:90%"),
                                div(actionButton("clear", intToUtf8(as.integer("0x2573"))), style="display: inline-block;vertical-align:center;")
                        )),
                 column(4,
                        wellPanel(
                                span("- Prediction"),br(),
                                span("- Autocomplete"),br(),
                                span("- Autocorrect"),br(),
                                span("- Applicable by tapping word"),br(),
                                span("- Or by pressing TAB key"),br(),
                                style = "background: #33aa00; color: white"
                        ))),
        hr(),
        h3("Inside of the prediction model", style = "color: grey"),
        # Show predicted plot
        tabsetPanel(
                type = "tabs",
                tabPanel("Bar Plot",
                         span(intToUtf8(as.integer("0x25A0")), style = "color: orangered"),
                         span("Prediction.", style = "color: grey"),
                         span(intToUtf8(as.integer("0x25A0")), style = "color: limegreen"),
                         span("Autocomplete.", style = "color: grey"),
                         span(intToUtf8(as.integer("0x25A0")), style = "color: royalblue"),
                         span("Autocorrect.", style = "color: grey"),
                         plotOutput("distPlot", height = "400px")) #,
#                tabPanel("Word Cloud",
#                         plotOutput("wordCloudPlot", height = "650px")),
#                tabPanel("Sentiment Word Cloud",
#                         p("The aim is to quantify the emotions of the input words and weighted the predicted next word. However, it is not done", style = "color: grey"),
#                         plotOutput("sentimentPlot", height = "650px"))
        ),
        
        # Appendix
        h3("Appendix", style = "color: grey"),
        p(
                tags$span("server.R and ui.R code on "),
                tags$a(href = "https://github.com/dr-orange/Data-Science-Capstone/tree/master/FinalProjectSubmission/ShinyApplication", "GitHub")
        ),
        p(
                tags$span("Overview on "),
                tags$a(href = "http://rpubs.com/dr_orange_jr/SmartKey", "RPubs")
        )
        
))
