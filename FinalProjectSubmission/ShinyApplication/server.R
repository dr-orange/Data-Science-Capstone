#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)

predictModelFilePath <- file.path(".", "predictModelForApp.rda")

if (!file.exists(predictModelFilePath)) {
        
} else {
        load(predictModelFilePath)
}

fastNextWords <- function(input,
                          predictModel,
                          outputs = 3,
                          k = 0) {
        # k is the least important of the parameters. It is usually chosen to be 0.
        # However, empirical testing may find better values for k.
        inputs <- str_split(tolower(input), "\\s+")[[1]]
        inputsSize <- length(inputs)
        if (inputsSize > 1) {
                preTriGram <- paste(inputs[inputsSize - 1],
                                    inputs[inputsSize],
                                    sep = "_")
                
                nextWordDt <- predictModel$ngramsDt %>%
                        filter(base == preTriGram, ngramsize == 3)
        } else {
                if (inputs == "") {
                        return()
                }
                nextWordDt <- NULL
        }
        preBiGram <- inputs[inputsSize]
        
        # extract n-gram that starts with input
        featuresNextWord <- NULL
        
        if (length(nextWordDt) > k) {
                prevWordDt <- predictModel$ngramsDt %>%
                        filter(ngram == preTriGram, ngramsize == 2)
                
                prevWordFreq <- prevWordDt$frequency
                
                # data frame
                featuresNextWord <-
                        nextWordDt %>%
                        mutate(p_bo = as.vector(
                                predictModel$SGT.dTrigram(frequency) * frequency / prevWordFreq
                        )) %>%
                        # Sort by reverse frequency order
                        arrange(-p_bo, prediction)
        } else {
                nextWordDt <- predictModel$ngramsDt %>%
                        filter(base == preBiGram, ngramsize == 2)
                
                if (length(nextWordDt) > k) {
                        prevWordDt <- predictModel$ngramsDt %>%
                                filter(ngram == preBiGram, ngramsize == 1)
                        
                        prevWordFreq <- prevWordDt$frequency
                        
                        # data frame
                        featuresNextWord <-
                                nextWordDt %>%
                                mutate(
                                        p_bo = as.vector(
                                                predictModel$SGT.dBigram(frequency) * frequency / prevWordFreq
                                        )
                                )
                        
                        alpha <- 1 / sum(featuresNextWord$p_bo)
                        featuresNextWord <- featuresNextWord %>%
                                mutate(p_bo = alpha * p_bo) %>%
                                # Sort by reverse frequency order
                                arrange(-p_bo, prediction)
                } else {
                        nextWordDt <- predictModel$unigram
                        prevWordFreq <- length(nextWordDfm)
                        
                        featuresNextWord <-
                                nextWordDt %>%
                                mutate(
                                        p_bo = as.vector(
                                                predictModel$SGT.dUnigram(frequency) * frequency / prevWordFreq
                                        )
                                )
                        
                        alpha <- 1 / sum(featuresNextWord$p_bo)
                        featuresNextWord <- featuresNextWord %>%
                                mutate(p_bo = alpha * p_bo) %>%
                                # Sort by reverse frequency order
                                arrange(-p_bo, prediction)
                        
                }
        }
        if (outputs > 0) {
                featuresNextWord %>% slice(1:outputs)
        } else {
                featuresNextWord
        }
}

# Define server logic required to draw a map
shinyServer(function(input, output) {
        dataInput <- reactive({
                ngram <- str_trim(input$ngram, side = "both")
                fastNextWords(ngram, predictModel)
        })
        output$nextWord <- renderText({
                nextWords <- dataInput()
                paste(nextWords$prediction, collapse = " | ")
        })
        output$distPlot <- renderPlot({
                # plot next words
                nextWords <- dataInput()
                if (length(nextWords) > 0) {
                        ggplot(nextWords,
                               aes(
                                       x = reorder(prediction,-p_bo),
                                       y = p_bo
                               )) +
                                geom_bar(stat = "identity", fill = "orangered") +
                                theme(axis.text.x = element_text(size=25)) +
                                xlab("Predicted next word [Top 3]") + ylab("P_bo")
                } else {
                        
                }
        })
})
