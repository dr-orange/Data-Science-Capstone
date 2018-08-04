#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tibble)

# options(warn=0)

predictModelFilePath <- file.path(".", "predictModelForApp.rda")

if (!file.exists(predictModelFilePath)) {
        # nothing
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

normalize <- function(word, size = 0) {
        if (is.null(word)) {
                NULL
        } else if (nrow(word) == 0) {
                NULL
        } else if (size != 0) {
                word %>% slice(1:size)
        } else {
                word
        }
}

# Define server logic required to draw a map
shinyServer(function(input, output) {
        dataInput <- reactive({
                ngram <- str_trim(input$ngram, side = "both")
                fastNextWords(ngram, predictModel, outputs = 0)
        })
        output$nextWordBtn <- renderUI({
                nextWords <- normalize(dataInput(), 3)
                listBtn <-
                        list(span("> ", style = "font-size: 1.4em", inline = TRUE))
                if (length(nextWords$prediction) > 0) {
                        for (i in 1:length(nextWords$prediction)) {
                                listBtn <-
                                        list(
                                                listBtn,
                                                actionButton(
                                                        paste0("button_", i),
                                                        nextWords$prediction[i],
                                                        style = "font-size: 1.4em"
                                                )
                                        )
                                # onclick(paste0("button_", i), js$updateInput(input$ngram, i))
                        }
                }
                tagList(listBtn)
        })
        
        # This is not a smart way, but it was obstructed by the closure and it had to do this way.
        onclick("button_1", js$updateInput(input$ngram, "1"))
        onclick("button_2", js$updateInput(input$ngram, "2"))
        onclick("button_3", js$updateInput(input$ngram, "3"))

        output$distPlot <- renderPlot({
                # plot next words
                nextWords <- normalize(dataInput(), 3)
                if (length(nextWords) > 0) {
                        ggplot(nextWords,
                               aes(
                                       x = reorder(prediction, -p_bo),
                                       y = p_bo
                               )) +
                                geom_bar(stat = "identity", fill = "orangered") +
                                theme(axis.text.x = element_text(size =
                                                                         25)) +
                                xlab("Predicted next word [Top 3]") + ylab("P_bo")
                } else {
                        
                }
        })
        output$wordCloudPlot <- renderPlot({
                nextWords <- normalize(dataInput())
                if (length(nextWords) > 0) {
                        wordcloud(
                                nextWords[, 3],
                                nextWords[, 5],
                                scale = c(3:.5),
                                min.freq = 1,
                                max.words = 50,
                                random.order = FALSE,
                                rot.per = .25,
                                colors = brewer.pal(8, "Dark2")
                        )
                } else {
                        
                }
        })
        output$sentimentPlot <- renderPlot({
                nextWords <- normalize(dataInput())
                if (length(nextWords) > 0) {
                        emo <- nextWords %>%
                                left_join(get_sentiments("bing"),
                                          by = c("prediction" = "word")) %>%
                                mutate(
                                        sentiment = ifelse(is.na(sentiment), "na", sentiment),
                                        Positive = ifelse(sentiment == "positive", frequency, 0),
                                        Negative = ifelse(sentiment == "negative", frequency, 0),
                                        Neutral = ifelse(sentiment == "na", frequency, 0)
                                ) %>%
                                column_to_rownames("prediction") %>%
                                select(Positive, Negative, Neutral)
                        
                        if (sum(emo$Positive) * sum(emo$Negative) * sum(emo$Neutral) > 0) {
                                emo %>%
                                        comparison.cloud(
                                                colors = brewer.pal(8, "Dark2"),
                                                random.order = FALSE,
                                                scale = c(3:2),
                                                rot.per = .25,
                                                max.words = 50
                                        )
                        } else {
                                
                        }
                        
                }
        })
})
