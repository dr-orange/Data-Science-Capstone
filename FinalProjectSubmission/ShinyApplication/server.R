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
                
                nextWordDt <-
                        predictModel$ngramsDt[base == preTriGram &
                                                      ngramsize == 3]
        } else {
                nextWordDt <- data.table()
        }
        preBiGram <- inputs[inputsSize]
        
        # extract n-gram that starts with input
        featuresNextWord <- NULL
        
        if (nrow(nextWordDt) > k) {
                prevWordDt <-
                        predictModel$ngramsDt[ngram == preTriGram &
                                                      ngramsize == 2]
                
                featuresNextWord <-
                        setorderv(nextWordDt[, p_bo := as.vector(
                                predictModel$SGT.dTrigram(frequency) * frequency / prevWordDt$frequency
                        )],
                        c("p_bo", "prediction"), c(-1, 1))
        } else {
                nextWordDt <-
                        predictModel$ngramsDt[base == preBiGram & ngramsize == 2]
                
                if (nrow(nextWordDt) > k) {
                        prevWordDt <-
                                predictModel$ngramsDt[ngram == preBiGram & ngramsize == 1]
                        
                        featuresNextWord <-
                                nextWordDt[, p_bo := as.vector(
                                        predictModel$SGT.dBigram(frequency) * frequency / prevWordDt$frequency
                                )]
                        
                        alpha <- 1 / sum(featuresNextWord$p_bo)
                        featuresNextWord <-
                                setorderv(featuresNextWord[, p_bo := alpha * p_bo],
                                          c("p_bo", "prediction"),
                                          c(-1, 1))
                } else {
                        nextWordDt <- predictModel$ngramsDt[ngramsize == 1]
                        
                        featuresNextWord <-
                                nextWordDt[, p_bo := as.vector(
                                        predictModel$SGT.dUnigram(frequency) * frequency / sum(nextWordDt$frequency)
                                )]
                        
                        alpha <- 1 / sum(featuresNextWord$p_bo)
                        featuresNextWord <-
                                setorderv(featuresNextWord[, p_bo := alpha * p_bo],
                                          c("p_bo", "prediction"),
                                          c(-1, 1))
                }
        }
        
        if (outputs > 0) {
                featuresNextWord %>% slice(1:outputs)
        } else {
                featuresNextWord
        }
}

nearestWord <- function(input) {
        
}

prevWords <- function(input) {
        inputs <- str_split(input, "\\s+")[[1]]
        prevInput <- paste(inputs[-length(inputs)], collapse = " ")
        nowWord <- inputs[length(inputs)]
        
        list(nowWord = nowWord, prevInput = prevInput)
}

fastNowWords <- function(input,
                         predictModel,
                         outputs = 0,
                         k = 0) {
        prevInput <- prevWords(input)$prevInput
        nowWord <- prevWords(input)$nowWord
        
        predictWord <-
                fastNextWords(tolower(prevInput), predictModel, outputs = 0) %>%
                filter(str_detect(prediction, paste0("^", tolower(nowWord))))
        
        if (outputs > 0) {
                predictWord %>% slice(1:outputs)
        } else {
                predictWord
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

# emotionOfSentence <- function(sentence) {
#         word <- str_split(tolower(sentence), "\\s+")[[1]]
#         data.frame(word = word, frequency = 1) %>%
#                 left_join(get_sentiments("bing")) %>%
#                 mutate(
#                         sentiment = ifelse(is.na(sentiment), "na", sentiment),
#                         Positive = ifelse(sentiment == "positive", frequency, 0),
#                         Negative = ifelse(sentiment == "negative", frequency, 0),
#                         Neutral = ifelse(sentiment == "na", frequency, 0)
#                 ) %>%
#                 select(word, Positive, Negative, Neutral)
#         # not effective?
# }

# Define server logic required to draw a map
shinyServer(function(input, output, session) {
        hide("loading_page")
        
        dataInput <- reactive({
                ngram <- str_trim(input$ngram, side = "both")
                fastNextWords(ngram, predictModel, outputs = 0)
        })
        
        dataInput2 <- reactive({
                ngram <- input$ngram
                if (length(grep("\\s$", ngram)) == 0) {
                        fastNowWords(ngram, predictModel, outputs = 3)
                } else {
                        NULL
                }
        })
        
        output$nextWordBtn <- renderUI({
                nextWords <- normalize(dataInput(), 3)
                nowWords <- normalize(dataInput2(), 3)
                listBtn <-
                        list(span("> ", style = "font-size: 1.4em", inline = TRUE))
                if (length(nowWords) > 0) {
                        # Autocomplete
                        for (i in 1:length(nowWords$prediction)) {
                                listBtn <-
                                        list(
                                                listBtn,
                                                actionButton(
                                                        paste0("button_n", i),
                                                        nowWords$prediction[i],
                                                        style = "font-size: 1.4em"
                                                )
                                        )
                        }
                } else if (length(nextWords$prediction) > 0) {
                        # Prediction
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
                } else {
                        # Autocorrect
                        # adist() or library(stringdist)
                        # https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf
                }
                tagList(listBtn)
        })
        
        # This is not a smart way, but it was obstructed by the closure and it had to do this way.
        onclick("button_n1",
                js$updateInput(prevWords(input$ngram)$prevInput, "n1", " "))
        onclick("button_n2",
                js$updateInput(prevWords(input$ngram)$prevInput, "n2", " "))
        onclick("button_n3",
                js$updateInput(prevWords(input$ngram)$prevInput, "n3", " "))
        onclick("button_1", js$updateInput(input$ngram, "1"))
        onclick("button_2", js$updateInput(input$ngram, "2"))
        onclick("button_3", js$updateInput(input$ngram, "3"))
        onclick("clear", js$clearInput())
        
        output$distPlot <- renderPlot({
                # plot next words
                nextWords <- normalize(dataInput(), 3)
                nowWords <- normalize(dataInput2(), 3)
                if (length(nowWords) > 0) {
                        ggplot(nowWords,
                               aes(
                                       x = reorder(prediction,-p_bo),
                                       y = p_bo
                               )) +
                                geom_bar(stat = "identity", fill = "limegreen") +
                                theme(axis.text.x = element_text(size =
                                                                         25)) +
                                xlab("Predicted next word [Top 3]") + ylab("P_bo")
                } else if (length(nextWords) > 0) {
                        ggplot(nextWords,
                               aes(
                                       x = reorder(prediction,-p_bo),
                                       y = p_bo
                               )) +
                                geom_bar(stat = "identity", fill = "orangered") +
                                theme(axis.text.x = element_text(size =
                                                                         25)) +
                                xlab("Predicted next word [Top 3]") + ylab("P_bo")
                } else {
                        
                }
        })
        
        ## --------------------------------------------------------------------------
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
