Sys.setlocale(category = "LC_TIME", locale = "C")

library(stringr)
library(dplyr)
library(quanteda)
library(readtext)
library(R.utils)
library(ggplot2)
library(data.table)

set.seed(3301)

downloadData <- function(workingDataPath = file.path("data")) {
        # Download Data
        rawDataFileUrl <-
                "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        downloadedZipfilePath <-
                file.path(workingDataPath, "Coursera-SwiftKey.zip")
        badWordsFileUrl <-
                "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
        badWordsFilePath <-
                file.path(workingDataPath, "bad-words.txt")
        basePath <- file.path(workingDataPath, "final", "en_US")
        
        # Create working directory
        if (!file.exists(workingDataPath)) {
                dir.create(workingDataPath)
        }
        # Download ziped file
        if (!file.exists(downloadedZipfilePath)) {
                download.file(rawDataFileUrl,
                              destfile = downloadedZipfilePath,
                              method = "curl")
        }
        # Download Bad Words Text File
        if (!file.exists(badWordsFilePath)) {
                download.file(badWordsFileUrl,
                              destfile = badWordsFilePath,
                              method = "curl")
        }
        # Unzip
        if (!file.exists(basePath)) {
                unzip(zipfile = downloadedZipfilePath, exdir = workingDataPath)
        }
        
        list(
                blogs = file.path(basePath, "en_US.blogs.txt"),
                twitter = file.path(basePath, "en_US.twitter.txt"),
                news = file.path(basePath, "en_US.news.txt"),
                badwords = badWordsFilePath
        )
}

subSample <- function(input, output) {
        if (!file.exists(output)) {
                subSamplingRate <-
                        .25 # 25% data. object size is >2Gb (Shinyapp.io limit 1Gb)
                fileLines <- as.numeric(countLines(input))
                flipABiasedCoin <-
                        rbinom(fileLines, size = 1, prob = subSamplingRate)
                conRead <- file(input, "r")
                conWrite <- file(output, "w")
                len <- 0
                while (length(oneLine <-
                              readLines(conRead, 1, warn = FALSE)) > 0) {
                        len <- len + 1
                        if (flipABiasedCoin[len] == 1) {
                                writeLines(oneLine, conWrite)
                        }
                }
                close(conRead)
                close(conWrite)
        }
        return(as.numeric(countLines(output)))
}

devideDataset <- function(input, outputTrain, outputTest) {
        if (!file.exists(outputTrain) || !file.exists(outputTest)) {
                trainRate <- .7
                fileLines <- as.numeric(countLines(input))
                flipABiasedCoin <-
                        rbinom(fileLines, size = 1, prob = trainRate)
                conRead <- file(input, "r")
                conWriteTrain <- file(outputTrain, "w")
                conWriteTest <- file(outputTest, "w")
                len <- 0
                while (length(oneLine <-
                              readLines(conRead, 1, warn = FALSE)) > 0) {
                        len <- len + 1
                        if (flipABiasedCoin[len] == 1) {
                                writeLines(oneLine, conWriteTrain)
                        } else {
                                writeLines(oneLine, conWriteTest)
                        }
                }
                close(conRead)
                close(conWriteTrain)
                close(conWriteTest)
        }
        return(as.numeric(countLines(outputTrain)))
}

simpleGoodTuring <- function(r, Nr, sd = 1.65) {
        # number of words
        N <- sum(r * Nr)
        d <- diff(r)
        
        ## Turing estimate
        # turing estimate index
        ti <- which(d == 1)
        # discount coefficients of Turing estimate
        dct <- numeric(length(r))
        dct[ti] <- (r[ti] + 1) / r[ti] * c(Nr[-1], 0)[ti] / Nr[ti]
        
        ## Linear Good-Turing estimate
        Zr <- Nr / c(1, 0.5 * (d[-1] + d[-length(d)]), d[length(d)])
        f <- lsfit(log(r), log(Zr))
        coef <- f$coef
        # corrected term frequency
        rc <- r * (1 + 1 / r) ^ (1 + coef[2])
        # discount coefficients of Linear Good-Turing estimate
        dclgt <- rc / r
        
        ## make switch from Turing to LGT estimates
        # standard deviation of term frequencies between 'r' and 'rc' (?)
        rsd <- rep(1, length(r))
        rsd[ti] <-
                (seq_len(length(r))[ti] + 1) / Nr[ti] * sqrt(Nr[ti + 1] * (1 + Nr[ti + 1] / Nr[ti]))
        
        dc <- dct
        for (i in 1:length(r)) {
                if (abs(dct[i] - dclgt[i]) * r[i] / rsd[i] <= sd) {
                        dc[i:length(dc)] <- dclgt[i:length(dc)]
                        break
                }
        }
        
        ## renormalize the probabilities for observed objects
        # summation of probabilities
        sump <- sum(dc * r * Nr) / N
        # renormalized discount coefficients
        dcr <- (1 - Nr[1] / N) * dc / sump
        
        # term frequency
        tf <- c(Nr[1] / N, r * dcr)
        p <- c(Nr[1] / N, r * dcr / N)
        names(p) <- names(tf) <- c(0, r)
        
        list(p = p, r = tf)
}

sgtFactory <- function(unigram, bigram, trigram) {
        NrTbl1 <- textstat_frequency(unigram) %>%
                select(frequency) %>%
                mutate(freqOfFrequency = 1) %>%
                group_by(frequency) %>%
                summarise_all(sum)
        
        SGT1 <-
                simpleGoodTuring(NrTbl1$frequency, NrTbl1$freqOfFrequency)
        
        NrTbl2 <- textstat_frequency(bigram) %>%
                select(frequency) %>%
                mutate(freqOfFrequency = 1) %>%
                group_by(frequency) %>%
                summarise_all(sum)
        
        SGT2 <-
                simpleGoodTuring(NrTbl2$frequency, NrTbl2$freqOfFrequency)
        
        NrTbl3 <- textstat_frequency(trigram) %>%
                select(frequency) %>%
                mutate(freqOfFrequency = 1) %>%
                group_by(frequency) %>%
                summarise_all(sum)
        
        SGT3 <-
                simpleGoodTuring(NrTbl3$frequency, NrTbl3$freqOfFrequency)
        
        c(
                dUnigram = function(freq) {
                        SGT1$p[as.character(freq)]
                },
                dBigram = function(freq) {
                        SGT2$r[as.character(freq)] / freq
                },
                dTrigram = function(freq) {
                        SGT3$r[as.character(freq)] / freq
                }
        )
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

fastPerplexity <- function(input, predictModel) {
        inputSentence <- corpus_reshape(input, to = "sentence")
        inputToken <- tokens(
                inputSentence,
                remove_symbols = TRUE,
                remove_punct = TRUE,
                remove_numbers = TRUE,
                remove_twitter = TRUE,
                remove_url = TRUE,
                include_docvars = FALSE
        )
        
        p_bo_unk <-
                as.numeric(predictModel$SGT.dUnigram(0)) / sum(predictModel$ngramsDt[ngramsize == 1]$frequency)
        S <- length(inputSentence$documents$texts)
        N <- 0
        p_c <- 0
        print(paste("sentences: ", S))
        for (s in 1:S) {
                print(s)
                m <- length(inputToken[[s]])
                p_s <- 0
                preced <- ""
                for (i in 1:m) {
                        w <- tolower(inputToken[[s]][i])
                        prediction <-
                                fastNextWords(preced, predictModel, outputs = 0)
                        p_bo <- prediction[prediction == w]$p_bo
                        if (length(p_bo) == 0) {
                                p_bo <- p_bo_unk
                        }
                        p_s <- p_s + log2(p_bo)
                        preced <- paste(preced, w, " ", sep = "")
                }
                N <- N + m
                p_c <- p_c + p_s
        }
        
        2 ^ (-p_c / N)
}

trainModel <- function(dataPath, predictModelFilePath) {
        attach(downloadData(dataPath))
        
        blogsSubSampling <-
                file.path(dataPath, "sub-sample.blogs.txt")
        subBlogs <- subSample(blogs, blogsSubSampling)
        
        twitterSubSampling <-
                file.path(dataPath, "sub-sample.twitter.txt")
        subTweets <- subSample(twitter, twitterSubSampling)
        
        newsSubSampling <-
                file.path(dataPath, "sub-sample.news.txt")
        subNews <- subSample(news, newsSubSampling)
        
        blogsTrain <- paste0(blogsSubSampling, ".train.txt")
        blogsTest <- paste0(blogsSubSampling, ".test.txt")
        twitterTrain <- paste0(twitterSubSampling, ".train.txt")
        twitterTest <- paste0(twitterSubSampling, ".test.txt")
        newsTrain <- paste0(newsSubSampling, ".train.txt")
        newsTest <- paste0(newsSubSampling, ".test.txt")
        
        trainBlogs <-
                devideDataset(blogsSubSampling, blogsTrain, blogsTest)
        trainTweets <-
                devideDataset(twitterSubSampling, twitterTrain, twitterTest)
        trainNews <-
                devideDataset(newsSubSampling, newsTrain, newsTest)
        
        projectCorpus <- readtext(blogsTrain) %>% corpus()
        projectCorpus <-
                projectCorpus + readtext(twitterTrain) %>% corpus()
        projectCorpus <-
                projectCorpus + readtext(newsTrain) %>% corpus()
        
        profanity <- readLines(badwords)
        
        projectToken <- projectCorpus %>%
                # nomarize words
                tokens(
                        remove_symbols = TRUE,
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        remove_twitter = TRUE,
                        remove_url = TRUE,
                        include_docvars = FALSE
                ) %>%
                # removing profanity and other words
                tokens_remove(profanity)
        
        ## 1-Gram
        unigram <- projectToken %>%
                dfm()
        
        ## 2-Gram
        bigram <- projectToken %>%
                tokens_ngrams(n = 2) %>%
                dfm()
        
        ## 3-Gram
        trigram <- projectToken %>%
                tokens_ngrams(n = 3) %>%
                dfm()
        
        SGT <- sgtFactory(unigram, bigram, trigram)
        
        remove(projectCorpus, projectToken)
        
        trigramDt <-
                data.table(
                        ngram = colnames(trigram),
                        keep.rownames = F,
                        stringsAsFactors = F
                )
        trigramDt[, base := strsplit(ngram, "_[^_]+$")[[1]], by = ngram]
        trigramDt[, prediction := str_split(ngram, paste0(base, "_"), n = 2)[[1]][2], by = ngram]
        trigramDt[, ngramsize := 3, by = ngram]
        trigramDt[, frequency := colSums(trigram)]
        
        bigramDt <-
                data.table(
                        ngram = colnames(bigram),
                        keep.rownames = F,
                        stringsAsFactors = F
                )
        bigramDt[, base := strsplit(ngram, "_[^_]+$")[[1]], by = ngram]
        bigramDt[, prediction := str_split(ngram, paste0(base, "_"), n = 2)[[1]][2], by = ngram]
        bigramDt[, ngramsize := 2, by = ngram]
        bigramDt[, frequency := colSums(bigram)]
        
        unigramDt <-
                data.table(
                        ngram = colnames(unigram),
                        keep.rownames = F,
                        stringsAsFactors = F
                )
        unigramDt[, base := "", by = ngram]
        unigramDt[, prediction := ngram, by = ngram]
        unigramDt[, ngramsize := 1, by = ngram]
        unigramDt[, frequency := colSums(unigram)]
        
        ngramsDt <- rbindlist(list(trigramDt, bigramDt, unigramDt))
        setkeyv(ngramsDt, c("ngram", "base", "prediction"))
        
        predictModel <- c(SGT = SGT, ngramsDt = list(ngramsDt))
        
        remove(trigramDt, bigramDt, unigramDt, ngramsDt)
        remove(unigram, bigram, trigram, SGT)
        
        save(predictModel, file = predictModelFilePath)
}
