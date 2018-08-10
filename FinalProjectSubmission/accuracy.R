source("trainModel.R")

if(!"predictModel" %in% objects()) {
        load(file.path("ShinyApplication", "predictModelForApp.rda"))
}

dataPath <- file.path("..", "data")
blogsSubSampling <- file.path(dataPath, "sub-sample.blogs.txt")
twitterSubSampling <- file.path(dataPath, "sub-sample.twitter.txt")
newsSubSampling <- file.path(dataPath, "sub-sample.news.txt")

blogsTrain <- paste0(blogsSubSampling, ".train.txt")
blogsTest <- paste0(blogsSubSampling, ".test.txt")
twitterTrain <- paste0(twitterSubSampling, ".train.txt")
twitterTest <- paste0(twitterSubSampling, ".test.txt")
newsTrain <- paste0(newsSubSampling, ".train.txt")
newsTest <- paste0(newsSubSampling, ".test.txt")

badwords <-
        file.path(dataPath, "bad-words.txt")

profanity <- readLines(badwords)

blogsTestCorpus <- readtext("../data/600-sample.blogs.txt.test.txt") %>%
        corpus()
blogsTrainCorpus <- readtext("../data/600-sample.blogs.txt.train.txt") %>%
        corpus()

blogsTestPerplexity <- fastPerplexity(blogsTestCorpus, predictModel)
blogsTrainPerplexity <- fastPerplexity(blogsTrainCorpus, predictModel)

blogsTestAccuracy <- fastAccuracy(blogsTestCorpus, predictModel, profanity)
blogsTrainAccuracy <- fastAccuracy(blogsTrainCorpus, predictModel, profanity)

print(c(blogsTest = blogsTestPerplexity, blogsTrain = blogsTrainPerplexity))
print(c(blogsTest = blogsTestAccuracy, blogsTrain = blogsTrainAccuracy))
