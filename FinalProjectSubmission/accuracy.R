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

blogsTestCorpus <- readtext(blogsTest) %>%
        corpus()

blogsPerplexity <- fastPerplexity(blogsTestCorpus, predictModel)

print(list(blogs = blogsPerplexity))
