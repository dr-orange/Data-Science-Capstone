source("trainModel.R")

dataPath <- file.path("..", "data")
predictModelFilePath <-
        file.path("ShinyApplication", "predictModelForApp.rda")

trainModel(dataPath, predictModelFilePath)
