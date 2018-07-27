downloadData <- function(workingDataPath = file.path("data")) {
        # Download Data
        rawDataFileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        downloadedZipfilePath <- file.path(workingDataPath, "Coursera-SwiftKey.zip")
        basePath <- file.path(workingDataPath, "final", "en_US")
        
        # Create working directory
        if(!file.exists(workingDataPath)) {
                dir.create(workingDataPath)
        }
        # Download ziped file
        if(!file.exists(downloadedZipfilePath)) {
                download.file(rawDataFileUrl, destfile = downloadedZipfilePath, method = "curl")
        }
        # Unzip
        if(!file.exists(basePath)) {
                unzip(zipfile = downloadedZipfilePath, exdir = workingDataPath)
        }
        
        list(blogs = file.path(basePath, "en_US.blogs.txt"), 
             twitter = file.path(basePath, "en_US.twitter.txt"),
             news = file.path(basePath, "en_US.news.txt"))
}
