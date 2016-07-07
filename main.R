require(weatherData)
source("examplePresentation.R")
source("Comparison.R")
source("finkImportantPoints.R")
source("paa.R")
source("rdp.R")
source("generateDatasets.R")
source("simMeasures.R")


#### First test ######

# data <- data.frame(x = 1:40, y = rnorm(40, 2))
# showImportantPointsExample(data)
# showPAAExample(data)

######################


# lineData <- generateDataSets()

#Use precalculated eps for rdp in detailed analysis
#has to be called first to use detailed analysis
# lineData <- visualComparison(lineData, method = "ips") #paa
# results <- detailedAnalysis(lineData)

# results2 <- multiAnalysis(N = 3) #can take a while...
