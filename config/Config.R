packages <- c("httr", "XML", "zoo", "PoloniexR", "quantmod",
              "xts", "lubridate", "PerformanceAnalytics")

packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

Sys.setenv(TZ='UTC')

source("functions/FetchBTCInfo.R")
source("functions/PlotReturns.R")
source("functions/CalculateReturnsUnivariate.R")
source("functions/SimpleMACDStrategyUnivariate.R")
