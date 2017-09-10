
#################
### BITCOIN #####
#################
source("config/Config.R")
btc.close <- FetchBTCInfo(param           = "market-price",   
                          data.identifier = "btc.close", 
                          date.start      = "2012-01-01")
names(btc.close) <- "close"

btc.directions.macd <- SimpleMACDStrategyUnivariate(asset         = btc.close, 
                                                    asset.name    = "BTC ChART",
                                                    nFast         = 12,
                                                    nSlow         = 26,
                                                    nSig          = 9,
                                                    long.only     = TRUE,
                                                    plot.strategy = TRUE,
                                                    plot.results  = TRUE,
                                                    strategy.name = "MACD Strategy")
