# Go long when MACD passes signal line to the upside, vice versa short
SimpleMACDStrategyUnivariate <- function(asset, 
                                         asset.name    = "asset",
                                         nFast         = 12,
                                         nSlow         = 26,
                                         nSig          = 9,
                                         long.only     = TRUE,
                                         plot.strategy = TRUE,
                                         plot.results  = TRUE, 
                                         strategy.name ="Custom_Strategy"){
  
  if(plot.strategy){
    chartSeries(asset, TA='addMACD()', name = asset.name);
    
    all.years <- as.character(unique(format(index(asset),"%Y")))
    lapply(all.years, function(x) zoomChart(x))
  }
  
  macd     <- MACD(asset, nfFAst = nFast, nSlow = nSlow, nSig = nSig)
  go.long  <- na.omit(macd[, "macd"] >= macd[, "signal"])
  go.short <- !go.long
  
  long.entry <- macd[, "macd"] >= macd[, "signal"]
  long.exit  <- macd[, "macd"] < macd[, "signal"]
  
  short.entry <- macd[, "macd"] < macd[, "signal"]
  short.exit  <- macd[, "macd"] >= macd[, "signal"]
  
  # Initialize long signal array
  long.signals <- xts(matrix(data=rep(NA, nrow(asset)), nrow=nrow(asset), ncol=1),
                      order.by=index(asset))
  # Set 1 where long entry
  long.signals[long.entry] <- 1
  # Set 0 where long exit
  long.signals[long.exit] <- 0
  # Carry long signals forward
  long.signals <- na.locf(long.signals, fromLast=FALSE)
  
  # Initialize short signal array
  short.signals <- xts(matrix(data=rep(NA, nrow(asset)),nrow=nrow(asset), ncol=1),
                       order.by=index(asset))
  
  # Set -1 where short entry
  short.signals[short.entry] <- -1
  if(long.only){
    short.signals[short.entry] <- 0
  }
  # Set 0 where short exit
  short.signals[short.exit] <- 0
  
  # Carry short signals forward
  short.signals <- na.locf(short.signals, fromLast=FALSE)
  
  # add long and short signals for overal direction
  directions <- long.signals+short.signals
  # lag forward to allign forecasted direction with actual next day realized returns
  directions <- lag(directions, 1)
  
  return( strategy.results <- CalculateReturnsUnivariate(asset         = asset, 
                                                         directions    = directions,
                                                         plot.results  = plot.results, 
                                                         strategy.name = strategy.name) )
}