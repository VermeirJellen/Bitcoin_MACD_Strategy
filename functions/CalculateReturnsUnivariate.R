CalculateReturnsUnivariate <- function(asset, 
                                       directions, 
                                       plot.results  = TRUE,
                                       strategy.name = "Custom.Strategy"){
  
  directions <- na.omit(directions)
  returns    <- na.omit((asset/lag(asset, k= 1) - 1) *100)
  returns    <- returns[index(directions)]
  
  buy.and.hold.returns <- rep(1, nrow(returns)) * returns
  strategy.returns     <- directions * returns
  
  if(plot.results){
    PlotReturns(returns              = strategy.returns,
                strategyName         = paste(strategy.name, "- Complete Time Period"), 
                returns.compare      = buy.and.hold.returns, 
                strategyName.compare = "Buy and Hodl - Complete Time Period")
    
    all.years <- as.character(unique(format(index(strategy.returns),"%Y")))
    lapply(all.years, function(x) { PlotReturns(returns  = strategy.returns[x],
                                    strategyName         = paste(strategy.name, "-", x), 
                                    returns.compare      = buy.and.hold.returns[x], 
                                    strategyName.compare = paste("Buy and Hodl - ", x))})
  }
  
  return(list(strategy.returns     = strategy.returns,
              buy.and.hold.returns = buy.and.hold.returns))
}