# This class represents a simple mean reversion strategy
MeanReversionStrategy <- setClass(
  # Set the name for the class
  "MeanReversionStrategy",
  
  # Define the slots
  slots=c(
    portfolio = "CointegrationTestPortfolio"
  ),
  
  validity=function(object)
  {
    # Object and data validation checks can be performed here.
    # For simplicity, we will not implement any checks and insure validaty of the data in the main program
    return(TRUE);
  }
)

setGeneric(name="MeanReversionStrategy.getPortfolio",def=function(theObject){ standardGeneric("MeanReversionStrategy.getPortfolio")})
# This function returns the cointegrationResults
setMethod(f="MeanReversionStrategy.getPortfolio",signature="MeanReversionStrategy",definition=function(theObject)
{
  return(theObject@portfolio)
})

setGeneric(name="executeStrategy",def=function(theObject){ standardGeneric("executeStrategy") })
# Execute mean reversion strategy on the portfolio spread
setMethod(f="executeStrategy",signature=c("MeanReversionStrategy"),definition=function(theObject)
{
  timeStamps <- getPortfolioSpreadAndTransactionCosts(MeanReversionStrategy.getPortfolio(theObject))$Time
  begin <- timeStamps[1]
  end <- timeStamps[length(timeStamps)]
  
  tradableSpread <- getPortfolioSpreadAndTransactionCosts(MeanReversionStrategy.getPortfolio(theObject))$Spread
  transactionCosts <- getPortfolioSpreadAndTransactionCosts(MeanReversionStrategy.getPortfolio(theObject))$TransactionCosts
  
  meanSpread <- mean(tradableSpread)
  stdSpread <- sd(tradableSpread)
  zScore = (tradableSpread - meanSpread)/stdSpread
  
  longsEntry <- (zScore < -c_entryZscore)
  longsExit <- (zScore > -c_exitZscore)
  shortsEntry <- (zScore > c_entryZscore)
  shortsExit <- (zScore < c_exitZscore)
  
  nrDataPoints <- length(tradableSpread)
  # Create vector with 0 values
  numUnitsLong <- vector(mode="numeric",length=nrDataPoints)
  # Set NA from 2:end
  numUnitsLong[2:nrDataPoints] <- NA
  numUnitsShort <- numUnitsLong # Copy by value
  
  # numUnitsLong represents an array with 1 entrys on positions where we are long the portfolio spread
  numUnitsLong[longsEntry] = 1;
  numUnitsLong[longsExit] = 0
  numUnitsLong <- fillMissingValues(numUnitsLong)
  
  # numUnitsShort represents an array with -1 entrys on positions where we are short the portfolio spread
  numUnitsShort[shortsEntry] <- -1
  numUnitsShort[shortsExit] <- 0
  numUnitsShort <- fillMissingValues(numUnitsShort)
  
  # numUnits represents an array that indicates how many units of the portfolio spread we bought (1), or sold (-1)
  numUnits <- numUnitsLong+numUnitsShort
  
  portfolio <- MeanReversionStrategy.getPortfolio(theObject)
  currencyPairs <- getCurrencyPairList(portfolio)
  # numUnitsPortfolio epresents a matrix with the number of units of the portfolio that we buy or sell, for every timestamp
  numUnitsPortfolio <- repmat(matrix(numUnits),1,length(currencyPairs))
  
  # hedgeRatioMatrix represents a matrix with the hedgeRatio of each individual currencyPair, for every timestamp 
  # This can also be viewed as the "shares allocation" for each currency Pair at any given point in time (the hedge ratio is fixed and always the same)
  hedgeRatioMatrix <- repmat(matrix(getOptimalEigenvector(MeanReversionStrategy.getPortfolio(theObject)),nrow=1),length(numUnits),1)
  
  # prices matrix represents a matrix with the prices of the CurrencyPairs in the Portfolio, for every timestamp
  pricesMatrix <- data.matrix(getAllPrices(portfolio,begin,end))

  # hedgeRatioMatrix * pricesMatrix represents the USD capital allocation to buy the portfolio
  # positions represents our USD capital in each currencyPair at any given point in time
  positions <- numUnitsPortfolio*hedgeRatioMatrix*pricesMatrix
  
  # takePosition contains a 1 value at timestamps where we open a position (and incur transaction costs)
  takePosition = vector(mode="numeric",length=nrDataPoints)
  for(i in 2:length(numUnits))
  {
    if(abs(numUnits[i])==1 & numUnits[i-1]==0)
      takePosition[i]=1
  }
  absTransactionCosts <- takePosition*transactionCosts
  
  
  # Pnl of the strategy on each timeStamp
  pnl <- lag.matrix(positions)*(pricesMatrix-lag.matrix(pricesMatrix))/lag.matrix(pricesMatrix)
  pnl[which(is.na(pnl))] <- 0 # First entry is NA. Set to 0.
  pnl <- rowSums(pnl)
  
  # Subtract transactionCosts from pnl
  absTransactionCosts <- vect_lag(absTransactionCosts)
  absTransactionCosts[which(is.na(absTransactionCosts))] <- 0 # First entry is NA. Set to 0.
  pnl <- pnl-absTransactionCosts
  
  # Return is P&L divided by gross market value of the portfolio
  laggedPositions <- lag.matrix(positions)
  laggedPositions[which(is.na(laggedPositions))] <- 0
  ret <- pnl/rowSums(abs(laggedPositions))
  ret[which(is.na(ret))] <- 0

  par(mfrow=c(2,2))
  # Plot the spread
  plotSpread(portfolio,TRUE)
  
  # Plot standardizedSpread
  zScoreDiagramString <- paste('Strategy (Zscore entry above/below ',c_entryZscore, ' std, Zscore exit below/above ',c_exitZscore, ' std)',sep="")
  plot(timeStamps,zScore,xlab="Time",ylab="Standardized Spread",main=zScoreDiagramString,type="l")
  lim <- par("usr")
  beginRect=1;endRect=1;
  for(i in 2:length(numUnits))
  {
    # Begin long position
    if(numUnits[i]==1 & numUnits[i-1]==0)
      beginRect=i
    # Begin short position
    if(numUnits[i]==-1 & numUnits[i-1]==0)
      beginRect=i
    
    # End long position
    if(numUnits[i]==0 & numUnits[i-1]==1)
      rect(timeStamps[beginRect],lim[3]-1, timeStamps[i-1], -c_exitZscore, col = "green")
    # End short position
    if(numUnits[i]==0 & numUnits[i-1]==-1)
      rect(timeStamps[beginRect],c_exitZscore, timeStamps[i-1], lim[4]+2, col = "red")
  }
  lines(timeStamps,zScore)
  abline(h=c(-c_entryZscore,c_entryZscore,-c_exitZscore,c_exitZscore,0),col=c("blue","blue","red","red","green"))
  
  # Calculate and plot the results
  APR <- round((prod(1+ret)^(252/length(ret))-1)*100,digits=4)
  sharpeRatio <- round(sqrt(252)*mean(ret)/sd(ret),digits=4)
  results <- (cumprod(1+ret)-1)*100
  maxDD <- round(maxDrawdown(ret)*100,digits=4)
  
  dailyReturnsString <- paste('Daily Returns (average: ',round(mean(ret*100),digits=4), '%, std: ', round(sd(ret*100),digits=4), '%)',sep="")
  plot(timeStamps,ret*100,xlab="Time",ylab="Returns (%)", main=dailyReturnsString)
  
  resultString <- paste('StrategyResults (APR: ',APR,'%, Sharpe Ratio: ',sharpeRatio,', maximumDrawdown: ',maxDD,'%)',sep="")
  plot(timeStamps,results,xlab="Time",ylab="Return (%)",main=resultString, type='l')
  
  return(ret)
})
