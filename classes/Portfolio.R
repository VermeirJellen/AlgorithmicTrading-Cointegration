# Portfolio class
# A Portfolio object consists of a list of CurrencyPair objects and some generic methods that can be invoked on a Portfolio
Portfolio <- setClass(
  # Set the name for the class
  "Portfolio",
  
  # Define the slots
  slots=c(currencyPairList = "list"),
           
  # Define prototype
  prototype=list( currencyPairList=list() ), # Create Portfolio object with empty CurrencyPair list
  
  validity=function(object)
  {
    # Object and data validation checks can be performed here.
    # For simplicity, we will not implement any checks and insure validaty of the data in the main program
    return(TRUE);
  }
)

setGeneric(name="getCurrencyPairList",def=function(theObject){ standardGeneric("getCurrencyPairList")})
# This function fetches the list of CurrencyPair objects that are part of this portfolio
setMethod(f="getCurrencyPairList",signature="Portfolio",definition=function(theObject)
{
  return(theObject@currencyPairList)
})

setGeneric(name="setCurrencyPairList",def=function(theObject,cList){ standardGeneric("setCurrencyPairList")})
# This method sets the list of CurrencyPair objects that are part of this portfolio
setMethod(f="setCurrencyPairList",signature=c("Portfolio","list"),definition=function(theObject,cList)
{
  theObject@currencyPairList <- cList
  validObject(theObject)
  return(theObject)
})


setGeneric(name="findMinimumTimeStamp",def=function(theObject){ standardGeneric("findMinimumTimeStamp") })
# This function returns the earliest timestamp for which there is data for at least one of the CurrencyyPair objects in the Portfolio
setMethod(f="findMinimumTimeStamp",signature=c("Portfolio"),definition=function(theObject)
{
  cList <- getCurrencyPairList(theObject)
  globalMinimum = getTimeSeriesData(cList[[1]])$Time[1]; # Minimum timestamp for the first currencypair
  for(i in 2:length(cList)) # Loop over all currency pairs in the list and find global minimum
  {
    minimum = getTimeSeriesData(cList[[i]])$Time[1];
    if(minimum < globalMinimum)
      globalMinimum = minimum;
  }
  return(globalMinimum)
})

setGeneric(name="findMaximumTimeStamp",def=function(theObject){ standardGeneric("findMaximumTimeStamp") })
# This method returns the latest timestamp for which there is data for at least one of the CurrencyPair objects in the Portfolio
setMethod(f="findMaximumTimeStamp",signature=c("Portfolio"),definition=function(theObject)
{
  cList <- getCurrencyPairList(theObject)
  timeStamps = getTimeSeriesData(cList[[1]])$Time
  globalMaximum = timeStamps[length(timeStamps)] # Maximal timestamp for the first CurrencyPair object of the Portfolio
  for(i in 2:length(cList)) # Loop over all other CurrenyPair objects in the portfolio
  {
    timeStamps = getTimeSeriesData(cList[[i]])$Time
    maximum = timeStamps[length(timeStamps)]
    if(maximum > globalMaximum)
      globalMaximum = maximum;
  }
  return(globalMaximum)
})


setGeneric(name="getAvailablePairsInTimeWindow", def=function(theObject,begin,end){ standardGeneric("getAvailablePairsInTimeWindow")})
# This method returns a new subPortfolio consisting of CurrencyPair objects from the original Portfolio for which data is fully available in the [begin:end] interval
# Note: the [begin:end] data-interval is NOT filtered in the resulting subportfolio. 
# Hence, Full data intervals for the relevant CurrencyPair objects is returned
setMethod(f="getAvailablePairsInTimeWindow", signature=c("Portfolio","POSIXlt","POSIXlt"), definition=function(theObject,begin,end)
{
  resultList = list() # Create empty CurrencyPair list
  cList <- getCurrencyPairList(theObject) # Fetch CurrencyPair list associated to this Portfolio
  for(i in 1:length(cList))
  {
    # Check if CurrencyPair i of the current portfolio contains data inside the [begin:end] interval
    if(containsDataInterval(cList[[i]],begin,end))
      resultList[length(resultList)+1] <- cList[[i]] # We add the CurrencyPair object to the resultList
  }
  # We return a new sub Portfolio that consists of the CurrencyPair objects in the resultList
  return(Portfolio(currencyPairList=resultList))
})


setGeneric(name="copySubIntervalPortfolio", def=function(theObject,begin,end,fullIntervalRequired=TRUE){ standardGeneric("copySubIntervalPortfolio") })
# This method returns a new subPortfolio consisting of CurrencyPair objects from the original Portfolio. 
# However, only data in the [begin:end] is retained for the CurrencyPair objects in the new subPortfolio
# If filterCurrencyPairs==TRUE, then CurrencyPairs that do not have complete data in the [begin:end] interval are removed from the subPortfolio
setMethod(f="copySubIntervalPortfolio", signature=c("Portfolio","POSIXlt","POSIXlt"), definition=function(theObject,begin,end,fullIntervalRequired=TRUE)
{
  if(fullIntervalRequired) # Create subPortfolio of CurrencyPair objects that have complete data in [begin:end] interval and fetch the CurrencyPair list
    cList <- getCurrencyPairList(getAvailablePairsInTimeWindow(theObject,begin,end))
  else
    cList <- getCurrencyPairList(theObject)
  
  nrEntries <- length(cList)
  resultList <- vector('list',nrEntries)
  for(i in 1:nrEntries)
    resultList[[i]] <- copySubInterval(cList[[i]],begin,end)
  
  # Return the resulting Portfolio object
  resultPortfolio <- setCurrencyPairList(theObject,resultList)
  validObject(resultPortfolio)
  return(resultPortfolio)
})

setGeneric(name="cleanPortfolio", def=function(theObject,begin,end){ standardGeneric("cleanPortfolio") })
# Remove all data entrys from the input portfolio in the [begin,end] interval where volume == 0
# Important note: This method assumes that TimeSeries data of individiual CurrencyPairs in the portfolio are alligned with eachother
# (The portfolio's are structured according to this assumption during the control flow of the main project)
setMethod(f="cleanPortfolio",signature=c("Portfolio","POSIXlt","POSIXlt"),definition=function(theObject,begin,end)
{
  cList <- getCurrencyPairList(theObject)
  # Maintain entrys outside of the [begin:end] interval
  logicalVectorTime <- (getTimeSeriesData(cList[[1]])$Time[1] < begin | getTimeSeriesData(cList[[1]])$Time > end)
  
  # Maintain values for which the volume indicator > 0, for the first CurrencyPair object
  logicalVectorVolume <- (getTimeSeriesData(cList[[1]])$Volume > 0)
  # Volume values of every other CurrencyPair object in the cList must also be > 0, otherwise the corresponding data entry must be removed
  for(i in seq(2:length(cList)))
    logicalVectorVolume <- (logicalVectorVolume & getTimeSeriesData(cList[[i]])$Volume > 0)
  
  # Check if data is outside of the [begin:end] interval and/or volume values of all CurrencyPair objects are > 0
  logicalVectorEndResult = (logicalVectorTime | logicalVectorVolume)
  
  resultList = vector("list",length(cList))
  # We only keep the time series data for which the conditions are met
  for(i in seq(1:length(cList)))
    resultList[[i]] <- setTimeSeriesData(cList[[i]],getTimeSeriesData(cList[[i]])[logicalVectorEndResult,])
  
  # Return a portfolio that contains the cleaned CurrencyPair objects
  resultPortfolio <- setCurrencyPairList(theObject,resultList)
  validObject(resultPortfolio)
  return(resultPortfolio)
})


setGeneric(name="getAllPrices", def=function(theObject,begin,end){ standardGeneric("getAllPrices") })
# This method adds the bidprices of all individual CurrencyPairs in a dataframe and returns it
setMethod(f="getAllPrices",signature=c("Portfolio","POSIXlt","POSIXlt"),definition=function(theObject,begin,end){
  cList <- getCurrencyPairList(theObject)
  
  # Create dataframe with bidprices of the first CurencyPair inside the [begin:end] interval
  logicalVector <- (getTimeSeriesData(cList[[1]])$Time >= begin & getTimeSeriesData(cList[[1]])$Time <= end)
  bidPrices <- getTimeSeriesData(cList[[1]])$Bid[logicalVector]
  prices <- data.frame(bidPrices)
  colnames(prices)[1] <- getIdentifier(cList[[1]])
  
  # Append bidprices of all other CurrencyPairs to the dataframe
  for(i in 2:length(cList))
  {
    logicalVector <- (getTimeSeriesData(cList[[i]])$Time >= begin & getTimeSeriesData(cList[[i]])$Time <= end)
    bidPrices <- getTimeSeriesData(cList[[i]])$Bid[logicalVector]
    prices <- cbind(prices,bidPrices)
    colnames(prices)[i] <- getIdentifier(cList[[i]])
  }
  return(prices)
})

setGeneric(name="getAllTransactionCosts", def=function(theObject,begin,end){ standardGeneric("getAllTransactionCosts") })
# This method adds the TransactionCosts of all individual CurrencyPairs in a dataframe and returns it
setMethod(f="getAllTransactionCosts",signature=c("Portfolio","POSIXlt","POSIXlt"),definition=function(theObject,begin,end){
  cList <- getCurrencyPairList(theObject)
  
  # Create dataframe with transactionCosts of the first CurencyPair inside the [begin:end] interval
  logicalVector <- (getTimeSeriesData(cList[[1]])$Time >= begin & getTimeSeriesData(cList[[1]])$Time <= end)
  costs <- getTimeSeriesData(cList[[1]])$TransactionCost[logicalVector]
  transactionCosts <- data.frame(costs)
  colnames(transactionCosts)[1] <- getIdentifier(cList[[1]])
  
  # Append transactionCosts of all other CurrencyPairs to the dataframe
  for(i in 2:length(cList))
  {
    logicalVector <- (getTimeSeriesData(cList[[i]])$Time >= begin & getTimeSeriesData(cList[[i]])$Time <= end)
    costs <- getTimeSeriesData(cList[[i]])$TransactionCost[logicalVector]
    transactionCosts <- cbind(transactionCosts,costs)
    colnames(transactionCosts)[i] <- getIdentifier(cList[[i]])
  }
  return(transactionCosts)
})

setGeneric(name="plotCurrencyPairsSeparately", def=function(theObject,begin,end){ standardGeneric("plotCurrencyPairsSeparately") })
# This method plots the prices of the individual pairs in subplots
setMethod(f="plotCurrencyPairsSeparately",signature=c("Portfolio","POSIXlt","POSIXlt"),definition=function(theObject,begin,end){
  cList <- getCurrencyPairList(theObject)
  
  sizeCollection = length(cList)
  
  # Calculate the number of rows and columns needed to create the subplots
  nrColumns = ceiling(sqrt(sizeCollection))
  nrRows <- nrColumns
  if((nrRows-1)*nrColumns >= sizeCollection)
    nrRows <- nrRows-1
  par(mfrow=c(nrRows,nrColumns))
  
  for(i in 1:sizeCollection)
   plotCurrencyPair(cList[[i]],begin,end)
  par(mfrow=c(1,1))
})

setGeneric(name="plotCurrencyPairsTogether", def=function(theObject,begin,end){ standardGeneric("plotCurrencyPairsTogether") })
# This method plots the prices of the individual pairs together in one plot
setMethod(f="plotCurrencyPairsTogether",signature=c("Portfolio","POSIXlt","POSIXlt"),definition=function(theObject,begin,end){
  cList <- getCurrencyPairList(theObject)
  sizeCollection = length(cList)

  logicalVectorTime <- (getTimeSeriesData(cList[[1]])$Time[1] >= begin & getTimeSeriesData(cList[[1]])$Time <= end)
  
  res <- getTimeSeriesData(cList[[1]])$Bid[logicalVectorTime]
  for(i in 2:sizeCollection)
    res <- cbind(res,getTimeSeriesData(cList[[i]])$Bid[logicalVectorTime])
  
  timeInterval <- getTimeSeriesData(cList[[1]])$Time[logicalVectorTime]
  par(mfrow=c(1,1))
  plot(timeInterval,res[,1],ylim=range(res),type='l',xlab="Price",ylab="Time",col=1)
  for(i in 2:sizeCollection)
  {
    par(new=TRUE)
    plot(timeInterval,res[,i],ylim=range(res),axes=FALSE,xlab="",ylab ="",type='l',col=i)
  }
})

# This function takes a collection of CointegrationTestPortfolio objects as inputs and plots the spreads
plotCointegratedSpreads <- function(cointegrationResultCollection)
{
  sizeCollection = length(cointegrationResultCollection)
  
  # Calculate the number of rows and columns needed to create the subplots
  nrColumns = ceiling(sqrt(sizeCollection))
  nrRows <- nrColumns
  if((nrRows-1)*nrColumns >= sizeCollection)
    nrRows <- nrRows-1
  par(mfrow=c(nrRows,nrColumns))
  
  # Iterate over all the CointegrationResults and plot the Spread
  for(i in 1:sizeCollection)
    plotSpread(cointegrationResultCollection[[i]])
}

# CointegrationTestPortfolio Inherits from the Portfolio baseclass.
# Objects of this class represent Portfolio's on which specific Cointegration related functions and operations can be performed
CointegrationTestPortfolio <- setClass(
  # Set the name for the class
  "CointegrationTestPortfolio",
  
  # cointegrationResults slot of class ca.jo
  slots=c(cointegrationResults = "ca.jo"),
  
  # Default validity function
  validity=function(object){ return(TRUE); },
  
  # Inherit from Portfolio base class
  contains="Portfolio"
)


setGeneric(name="cointegrationTestPortfolioCopyConstructor",def=function(theObject,portfolio){ standardGeneric("cointegrationTestPortfolioCopyConstructor")})
# This function copies and transforms a basic Portfolio object into a CointegrationTestPortfolio
# Note: function is not used in project control flow
setMethod(f="cointegrationTestPortfolioCopyConstructor",signature=c("CointegrationTestPortfolio","Portfolio"),definition=function(theObject,portfolio)
{
  # Copy the currency pair list
  cointegrationTestPortfolio <- setCurrencyPairList(theObject,getCurrencyPairList(portfolio))
  
  # Check if individual pairs in the portfolio are I(1)
  if(individualPairsI1(cointegrationTestPortfolio,begin,end))
  {
    # Perform cointegration test on portfolio. Set the cointegration testresults
    cointegrationTestPortfolio <- performJohansenProcedureForCointegration(cointegrationTestPortfolio,begin,end)
  }
  
  validObject(cointegrationTestPortfolio)
  return(cointegrationTestPortfolio)
})


setGeneric(name="getCointegrationResults",def=function(theObject){ standardGeneric("getCointegrationResults")})
# This function fetches the cointegration testresults for this Portfolio
setMethod(f="getCointegrationResults",signature="CointegrationTestPortfolio",definition=function(theObject)
{
  return(theObject@cointegrationResults)
})

setGeneric(name="setCointegrationResults",def=function(theObject,results){ standardGeneric("setCointegrationResults")})
# This function sets the cointegration testresults for this Portfolio
setMethod(f="setCointegrationResults",signature=c("CointegrationTestPortfolio","ca.jo"),definition=function(theObject,results)
{
  theObject@cointegrationResults <- results
  validObject(theObject)
  return(theObject)
})


setGeneric(name="individualPairsI1", def=function(theObject,begin,end){ standardGeneric("individualPairsI1") })
# This method returns true when all the CurrencyPair objects in the Portfolio are integrated of order 1 in the [begin:end] interval
setMethod(f="individualPairsI1",signature=c("CointegrationTestPortfolio","POSIXlt","POSIXlt"), definition=function(theObject,begin,end)
{
  cList <- getCurrencyPairList(theObject)
  for(i in seq(1,length(cList)))
  {
    if(!isI1(cList[[i]],begin,end))
    {
      print(getIdentifier(cList[[i]]))
      return(FALSE)
    }
  }
  return(TRUE);
})


setGeneric(name="performJohansenProcedureForCointegration", def=function(theObject,begin,end){ standardGeneric("performJohansenProcedureForCointegration") })
# This method performs the johansen procedure for cointegration on the [begin:end] interval of the Portfolio and fills in the cointegrationResults slot with the results
# Only the data interval for which the test was performed is retained for the resulting Portfolio
setMethod(f="performJohansenProcedureForCointegration",signature=c("CointegrationTestPortfolio","POSIXlt","POSIXlt"),definition=function(theObject,begin,end){

  prices <- getAllPrices(theObject,begin,end)
  
  # We first find optimal VAR solution lag length by minimizing an information criterion obtained for different lag length models
  # SC information criterion is used for consistency since we have big sample sizes --> Will deliver asymptotically correct results (but is inefficient).
  # Include intercept in model: Allow trend in data generating process for the levels of the prices.
  varest <- VAR(prices,p=1,type="const",lag.max=24, ic="SC")
  # in the Johansen procedure for cointegration a lagged VAR (VECM) is used. Hence we need to subtract 1 from the optimal VAR lag length.
  lagLength <- max(2,varest$p-1)
  
  # Perform Johansen procedure for cointegration
  # Allow intercepts in the cointegrating vector: data without zero mean
  # Use trace statistic (null hypothesis: number of cointegrating vectors <= r)
  res <- ca.jo(prices,type="trace",ecdet="const",K=lagLength,spec="longrun")
  
  # Create copy of the portfolio that only contains data for the [begin:end] interval for which the cointegration was tested
  cointegrationTestedPortfolio <- copySubIntervalPortfolio(theObject,begin,end,FALSE)
  # Set the cointegration testresults
  cointegrationTestedPortfolio <- setCointegrationResults(cointegrationTestedPortfolio,res)
  
  # Return the resulting CointegrationTestPortfolio
  return(cointegrationTestedPortfolio)
})

setGeneric(name="isCointegrated",def=function(theObject){ standardGeneric("isCointegrated") })
# This method returns true when the Portfolio is cointegrated with at least 90% confidence, FALSE otherwise
# Note: the function assumes that the johansen procedure was executed on the Portfolio
setMethod(f="isCointegrated",signature="CointegrationTestPortfolio",definition=function(theObject)
{
  testStatistics <- getCointegrationResults(theObject)@teststat
  criticalValues <- getCointegrationResults(theObject)@cval
  
  # chi^2. If testStatic for r<= 0 is greater than the corresponding criticalValue, then r<=0 is rejected and we have at least one cointegrating vector
  # Note: trace statistic implicitly assumed, as was implemented during the control flow of the project
  # We use 90% confidence level to make our decision
  if(testStatistics[length(testStatistics)] >= criticalValues[dim(criticalValues)[1],1])
    return(TRUE)
  else
    return(FALSE)
})

setGeneric(name="getOptimalEigenvector",def=function(theObject){ standardGeneric("getOptimalEigenvector")})
# This function fetches and returns the optimal cointegrated eigenvector/hedgeratio's
# Note: the function assumes that the johansen procedure was executed on the Portfolio
setMethod(f="getOptimalEigenvector",signature="CointegrationTestPortfolio",definition=function(theObject)
{
  # Return eigenvector that has maximum eigenvalue. Only fetch values that correspond to the hedge ratio's, we do not need the constant coefficient
  return(getCointegrationResults(theObject)@V[1:length(getCurrencyPairList(theObject)),which.max(getCointegrationResults(theObject)@lambda)])
})

setGeneric(name="getPortfolioSpreadAndTransactionCosts",def=function(theObject){ standardGeneric("getPortfolioSpreadAndTransactionCosts")})
# This method calculates and returns the Portfolio spread and the Portfolio transaction costs that are incurred when bying/selling the portfolio at the corresponding timeStamp
# Calculation depends on the individual price series of the CurrencyPair objects and the optimal eigenvector associated to the results of the cointegration test
# A dataframe with Time stamps and the value of the spread is returned
# Note: the function assumes that the johansen procedure was executed on the Portfolio
setMethod(f="getPortfolioSpreadAndTransactionCosts",signature="CointegrationTestPortfolio",definition=function(theObject)
{
  # Fetch timestamps for first CurrencyPair (timestamps are alligned for other CurrencyPair objects)
  timeStamps <- getTimeSeriesData(getCurrencyPairList(theObject)[[1]])$Time
  begin <- timeStamps[1]
  end <- timeStamps[length(timeStamps)]
  
  # fetch dataframe containing bidprices of all the CurrencyPair objects in the Portfolio
  prices <- getAllPrices(theObject,begin,end)
  # Fetch optimal eigenvector
  evec <- getOptimalEigenvector(theObject)
  
  # Calculate the spread
  # hegde ratio's / eigenvector values are multiplied with their associated price values and summation is made
  # If the Portfolio is cointegrated then the resulting spread should be stationary
  portfolioSpread <- rowSums(t(evec*t(prices)))
  
  
  # fetch dataframe containing transaction costs of all the CurrencyPair objects in the Portfolio
  transactionCosts <- getAllTransactionCosts(theObject,begin,end)
  # Absolute value of the hegderatio's represent the amount of units of each individual CurrencyPair that we buy or sell
  # We multiply these amounts by the transaction costs per CurrencyPair unit and make the summation to obtain the total transaction cost at this point in time
  portfolioTransactionCosts <- rowSums(t(abs(evec)*t(transactionCosts)))
  
  dataframe <- data.frame(Time=timeStamps,Spread=portfolioSpread,TransactionCosts=portfolioTransactionCosts)
  # Convert Time to POSIXlt format
  dataframe$Time <- timeStamps
  return(dataframe)
})

setGeneric(name="calculateHalfLife",def=function(theObject){ standardGeneric("calculateHalfLife")})
# This function calculates the halflife of mean reversion and returns the result
# dy(t) = (lambda*y(t-1) + mu)dt + dE
# Halflife = -log(2)/lambda
# Note: the function assumes that the johansen procedure was executed on the Portfolio
setMethod(f="calculateHalfLife",signature="CointegrationTestPortfolio",definition=function(theObject)
{
  portfolioSpread = getPortfolioSpreadAndTransactionCosts(theObject)$Spread
  laggedPortfolioSpread = vect_lag(portfolioSpread,1)
  deltaSpread = portfolioSpread-laggedPortfolioSpread
  
  laggedPortfolioSpread <- laggedPortfolioSpread[!is.na(laggedPortfolioSpread)]
  deltaSpread <- deltaSpread[!is.na(deltaSpread)]
  
  fit <- lm(deltaSpread ~ laggedPortfolioSpread)
  result <- -log(2)/fit$coefficients[2]
  return(result)
})


setGeneric(name="plotSpread",def=function(theObject,details=FALSE){ standardGeneric("plotSpread")})
# This function plots the Portfolio spread and shows the average and the +- 1 and 2 standard deviations
# Note: the function assumes that the johansen procedure was executed on the Portfolio
setMethod(f="plotSpread",signature="CointegrationTestPortfolio",definition=function(theObject,details=FALSE)
{
  currencyPairs <- getCurrencyPairList(theObject)
  timeStamps <- getPortfolioSpreadAndTransactionCosts(theObject)$Time
  spread <- getPortfolioSpreadAndTransactionCosts(theObject)$Spread
  halfLife <- calculateHalfLife(theObject)
  halfLifeString <- paste(" (HalfLife is ",ceiling(halfLife),' days)',sep="")
  
  nrPairs <- length(currencyPairs)
  evec <- getOptimalEigenvector(theObject)
  currencyString <- paste('Portfolio ','(',getIdentifier(currencyPairs[[1]]),sep="")
  spreadString <- paste(evec[1], "*", getIdentifier(currencyPairs[[1]]),sep="")
  for(j in 2:nrPairs)
  {    
    currencyString <- paste(currencyString,',',getIdentifier(currencyPairs[[j]]),sep="")
    sign = "-"
    if(evec[j] > 0)
      sign = "+"
    spreadString <- paste(spreadString,sign,round(abs(evec[j]),2),"*",getIdentifier(currencyPairs[[j]]),sep="")
  }
  currencyString <- paste(currencyString,')',sep="")
  
  if(details)
    plot(timeStamps,spread,xlab=paste("Time",halfLifeString),ylab="Spread",main=spreadString,type="l")
  else
    plot(timeStamps,spread,xlab=paste("Time",halfLifeString),ylab="Spread",main=currencyString,type="l")
  abline(h=c(mean(spread),mean(spread)+sd(spread),a=mean(spread)+2*sd(spread),mean(spread)-sd(spread),mean(spread)-2*sd(spread)),col=c("green","blue","red","blue","red"))
})