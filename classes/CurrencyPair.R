# Instances of this S4 class contain all relevant information and time series data for one specific currency pair
CurrencyPair <- setClass(
  # Set the name for the class
  "CurrencyPair",
  
  # Define the slots
  slots=c(
      identifier = "character", #Unique identifier for this currency pair
      baseCurrency = "character", # base currency pair
      quoteCurrency = "character", # quote currency pair
      timeSeriesData = "list" # contains the data we need to perform the relevant calculations
      # timeSeriesDta contains Time stamps, Bid price, Volume information and transaction costs per unit of the base/quote pair
  ),
  
  # Default prototype
   prototype=list(
      identifier="EURUSD",
      baseCurrency="EUR",
      quoteCurrency="USD",
      timeSeriesData = data.frame(Time=as.POSIXlt(Sys.time()),Bid=0,Volume=0,TransactionCost=0)
  ),
  
  # Object and data validation checks can be performed here.
  # For simplicity, we will not implement any checks and insure validaty of the data in the main program
  validity=function(object){ return(TRUE); }
  
)

setGeneric(name="getIdentifier",def=function(theObject){ standardGeneric("getIdentifier") })
# Implement getter method. Return identifier
setMethod(f="getIdentifier",signature="CurrencyPair",definition=function(theObject)
{
  return(theObject@identifier)
})

setGeneric(name="getBaseCurrency",def=function(theObject){ standardGeneric("getBaseCurrency") })
# Implement getter method. Return baseCurrency
setMethod(f="getBaseCurrency",signature="CurrencyPair",definition=function(theObject)
{
  return(theObject@baseCurrency)
})

setGeneric(name="getQuoteCurrency",def=function(theObject){ standardGeneric("getQuoteCurrency") })
# Implement getter method. Return quoteCurrency
setMethod(f="getQuoteCurrency",signature="CurrencyPair",definition=function(theObject)
{
  return(theObject@quoteCurrency)
})

setGeneric(name="getTimeSeriesData",def=function(theObject){ standardGeneric("getTimeSeriesData") })
# Implement getter method. Return timeSeriesData
setMethod(f="getTimeSeriesData",signature="CurrencyPair",definition=function(theObject)
{
  return(theObject@timeSeriesData)
})


setGeneric(name="setIdentifier",def=function(theObject,id){ standardGeneric("setIdentifier") })
# Implement setter method. Set identifier
setMethod(f="setIdentifier",signature=c("CurrencyPair","character"),definition=function(theObject,id)
{
  theObject@identifier <- id
  validObject(theObject)
  return(theObject)
})

setGeneric(name="setBaseCurrency",def=function(theObject,baseCurrency){ standardGeneric("setBaseCurrency") })
# Implement setter method. Set baseCurrency
setMethod(f="setBaseCurrency",signature=c("CurrencyPair","character"),definition=function(theObject, baseCurrency)
{
  theObject@baseCurrency <- baseCurrency
  validObject(theObject)
  return(theObject)
})

setGeneric(name="setQuoteCurrency",def=function(theObject,quoteCurrency){ standardGeneric("setQuoteCurrency") })
# Implement setter method. Set quoteCurrency
setMethod(f="setQuoteCurrency",signature=c("CurrencyPair","character"),definition=function(theObject, quoteCurrency)
{
  theObject@quoteCurrency <- quoteCurrency
  validObject(theObject)
  return(theObject)
})

setGeneric(name="setTimeSeriesData",def=function(theObject,timeSeriesData){ standardGeneric("setTimeSeriesData") })
# Implement setter method. Set timeSeriesData
setMethod(f="setTimeSeriesData",signature=c("CurrencyPair","list"),definition=function(theObject, timeSeriesData)
{
  theObject@timeSeriesData <- timeSeriesData
  validObject(theObject)
  return(theObject)
})

setGeneric(name="initializeCurrencyPair",def=function(theObject,id,dataFrameBid,dataFrameAsk,beginTimeStamp,endTimeStamp){ standardGeneric("initializeCurrencyPair") })
# Create a currencyPair object by using the input dataframes that represent the information from the bid and ask .csv files
# Only data inside the [beginTimeStamp:endTimestamp] interval is used to create the CurrencyPair object
setMethod(f="initializeCurrencyPair",signature=c("CurrencyPair","character","list","list","POSIXlt","POSIXlt"),definition=function(theObject,id,dataFrameBid,dataFrameAsk,beginTimeStamp,endTimeStamp)
{
    theObject <- setIdentifier(theObject,id)
    theObject <- setBaseCurrency(theObject,substr(id,1,3))
    theObject <- setQuoteCurrency(theObject,substr(id,4,6))
    
    timeStamps = strptime(dataFrameBid$Time,"%Y.%m.%d %H:%M:%S") 
    # Filter for [beginTimeStamp:endTimeStamp interval]
    logicalVector = (timeStamps >= beginTimeStamp & timeStamps <= endTimeStamp)
    # ignore NA values for which POSIXlt conversion is incorrect and comparison with timestamps fails (Note: this does not occur on daily data)
    logicalVector[which(is.na(logicalVector))] = FALSE;
    
    # Add timeseries data for this CurrencyPair obejct to a dataframe
    # - Time: Timestamps in string YYYY.mm.dd HH:MM:SS format
    # - Bid: The bid open price
    # - Voume: Volume indicator. We only use volume information to detect missing data entries.Those entries contain 0 value volumes.
    # - TransactionCosts: This corresponds to the bid/ask spread, which is the current ask minus the current bid value for this particular timestamp
    # --> This value represents the cost expressed in the quote currency pair associated with each unit of the base currency pair that we buy or sell (round trip)
    dataframe = data.frame(Time=dataFrameBid$Time[logicalVector],
                      Bid=dataFrameBid$Open[logicalVector],
                      Volume=dataFrameBid$Volume[logicalVector],
                      TransactionCosts=dataFrameAsk$Open[logicalVector]-dataFrameBid$Open[logicalVector])
    
    # convert timestamps to POSIXlt format
    dataframe$Time <- strptime(dataframe$Time,"%Y.%m.%d %H:%M:%S")
    # set the timeSeriesData
    theObject <- setTimeSeriesData(theObject,dataframe)
    validObject(theObject)
    return(theObject)
})

setGeneric(name="isI1",def=function(theObject,begin,end){ standardGeneric("isI1")})
# This method returns TRUE when the Bid priceseries of this CurrencyPair is integrated of order 1 (I(1)) inside the [begin:end] interval, FALSE otherwise
# ADF with general regression equation is used, assuming that the series has a drift but no linear trend
# We use the BIC information criterion to select the optimal lag length. This criterion is used for consistency reasons since we have big sample sizes --> Will deliver asymptotically correct results (but is inefficient).
# max lag length by Schwert (1989): pMax = floor(12*(T/100)^(1/4)) 
# We compare testvalues with the critical value at the 95% confidence level to generate our conclusions
setMethod(f="isI1",signature=c("CurrencyPair","POSIXlt","POSIXlt"),definition=function(theObject,begin,end)
{
  # We fetch the Bid prices corresponding to the [begin:end] interval
  timeStamps = getTimeSeriesData(theObject)$Time
  logicalVector = (timeStamps >= begin & timeStamps <= end)
  pricesToAnalyze = getTimeSeriesData(theObject)$Bid[logicalVector]
  
  # We perform an ADF test on the levels
  # resultsLevels <- adf.test(pricesToAnalyze,alternative="stationary",k=trunc(length(pricesToAnalyze)^(1/3)))
  resultsLevels <- ur.df(pricesToAnalyze,type="drift",lags=floor(12*(length(pricesToAnalyze)/100)^(1/4)), selectlags="BIC")
  
  # we perform an ADF test on the differenced levels
  diffLevels <- diff(pricesToAnalyze)
  #resultsDiff <- adf.test(diffLevels,alternative="stationary",k=trunc(length(diffLevels)^(1/3)))
  resultsDiff <- ur.df(diffLevels,type="drift",lags=floor(12*(length(diffLevels)/100)^(1/4)),selectlags="BIC")
    
  # The price series are I(1) when:
  # 1) The levels contain a unit root. Hence, the prices series are at least I(1)
  # 2) Differenced prices are stationary. Hence, the price series are not I(2)
  #if(resultsLevels$p.value > c_rejectionLevelADF & resultsDiff$p.value < c_rejectionLevelADF)
  if(resultsLevels@teststat[1] > resultsLevels@cval[1,2] & resultsDiff@teststat[1] < resultsDiff@cval[1,2])
    return(TRUE)
  else
    return(FALSE) 
})

setGeneric(name="containsDataInterval",def=function(theObject,begin,end){ standardGeneric("containsDataInterval") })
# This function returns TRUE when the dataInterval [begin:end] is fully contained in the CurrencyPairs' timeSeriesData, FALSE otherwise
setMethod(f="containsDataInterval",signature=c("CurrencyPair","POSIXlt","POSIXlt"),definition=function(theObject,begin,end)
{
  timeStampSeries = getTimeSeriesData(theObject)$Time
  # If the first timeStamp of the dataseries <= the begin endstamp and the last timeStamp of the dataseries >= the end timestamp,
  # then the data contains the [begin:end] interval and we add the CurrencyObject to the resultlist
  if(timeStampSeries[1] <= begin & timeStampSeries[length(timeStampSeries)] >= end)
    return(TRUE)
  else
    return(FALSE)
})


setGeneric(name="copySubInterval",def=function(theObject,begin,end){ standardGeneric("copySubInterval") })
# This method returns a copy of the CurrencyPair object that only contains available data in the [begin:end] interval
setMethod(f="copySubInterval",signature=c("CurrencyPair","POSIXlt","POSIXlt"),definition=function(theObject,begin,end)
{
  timeStampSeries = getTimeSeriesData(theObject)$Time
  logicalVector = (timeStampSeries >= begin & timeStampSeries <= end)
  copy <- setTimeSeriesData(theObject,getTimeSeriesData(theObject)[logicalVector,])
  
  return(copy)
})

setGeneric(name="plotCurrencyPair",def=function(theObject,begin,end){ standardGeneric("plotCurrencyPair") })
# This method plots the bid prices of this CurrencyPair object
setMethod(f="plotCurrencyPair",signature=c("CurrencyPair","POSIXlt","POSIXlt"),definition=function(theObject,begin,end)
{
  logicalVectorTime <- (getTimeSeriesData(theObject)$Time[1] >= begin & getTimeSeriesData(theObject)$Time <= end)
  
  timeInterval <- getTimeSeriesData(theObject)$Time[logicalVectorTime]
  prices <- getTimeSeriesData(theObject)$Bid[logicalVectorTime]
  plot(timeInterval,prices,type='l',main=getIdentifier(theObject),ylab="Prices",xlab="Time")
  #abline(h=c(mean(prices),mean(prices)+sd(prices),a=mean(prices)+2*sd(prices),mean(prices)-sd(prices),mean(prices)-2*sd(prices)),col=c("green","blue","red","blue","red"))
})


# Inverted Currency pair class for which the base and quote pairs must be inverted. This class inherits from the CurrencyPair class.
# The class Overwrites the initializeCurrencyPair function in order to recalculate inverted timeries and corresponding transaction costs
#
# Purpose of this class: We need to invert data for certain pairs in order to place the USD currency as quote (X/USD)
# This ensures that the value of all our positions can be expressed in USD dollars for consistency reasons.
# Example: if we buy 5 GBP and the GPB/USD quote = 2 then the USD dollar value of our position = 5x2 = 10USD
# Practical trading note: If, according to the trading algorithm we need to buy 1 unit of an inverted pair X/USD then in real trading we will have to sell 1/y units of USD/X, with y being the tradable USD/X quote.
InvertedCurrencyPair <- setClass(
  # Set the name for the class
  "InvertedCurrencyPair",
  
  # Set slots. Empty, no additonal fields
  slots=character(0),
  
  # Default prototype
  prototype=list(),
  
  # Object and data validation checks can be performed here.
  # For simplicity, we will not implement any checks and insure validaty of the data in the main program
  validity=function(object){ return(TRUE) },
  
  # Set the inheritance for this class
  contains = "CurrencyPair"
)

setMethod(f="initializeCurrencyPair",signature=c("InvertedCurrencyPair","character","list","list","POSIXlt","POSIXlt"),definition=function(theObject,id,dataFrameBid,dataFrameAsk,beginTimeStamp,endTimeStamp)
{
  # First, perform standard superclass/CurrencyPair initialization
  theObject <- callNextMethod(theObject,id,dataFrameBid,dataFrameAsk,beginTimeStamp,endTimeStamp)
  
  tempQuote <- getQuoteCurrency(theObject)
  # Invert identifier
  theObject <- setIdentifier(theObject,paste(tempQuote,getBaseCurrency(theObject),sep=""))
  # Invert base and quote
  theObject <- setQuoteCurrency(theObject,getBaseCurrency(theObject))
  theObject <- setBaseCurrency(theObject,tempQuote)
  
  dataframe = getTimeSeriesData(theObject)
  # If we buy one unit of X/USD, this means that in practice we will need to sell 1/Y units of USD/X, with Y = the USD/X quote
  # If we sell 1/Y units of USD/X we incur a transaction cost of 1/Y * transactionCost per unit USD/X
  # However this transaction cost is expressed in currency X and not in USD
  # To express this transaction cost in USD we will have to divide the result by the USD/X quote to get the dollar amount
  # We get: (1/Y * transactionCost per unit USD/X)/Y = (1/Y^2 * transactionCost per unit USD/X)
  dataframe$TransactionCosts <- dataframe$TransactionCosts / (dataframe$Bid^2) # transactioncost per unit of X/USD
  dataframe$Bid <- 1/dataframe$Bid # invert price data
  validObject(theObject)
  theObject <- setTimeSeriesData(theObject,dataframe)
  
  return(theObject)
})
