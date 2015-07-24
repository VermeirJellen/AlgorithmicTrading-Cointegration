# This function returns a Portfolio containing all tradable CurrencyPair objects
# Default settings are defined in the config.R file
# (Note: This function assumes that both bid and ask prices for a currency pair are present in the data folder)
getMarketData <- function(frequency=c_frequency,dataFolder=c_dataFolder,nrTimeStamps=c_maxNrTimeStamps,
                                pairSelection=c_pairSelection,minimumTimeStamp=c_minimumTimeStamp,maximumTimeStamp=c_maximumTimeStamp){
  
  # freq corresponds to a substring that is contained in the .csv filename and is used to differentiate between data of different frequency's
  # Note: for this project we only look at DAILy data. View default config.
  freq = switch(c_frequency,
          "MINUTE_1"="1 Min",
          "MINUTE_5"="5 Mins",
          "MINUTE_15"="15 Mins",
          "HOURLY_1"="Hourly",
          "HOURLY_4"="4 Hours",
          "DAILY"="Daily")
  
  # We fetch the filenames of all bid and ask datafiles that corresopnd to this particular frequency
  # Through default config, It is asssumed that the data files are contained in the .\\data subdirectory of the current working directory
  fileNamesBid_All = list.files(dataFolder, pattern=paste(freq,".*Bid",sep=""));
  fileNamesAsk_All = list.files(dataFolder, pattern=paste(freq,".*Ask",sep=""));
  
  # selection contains an array of currency pair strings that corresponds to the pairs that we want to trade
  selection = switch(c_pairSelection,
                     "MAJORS"= c_majorCurrencies,
                     "ATC" = c_ATCBrokersCurrencies,
                     "CUSTOM" = c_customCurrencies)
  
  # We filter out the .csv files that contain the name of the tradable currency pairs that we selected
  fileNamesBid_Selected = fileNamesBid_All[substr(fileNamesBid_All,1,6) %in% selection]
  fileNamesAsk_Selected = fileNamesAsk_All[substr(fileNamesAsk_All,1,6) %in% selection]
  
  # we read the .csv's of the tradable pairs and create CurrencyPair objects. These objects are added to a Portfolio
  nrOfSelectedCurrencyPairs <- length(fileNamesBid_Selected)
  tradablePairs <- vector("list",nrOfSelectedCurrencyPairs) # Create vector holding the CurrencyPair objects
  for(i in seq(1,nrOfSelectedCurrencyPairs))
  {
    currencyDataFrameBid = read.csv(file=paste(dataFolder,"\\",fileNamesBid_Selected[i],sep=""),header=TRUE,sep=",") # read bid data for currency pair
    currencyDataFrameAsk = read.csv(file=paste(dataFolder,"\\",fileNamesAsk_Selected[i],sep=""),header=TRUE,sep=",") # read ask data for currency pair
    
    identifier <- substr(fileNamesBid_Selected[i],1,6) # Fetch name of the pair from the .csv filename
    if(identical(substr(identifier,4,6),"USD")) # USD as quote: we create a CurrencyPair class
      tradablePairs[[i]] <- initializeCurrencyPair(CurrencyPair(),identifier,currencyDataFrameBid,currencyDataFrameAsk,minimumTimeStamp,maximumTimeStamp)
    else # USD as base: we create an InvertedCurrencyPair class
      tradablePairs[[i]] <- initializeCurrencyPair(InvertedCurrencyPair(),identifier,currencyDataFrameBid,currencyDataFrameAsk,minimumTimeStamp,maximumTimeStamp)
    
  }
  # return a Portfolio containing all tradable CurrencyPair objects
  return(Portfolio(currencyPairList=tradablePairs))
}

# This function takes two timestamps as inputs and returns the number of bars between those two timestamps
# The number of databars depends on the datafrequency, as defined in the configuration
calculateNrBars <- function(maxStamp,minStamp)
{
  nrBars <- switch(c_frequency,
                             "MINUTE_1"=as.numeric(difftime(maxStamp,minStamp,units="mins")),
                             "MINUTE_5"=as.numeric(difftime(maxStamp,minStamp,units="mins"))/5,
                             "MINUTE_15"=as.numeric(difftime(maxStamp,minStamp,units="mins"))/15,
                             "HOURLY_1"=as.numeric(difftime(maxStamp,minStamp,units="hours")),
                             "HOURLY_4"=as.numeric(difftime(maxStamp,minStamp,units="hours"))/4,
                             "DAILY"=as.numeric(difftime(maxStamp,minStamp,units="days"))
  )
  return(nrBars)
}


# Lag function for vectors.
# Note: Function was found online and it was copied here
vect_lag <- function(v, n=1, forward=FALSE) {
  if (forward)
    c(v[(n+1):length(v)], rep(NA, n))
  else
    c(rep(NA, n), v[1:(length(v) - n)])
}

# Lag function for matrices
# Note: Function was foudn online and it was copied here
lag.matrix <- function(x, k=1){
  N <- ncol(x)
  l <- matrix(embed(x,k+1)[, -c(1:(k*N))], ncol=N)
  NAs <- matrix(rep(NA, k*N), ncol=N)
  rbind(NAs, l)
}

# Repeats the last non NA value. Keep leading NA
# Note: Function was found online and it was copied here
fillMissingValues = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}   

# R equivalent of matlab repmat
# Note: Function was found online and was copied here
repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

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
  par(mfrow=c(1,1))
}