# if(!require(installr)) {install.packages("installr"); require(installr)}
# updateR()
# packages = c("forecast","tseries","urca","vars","xts","PerformanceAnalytics")
# install.packages(packages)

library(forecast)
library(tseries)
library(urca)
library(vars)
library(PerformanceAnalytics)

source("classes\\CurrencyPair.R")
source("classes\\Portfolio.R")
source("classes\\MeanReversionStrategy.R")
source("functions\\functions.R")

### DATA CONFIGURATION ###
# Define location of datafiles. Current working directory is used as relative root
c_dataFolder = ".\\data"

# Define frequency of trading algorithm
# Options are "MINUTE_1", "MINUTE_5", "MINUTE_15", "HOURLY_1", "HOURLY_4" and DAILY"
c_frequency="DAILY"
c_timeDifference <- switch(c_frequency,
                           "MINUTE_1"=as.difftime("00:01:00","%H:%M:%S",units="mins"),
                           "MINUTE_5"=as.difftime("00:05:00","%H:%M:%S",units="mins"),
                           "MINUTE_15"=as.difftime("00:15:00","%H:%M:%S",units="mins"),
                           "HOURLY_1"=as.difftime("01:00:00","%H:%M:%S",units="hours"),
                           "HOURLY_4"=as.difftime("04:00:00","%H:%M:%S",units="hours"),
                           "DAILY"=ISOdate(1950,1,2)-ISOdate(1950,1,1))

# Data interval. We only fetch and analyze data that lies inside the [c_minimumTimeStamp c_maximumTimeStamp] interval
c_minimumTimeStamp = strptime("2008.01.01 00:00:00","%Y.%m.%d %H:%M:%S")
c_maximumTimeStamp = strptime("2014.06.01 00:00:00","%Y.%m.%d %H:%M:%S")

# Indication of currency pairs that can potentially be traded, as defined by one of the vectors below
c_pairSelection = "MAJORS"

# Define currencies that can be traded. One of the vectors below is selected through the c_pairSelection parameter
c_majorCurrencies=c("EURUSD","GBPUSD","USDJPY","AUDUSD","USDCHF","NZDUSD","USDCAD"); # c_pairSelection = MAJORS
c_ATCBrokersCurrencies=c("AUDUSD","EURUSD","GBPUSD","NZDUSD","USDCAD","USDCHF","USDCNH","USDHKD","USDJPY",
                         "USDMXN","USDRUB","USDSEK","USDSGD","XAGUSD","XAUUSD") # c_PairSelection = ATC
c_customCurrencies=c("EURUSD","USDCHF") # c_pairSelection = CUSTOM

# RejectionLevels Configuration
c_rejectionLevelADF = 0.01
c_rejectionLevelJohansen = 0.10 # not used..

## COINTEGRATIONSTRATEGY CONFIGURATION
c_maxCurrenciesInPortfolio = 4 # maximum number of currency pairs that can be combined in one CointegrationTestPortfolio
c_entryZscore = 1 # Entry deviation
c_exitZscore = 0 # Exit deviation