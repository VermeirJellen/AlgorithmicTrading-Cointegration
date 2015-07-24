rm(list=ls())
# setwd("E:\\Master Financial And Actuarial Engineering\\Statistics for Finance and Insurance\\Projectdemo")
# Note: Working directory should be the path where demo.R is located
source("config.R")

# fetch a Portfolio that contains all tradable CurrencyPair objects (full dataset representing "the FX market")
tradablePairsPortfolio = getMarketData()

# fech the data interval that we want to analyze from configuration
begin <- c_minimumTimeStamp
end <- c_maximumTimeStamp

# We can only backest CurrencyPairs that have full available data starting from the start of the data interval up until the end of the data interval
# we create a Portfolio with those pairs and continue with only those pairs
pairsWithDataInTimeWindow <- getAvailablePairsInTimeWindow(tradablePairsPortfolio,begin,end)

# Only keep data in [begin end] interval
# Goal is to avoid data overhead during processing
marketData <- copySubIntervalPortfolio(pairsWithDataInTimeWindow,begin,end)

# We plot the currency pair price series
# We notice that the series do not look very stationary
plotCurrencyPairsSeparately(marketData,begin,end)

# We will now test whether or not all individual pairs in the portfolio are I(1)
if(individualPairsI1(cointegrationTestPortfolioCopyConstructor(CointegrationTestPortfolio(),marketData),begin,end))
  print("All currency pairs in the portfolio are I(1)")


# All possible combinations of CurrencyPair objects in the marketData Portfolio are added together in testable CointegrationTestPortfolio objects
# For these Portfolio's Cointegration tests will be performed.
# Note: There can be up to c_maxCurrenciesInPortfolio CurrencyPair objects in 1 CointegrationTestPortfolio: view config
cointegrationTestPortfolioCollection = list() # Initialize the resultList. This list will be filled with CointegrationTestPortfolio objects
for(i in seq(2,c_maxCurrenciesInPortfolio))
{
  # Fetch the list of CurrencyPair objects that are contained in the MarketData Portfolio
  cList <- getCurrencyPairList(marketData)
  # Create table of all "n choose k" combinations, with n = the number of CurrencyPair objects in the Portfolio and k = i
  combinations = t(combn(length(cList),i))
  for(j in seq(1,dim(combinations)[1])) # For every combination, fet ch the corresponding currency pairs and perform a CointegrationTest on the portfolio
  {
    # create a Portfolio containing z CurrencyPairs for which we want to perform the test
    toTest = vector("list",dim(combinations)[2])
    for(z in seq(1:dim(combinations)[2])) # Note: dim(combinations)[2] == i
    {
      # Add the currency pairs that correspond to this particular combination to the list
      toTest[[z]] = cList[[combinations[j,z]]]
    }
    
    # create CointegrationTestPortfolio that contains the CurrencyPair objects that correspond to the current combination
    testPortfolio <- CointegrationTestPortfolio(currencyPairList=toTest)
    # Clean the data
    cleanedTestPortfolio <- cleanPortfolio(testPortfolio,begin,end)
    
    # Check if individual pairs in the portfolio are I(1) --> already done
    #if(individualPairsI1(cleanedTestPortfolio,begin,end))
    #{
      # Perform the johansen procedure for cointegration on the Portfolio and store the tested Portfolio in the list
      cointegrationTestPortfolioCollection[[length(cointegrationTestPortfolioCollection)+1]] <- performJohansenProcedureForCointegration(cleanedTestPortfolio,begin,end)
    #}
  }
}

print(paste(length(cointegrationTestPortfolioCollection)," Portfolio's were tested for cointegration",sep=""))
# We only retain CointegrationTestPortfolio objects for which the Portfolio is cointegrated
cointegratedPortfolioList <- list()
for(i in 1:length(cointegrationTestPortfolioCollection))
{
  if(isCointegrated(cointegrationTestPortfolioCollection[[i]]))
    cointegratedPortfolioList[length(cointegratedPortfolioList)+1] <- cointegrationTestPortfolioCollection[[i]]
}
print(paste(length(cointegratedPortfolioList), " Portfolio's are cointegrated", sep=""))

# Plot the spreads of the cointegrated Portfolio's
plotCointegratedSpreads(cointegratedPortfolioList)

# We take a look at the AUDUSD versus CADUSD portfolio and notice the relationship between the price series
plotSpread(cointegratedPortfolioList[[2]],TRUE)
plotCurrencyPairsSeparately(cointegratedPortfolioList[[2]],begin,end)
plotCurrencyPairsTogether(cointegratedPortfolioList[[2]],begin,end)
# We execute a mean reversion strategy on the Portfolio
returns <- executeStrategy(MeanReversionStrategy(portfolio=cointegratedPortfolioList[[2]]))


# When NZDUSD and JPYUSD are added to the cointegrating relationship the half life of mean reversion seems to improve.
# We execute the mean version strategy on this portfolio. We notice that we obtain a better sharpe ratio.
par(mfrow=c(1,1))
plotSpread(cointegratedPortfolioList[[14]],TRUE)
plotCurrencyPairsSeparately(cointegratedPortfolioList[[14]],begin,end)
plotCurrencyPairsTogether(cointegratedPortfolioList[[14]],begin,end)
# We execute a mean reversion strategy on the Portfolio
returns <- executeStrategy(MeanReversionStrategy(portfolio=cointegratedPortfolioList[[14]]))