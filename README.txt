Proof of concept: In sample cointegration-based statistical arbitrage trading strategy.

For a short overview on the relevant statistical methods, view "Cointegration-Based spread trading applied to the foreign exchange market.pdf", Chapters 1-3. 
View chapter Chapter 4, starting on page 6 for a quick overview on the implemented trading strategy.

Entrance point to the project is "demo.R" and the functionality is well documented inside the code. The script executes the following functionality in order:
1) A portfolio containing currency pair objects is created from the .csv files which are provided in the ./data directory. It should be noted that all relevant timeseries are recreated in such a way that the USD currency pair becomes the quoted pair, for consistency reasons.
2) A particular data window is selected and the individual time series are plotted (line 23)
3) The timeseries are tested for integration of order 1: I(1)
4) The I(1) Currency pairs are grouped in smaller cointegrationTestPortfolioCollection objects. Cointegration tests are performed on these subgroups one by one, as follows:
- We obtain the optimal VAR lag length by searching for the VAR model that fits the timeseries data in the portfolio optimally, based upon the SC criterion.
- We use this lag length to launch a Johansen Cointegration procedure test on the portfolio
- We use the trace statistic to determine the cointegration properties of the portfolio
- Some additional properties such as the half life of mean reversion are further determined for the cointegrating portfolios
5) For this partcular demo run we determine that for 16 out of the 91 tested portfolios, the null hypothesis of no cointegrating vectors is rejected with more than 90% confidence.
6) For these particular portfolios we use the "optimal" cointegrating vectors with highest eigenvalues and we use the relevant vectorvalues as our hedge ratio's.
7) We use the hedge ratio's to create spreads for these cointegrating portfolios and output some graphs for illustration purposes (line 75)
8) We execute a simple mean reversion strategy on the cointegrating spread and output the results for Portfolio(AUDUSD/CADUSD) (Line 82)
9) We execute a simple mean reversion strategy on the cointegrating spread and output the results for Portfolio(AUDUSD/CADUSD/NZDUSD/JPYUSD/ (Line 92). We note that the latter portfolio has stronger cointegrating properties and a lower half life of mean reversion. As expected, The results -as expressed by the Sharpe ratio- for this portfolio are better (Transaction costs / bid-ask spreads are included in the analysis).

Images containing the relevant results of the demo script are added to the main folder. In case of problems or errors while running the code, make sure that your R version is up to date and all the required subpackages that are mentioned in config.r are installed properly. Uncommenting and executing the first 4 lines in config.R should resolve most potential problems that might occur. Also view the extended comments inside the ./classes and ./functions files for more information on the technical implementation details. Separate README documents might be included at a later time to provide more detailed information.

Project code is copyrighted by Jellen Vermeir and available for distribution under the FreeBSD license conditions. Following people are also credited for the co-authoring of the attached paper:
- Thys Lynsey
- Van Overloop Hans
- Van Drom Karen
- Vanstreels Dieter
- Trippaers Veerle

Feel free to contact me about this project via jellenvermeir@gmail.com

