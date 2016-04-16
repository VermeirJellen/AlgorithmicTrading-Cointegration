## Cointegration-Based Statistical Arbitrage Trading Strategy 
This project consists of a proof of concept and conceptual demo of a simple stat-arb trading strategy that is based upon cointegration methods. View `./paper/Cointegration-Based spread trading applied to the foreign exchange market.pdf` for a theoretical overview on the relevant statistical methods. The conceptual demo in this project implements the trading strategy that is explained in Chapter 4 of the paper. It is important to note the academic nature of the project: The demo trading results contain a severe lookahead bias because the strategy is executed on in-sample data. Hence, the trading results will most likely not correspond with realistic out of sample trading.

### Running the Demo
The entrance point to the demo is `./demo.R` and the functionality is well documented inside the script. Following functionality is executed:

1. A portfolio containing currency pair objects is created from the `.csv` files which are provided in the `./data` directory. It should be noted that all relevant timeseries are recreated in such a way that the USD currency pair becomes the quoted pair, for consistency reasons.
2. A particular data window is selected and the individual time series are plotted (line 23)
3. The timeseries are tested for integration of order 1: I(1)
4. The I(1) Currency pairs are grouped in smaller cointegrationTestPortfolioCollection objects. Cointegration tests are performed on these subgroups one by one, as follows:
	- We obtain the optimal VAR lag length by searching for the VAR model that fits the timeseries data in the portfolio optimally, based upon the SC criterion.
	- We use this lag length to launch a Johansen cointegration procedure test on the portfolio.
	- We use the trace statistic to determine the cointegration properties of the portfolio.
	- Some additional properties such as the half life of mean reversion are further determined for the  cointegrating portfolios.
5. For this partcular demo run we determine that for 16 out of the 91 tested portfolios, the null hypothesis of no cointegrating vectors is rejected with more than 90% confidence.
6. For these particular portfolios we use the "optimal" cointegrating vectors with highest eigenvalues and we use the relevant vectorvalues as our hedge ratio's.
7. We use the hedge ratio's to create spreads for these cointegrating portfolios and output some graphs for illustration purposes (line 75)
8. We execute a simple mean reversion strategy on the cointegrating spread and output the results for Portfolio(AUDUSD/CADUSD) (Line 82)
9. We execute a simple mean reversion strategy on the cointegrating spread and output the results for Portfolio(AUDUSD/CADUSD/NZDUSD/JPYUSD/ (Line 92). We note that the latter portfolio has stronger cointegrating properties and a lower half life of mean reversion. As expected, The results -as expressed by the Sharpe ratio- for this portfolio are better (Transaction costs / bid-ask spreads are included in the analysis).

### Trading Results
View the `./images` folder for graphical illustrations of the trading results.

### Troubleshooting
In the event of problems or errors while running the code, make sure that your R version is up to date and all the required subpackages that are mentioned in `./config.r` are installed properly. Uncommenting and executing the first 4 lines in `./config.R` should resolve most potential problems that might occur. Also view the extended comments inside the `./classes` and `./functions` files for additional insight on the technical implementation details.

### Licensing
Copyright 2015 Jellen Vermeir.
<jellenvermeir@gmail.com>	

Cointegration-Based Statistical Arbitrage Trading Strategy is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Cointegration-Based Statistical Arbitrage Trading Strategy is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along with Cointegration-Based Statistical Arbitrage Trading Strategy. If not, see <http://www.gnu.org/licenses/>. 

Following people are given credit for the co-authoring of the attached paper:

- Thys Lynsey
- Van Overloop Hans
- Van Drom Karen
- Vanstreels Dieter
- Trippaers Veerle

