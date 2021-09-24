#Using stocks for minimum variance

# install.packages("timeSeries")
# install.packages("fPortfolio")
# install.packages("quantmod")
# install.packages("ggplot2")

library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(magrittr)

setwd("C:/Users/vinod/OneDrive - C 1/Vinod/PGPBABI/Financial and Risk Analytics/GroupAssignment")

######################STEP ONE: Create Returns Time Series#########################################

#Create Vector of Tickers
tickers <- c('NESTLEIND.BO','BHARTIARTL.BO','INFY.BO','HINDUNILVR.BO','TCS.BO')

##Benchamark Returns
benchmarkPrices <- getSymbols.yahoo("BSE-100.BO", from="2018-06-01", periodicity = "daily", auto.assign=FALSE)[,4]
colSums(is.na(benchmarkPrices))
benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))


#Calculate Returns: Daily
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2018-06-01", auto.assign=FALSE)[,4])

#Delete all dates with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

#Calculate Returns: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
portfolioReturns <- as.timeSeries(portfolioReturns)

#Calculate Monthly or Weekly Returns
Stock_Data <- tickers %>% lapply(function(x) getSymbols.yahoo(x, from="2018-06-01", auto.assign=FALSE)[,4]) %>%
  lapply(function(x) monthlyReturn(x))

portfolioReturns <- do.call(merge, Stock_Data)
# keep only the dates that have closing prices for all tickers
portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
colnames(portfolioReturns) <- tickers
portfolioReturns <- as.timeSeries(portfolioReturns)

#################STEP TWO: Calculate and Plot Frontier and Efficient Portfolios##############


# calculate the efficient frontier
effFrontier <- portfolioFrontier(portfolioReturns, constraints = "LongOnly")

# plot frontier
#'Options
#'1: Plot Efficient Frontier
#'2: Plot Minimum Variance Portfolio
#'3: Plot Tangency Portfolio
#'4: Plot Risk Returns of Each Asset
#'5: Plot Equal Weights Portfolio
#'6: Plot Two Asset Frontiers (Long)
#'7: Plot Monte Carlo Portfolios
#'8: Plot Sharpe Ratio

plot(effFrontier,c(1,2,3,4))

#Plot Frontier Weights (Can Adjust Number of Points)
frontierWeights <- getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
colnames(frontierWeights) <- tickers
risk_return <- frontierPoints(effFrontier)
write.csv(risk_return, "C:/Users/vinod/OneDrive - C 1/Vinod/PGPBABI/Financial and Risk Analytics/GroupAssignment/risk_return.csv")

#Output Correlation
cor_matrix <- cor(portfolioReturns)
cov_matrix <- cov(portfolioReturns)
write.csv(cov_matrix, "covmatrix.csv")

#Annualize Data
riskReturnPoints <- frontierPoints(effFrontier) # get risk and return values for points on the efficient frontier
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints[,"targetReturn"] * 252)
plot(annualizedPoints)

# plot Sharpe ratios for each point on the efficient frontier
riskFreeRate <- 0.065
plot((annualizedPoints[,"targetReturn"]-riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")

#Plot Frontier Weights (Need to transpose matrix first)
barplot(t(frontierWeights), main="Frontier Weights", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(frontierWeights))

#get weights for max returns
# defaultSpec <- portfolioSpec()
# setTargetRisk(defaultSpec) <- 0.065
# setSolver(defaultSpec)= "solveRglpk.CVAR"
# lppData <- portfolioData(data = portfolioReturns, spec = defaultSpec)
# port <- maxreturnPortfolio(lppData, defaultSpec, constraints = "Short")
# port

tangencyPort <- tangencyPortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
tangencyPort

mvpweights <- getWeights(mvp)
tangencyweights <- getWeights(tangencyPort)


#Extract value at risk
covRisk(portfolioReturns, mvpweights)
varRisk(portfolioReturns, mvpweights, alpha = 0.05)
cvarRisk(portfolioReturns, mvpweights, alpha = 0.05)

#Plot MVP Weights: Basic Graphs
# barplot(mvpweights, main="Minimum Variance Portfolio Weights", xlab="Assset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(frontierWeights))
barplot(mvpweights, main="Minimum Variance Portfolio Weights", xlab="Asset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2),horiz = FALSE)
pie(mvpweights, col=cm.colors(ncol(frontierWeights)+2))

#ggplot MVP Weights
df <- data.frame(mvpweights)
assets <- colnames(frontierWeights)
ggplot2::ggplot(data=df, ggplot2::aes(x=assets, y=mvpweights, fill=assets)) +
  ggplot2::geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(ggplot2::aes(label=sprintf("%.02f %%",mvpweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

dft <- data.frame(tangencyweights)
assets <- colnames(frontierWeights)
ggplot(data=dft, aes(x=assets, y=tangencyweights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tangencyweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

#ggplot Pie
bar <- ggplot(df, aes(x = "", y = mvpweights, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("Minimum Variance Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()

bar <- ggplot(dft, aes(x = "", y = tangencyweights, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()

###########################Examine Constraints and Stats#####################################
#Example Constraints:
#"minW[asset]=percentage" for box constraints resp.
#"maxsumW[assets]=percentage" for sector constraints.
#eqsumWConstraints(data, spec=portfolioSpec(), constraints="LongOnly")

#Set Specs
Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
setTargetRisk(Spec) = .12
# constraints <- c("minW[1:length(tickers)]=-1","maxW[1:length(tickers)]=.60", "Short")

constraints <- c('minW[tickers]=0', 'maxW[tickers]<.30')
effFrontierShort <- portfolioFrontier(portfolioReturns, Spec, constraints = constraints)
weights <- getWeights(effFrontierShort)
write.csv(weights, "weightsShort.csv")
colnames(weights) <- tickers

plot(effFrontierShort, c(1, 2, 3))

#Plot Frontier Weights (Need to transpose matrix first)
barplot(t(weights), main="Frontier Weights", col=cm.colors(ncol(weights)+2), legend=colnames(weights))

effPortShort <- minvariancePortfolio(portfolioReturns, Spec, constraints=constraints)
optWeights <- getWeights(effPortShort)
tanPortShort <- tangencyPortfolio(portfolioReturns, Spec, constraints=constraints)
tanWeights <- getWeights(tanPortShort)
maxR <- maxreturnPortfolio(portfolioReturns , Spec, constraints=constraints)
maxWeights <- getWeights(maxR)

#ggplot MVP Weights
df <- data.frame(tanWeights)
assets <- colnames(frontierWeights)
ggplot(data=df, aes(x=assets, y=tanWeights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tanWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio With Shorts Allowed")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

#####################################
#Calculate Beta and other metrics
#####################################
#use weights from the portfolio optimization solution (MVP portfolio)
weights <- c(.3064, .1215, .2806, .1271, .1644) #Weights for Opt returns
#Calculate Portfolio Returns
dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)

#Plot Performance
chart.CumReturns(portfolioReturn)
charts.PerformanceSummary(portfolioReturn)

#Calculate Metrics using 6.5% as the risk free rate
CAPM.beta(portfolioReturn, benchmarkReturns, .065/252)
CAPM.beta.bull(portfolioReturn, benchmarkReturns, .065/252)
CAPM.beta.bear(portfolioReturn, benchmarkReturns, .065/252)

CAPM.alpha(portfolioReturn, benchmarkReturns, .065/252)
CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .065/252)

SharpeRatio(portfolioReturn, Rf = .065/252, p = 0.95, FUN = "StdDev",
            weights = NULL, annualize = FALSE)

table.AnnualizedReturns(portfolioReturn, Rf=.065/252, geometric=TRUE)
table.CalendarReturns(portfolioReturn)

