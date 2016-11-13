library(quantmod)
library(Quandl)

# METHODOLOGY: 
#   Stocks chosen represent the 20 largest firms, by market-cap, from the 10 industries represented  
#   in S&P 100 Index (2 stocks from each industry), given each firm's stock has traded publicly
#   since January 1, 2000.
#
#   Constituent securities of the S&P 100 Index as well as their industries are sourced from:
#     http://bit.ly/2ffU0Tb and http://bit.ly/2ffS50O

# Read in a csv file that contains the selected tickers and their industries
top20 <- read.csv("C:/Users/jtryker/Documents/R/FIN643/top20_industries.csv", stringsAsFactors = FALSE)

# Define relevant parameters
start_date = "2000-01-01"
end_date = "2016-09-30"
tickers <- as.character(top20$Ticker)

# Use quantmod to query Yahoo finance for monthly returns for each ticker and combine xts objects
returns <- lapply(tickers, function(sym){ monthlyReturn(na.omit(getSymbols(sym, 
                                                                           from = start_date, 
                                                                           to = end_date, 
                                                                           auto.assign = FALSE)))})
returns_merged <- do.call(merge, returns)

# Use Quandl to obtain monthly market/facotr returns and risk-free rates from Ken French's data library
market <- Quandl("KFRENCH/FACTORS_M", start_date = start_date, end_date = end_date)
momentum <- Quandl("KFRENCH/MOMENTUM_M", start_date = start_date, end_date = end_date)

market <- as.xts(market[,-1], order.by = market[,1]) / 100
momentum <- as.xts(momentum[,-1], order.by = momentum[,1]) / 100

rf <- market[,4, drop = FALSE]
mkt <- market[,1, drop = FALSE]
smb <- market[,2, drop = FALSE]
hml <- market[,3, drop = FALSE]
umd <- momentum[,1, drop = FALSE]

# Ensure index positions match across tables
index(returns_merged) <- index(rf)
index(mkt) <- index(rf)
index(smb) <- index(rf)
index(hml) <- index(rf)
index(umd) <- index(rf)

# Subtract risk-free rate from each stock's gross monthly return
returns_rf <- matrix(nrow = nrow(returns_merged), ncol = ncol(returns_merged))

for (i in 1:ncol(returns_merged)) {
  returns_rf[,i] <- returns_merged[,i] - rf
}

returns_rf_xts <- xts(returns_rf, order.by = index(returns_merged))

# Run single factor linear regression against the market factor and extract relevant statistics
alphas <- sapply(1:ncol(returns_rf_xts), function(x) { coef(lm(returns_rf_xts[,x] ~ mkt))[1] } )
betas <- sapply(1:ncol(returns_rf_xts), function(x) { coef(lm(returns_rf_xts[,x] ~ mkt))[2] } )
t_stats <- sapply(1:ncol(returns_rf_xts), 
                  function(x) { summary(lm(returns_rf_xts[,x] ~ mkt))$coefficients[2,3] } )
p_values <- sapply(1:ncol(returns_rf_xts), 
                   function(x) { summary(lm(returns_rf_xts[,x] ~ mkt))$coefficients[2,4] } )

# Create a table from resulting statistics and write to a .csv file
table1 <- data.frame(ticker = tickers,
                     alpha = alphas,
                     beta = betas,
                     t_stat = t_stats,
                     p_value = p_values,
                     stringsAsFactors = FALSE)

table1

# Define parameters, create table and plot Security Market Line
rf_current <- as.numeric(rf[nrow(rf)])
mrp <- mean(mkt, na.rm = TRUE)
mean_r <- sapply(returns_rf_xts, mean, na.rm = TRUE)

table2 <- data.frame(ticker = tickers,
                     beta = betas,
                     exp_ret = mean_r,
                     stringsAsFactors = FALSE)

plot(x = table2$beta, y = table2$exp_ret, 
     col="white", main = "20 Tickers vs SML", 
     xlab = "Market Beta", ylab = "Mean Monthly Return")
abline(a = rf_current, b = mrp, col = "blue")
text(x = table2$beta, y = table2$exp_ret, labels = table2$ticker, cex = 0.7)


# Write query to fetch data for JRSTX (Intech US Managed Volatility mutual fund), plot results, 
# then run four-factor linear regression using Ken French's SMB, HML, and UMB factors
# FMAGX is the ticker for the Fidelity Magellen Fund - 

FMAGX <- monthlyReturn(getSymbols("FMAGX", from = start_date, to = end_date, auto.assign = FALSE))

plot(FMAGX, main = "FMAGX Monthly Returns", ylab = "Monthly Returns")

summary(lm(FMAGX ~ mkt + smb + hml + umd))
