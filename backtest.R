# backtest.R
# Mike Bond
#
# Test a single stock symbol for a set of trading rules over a particular period versus
# a simple buy-and-hold strategy, as well as a buy-and-hold of a default stock or fund like the S&P 500.
#
# Currently locked to Bollinger Bands. Next update to add other selectors available in TTR package.


require(quantmod)

# Calculate the rolling pct return over a vector of prices
# Args: price vector, trading signal: 1 is long, 0 is out of market, -1 is short
# Returns: cumulative vector of returns, based on signals
calc_returns <- function(prices, sig=1) {
  signaledReturns <- ROC(Cl(prices))*sig
  signaledReturns[which(is.na(signaledReturns))] = 0   # Clean blanks
  exp(cumsum(signaledReturns))   # 
}

#################################################

# Stock symbol to test versus a default for comparison
symbol <- "AAPL"
versus <- "SPY"

period <- "2 years"
strategy <- "BBands"
lowerBound <- 0  # Oversold trigger.
upperBound <- 1.2  # Overbought trigger.

# Get data for each symbol
primary <- getSymbols(symbol, auto.assign=FALSE)
primary <- last(primary, period)   # Filter by time
vers <- getSymbols(versus, auto.assign=FALSE)
vers <- last(vers, period)

# Create indicator
indicators <- BBands(Cl(primary))   # TTR function
indicators <- last(indicators, period)
price_plus <- cbind(primary, indicators)

# If indicator goes below buy signal (oversold), set to 1 (go long)
buy <- Lag(ifelse(indicators$pctB < lowerBound, 1, NA))
sig <- buy[]

# Sell signals
sell_action = 0  # 0 for sell, -1 for short, NA for hold
sell <- Lag(ifelse(indicators$pctB > upperBound, sell_action, NA))

# Combined signal
sig[which(!is.na(sell))] = sell_action  # Replace NA w/sell sig.
sig <- na.locf(sig, na.rm = TRUE)

# Calc returns for all 3 strategies
eq <- calc_returns(primary, sig)  # The trading rules/equity curve
buy_hold <- calc_returns(primary) # Buy and hold 
colnames(buy_hold) <- c(paste(symbol,"Buy/Hold"))
vers_eq <- calc_returns(vers)    # Default strategy

print(paste("Results for ", symbol))

if (length(eq) > 0) {  # If any signals generated a trade
  all_equity <- merge(pri=eq, buyhold=buy_hold, vers=vers_eq)
  print(paste("Strategy return: ", last(eq)))
  colnames(all_equity) <- c(paste(symbol,strategy),paste(symbol,"Buy-Hold"),versus)
} else {  # No signals generated, only buy and hold primary/versus
  all_equity <- merge(buyhold=buy_hold, vers=vers_eq)
  print("No trades for strategy")
  colnames(all_equity) <- c(paste(symbol,"Buy-Hold"),versus)
}

print(paste("Buy/Hold: ", last(buy_hold)))
print(paste(versus, ":", last(vers_eq)))

title=paste("Performance",symbol,strategy)
print(plot(all_equity, main=title, legend.loc = "topleft", legend.names = legend))


