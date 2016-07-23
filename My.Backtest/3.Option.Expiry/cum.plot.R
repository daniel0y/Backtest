#+ fig.width = 15, fig.height = 10, echo = FALSE
setwd("~/My.Backtest/3.Option.Expiry")
options(digits=4)
require(ggplot2)
source("C:/Users/Daniel.yang/Documents/R folder/backtest/functions.R")

file <- "./Equity.Index.csv"
ticker.file <- "./tickers.csv"
price <- load.factset.datadownloading(file, ticker.file)
ret <- log(price) - log(delay(price))
dates <- as.Date(rownames(price))
exp.dates <- spx.option.expiry.date(dates)
exp.dates <- exp.dates[exp.dates >= dates[1] & exp.dates <= tail(dates, 1)]
flag_event <- (rownames(price) %in% as.character(exp.dates))
n_back <- 7
n_forward <- 7


for(i in (1:ncol(price))) {
    ticker <- colnames(price)[i]
    ticker.ret <- ret[, ticker]
    main <- ticker
    # show cum.plot pattern
    plot <- cum.plot.event(ticker.ret, flag_event, n_back, n_forward, main, vol.adj = TRUE)
    print(plot)
}

