#+ fig.width = 15, fig.height = 10, echo = FALSE
setwd("~/My.Backtest/2.Reversal.Based.On.Z.score/Equity.Index")
options(digits=4)
require(ggplot2)
source("C:/Users/Daniel.yang/Documents/R folder/backtest/functions.R")

file <- "./Equity.Index.csv"
ticker.file <- "./tickers.csv"
price <- load.factset.datadownloading(file, ticker.file)
ret <- log(price) - log(delay(price))

# show cum.plot pattern
ret.5d <- apply(ret, MARGIN = 2, FUN=mv.sum, lag=5)
ret.vol <- apply(ret, MARGIN = 2, FUN=mv.sd, lag=20)
z.score <- ret.5d / ret.vol / sqrt(5)
for(i in (1:ncol(price))) {
    cum.plot(delay(z.score[, i]), ret[, i], main=paste0("5d reversal-", colnames(price)[i]), 
             xlab = "5d return zscore", ylab = "forwarding 1d return")
}
