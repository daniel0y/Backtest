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


wrap.pnl <- function(ticker, l.in, l.out, s.in, s.out, z.score, price, ret, is.reversal=FALSE) {
    alpha <- z.score[, ticker, drop=FALSE]
    portfolio <- alpha
    portfolio[] <- 0
    position <- portfolio
    for(i in (2:nrow(portfolio))) {
        if(is.na(alpha[i, 1])) next
        if(position[i-1, 1] == 0) {
            if(alpha[i, 1] > s.in) {
                position[i, 1] <- -1
            } else if(alpha[i, 1] < l.in) {
                position[i, 1] <- 1
            }
        } else if(position[i-1, 1] > 0) {
            if(alpha[i, 1] > l.out) {
                if(alpha[i, 1] > s.in) {
                    position[i, 1] <- -1
                } else {
                    position[i, 1] <- 0
                }
            } else {
                position[i, 1] <- position[i-1, 1]
            }
        } else if (position[i-1, 1] < 0) {
            if(alpha[i, 1] < s.out) {
                if(alpha[i, 1] < l.in) {
                    position[i, 1] <- 1
                } else {
                    position[i, 1] <- 0
                }
            } else {
                position[i, 1] <- position[i-1, 1]
            }
        }
    }
    portfolio <- position * price[, ticker, drop=FALSE]
    if(is.reversal) {
        portfolio <- -portfolio
    }
    plt <- show.pnl.gg(portfolio, ret[, ticker, drop=FALSE], 
                       title=paste0(ticker, " 1w reversal"), yearly.pnl=TRUE, hit.ratio = TRUE)
    print(plt)
}


ticker <- "KOSPI200"
l.in <- -1
l.out <- -0.9
s.in <- 1
s.out <- 0.9
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)



ticker <- "FTSE100"
l.in <- -1
l.out <- -0.9
s.in <- 1
s.out <- 0.9
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)


ticker <- "CAC40"
l.in <- -0.7
l.out <- -0.5
s.in <- 0.7
s.out <- 0.5
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)


ticker <- "SP500"
l.in <- -0.7
l.out <- -0.5
s.in <- 1
s.out <- 0.8
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)


ticker <- "VIX"
l.in <- -1
l.out <- -0.7
s.in <- 1.0
s.out <- 0.7
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)


ticker <- "VX00"
l.in <- -1
l.out <- -0.7
s.in <- 1.0
s.out <- 0.7
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)

ticker <- "VIX_HSI"
l.in <- -Inf
l.out <- Inf
s.in <- 0.8
s.out <- 0.6
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)


ticker <- "VIX_ASX"
l.in <- -1
l.out <- -0.7
s.in <- 1
s.out <- 0.7
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)

ticker <- "VSTOXX"
l.in <- -1.2
l.out <- -1
s.in <- 1.2
s.out <- 1
wrap.pnl(ticker, l.in, l.out, s.in, s.out, z.score, price, ret)
