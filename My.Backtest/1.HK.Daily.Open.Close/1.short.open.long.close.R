#+ fig.width = 15, fig.height = 10, echo = FALSE
require(Hmisc)
require(knitr)

file <- file <- "C:/Users/Daniel.yang/Documents/My.Backtest/1.HK.Daily.Open.Close/price.dat"
load(file)

df <- price[rownames(price) > "2001-01-01", c("HSI.Open", "HSI.Close")]
df <- na.omit(df)
ret <- df
colnames(ret) <- c("C2O", "O2C" )
ret$O2C <- df$HSI.Close / df$HSI.Open - 1
ret$C2O <- df$HSI.Open / delay(df$HSI.Close) - 1
portfolio <- ret
# Assuming always long or short 1 contract
portfolio$O2C <- -df$HSI.Open
portfolio$C2O <- delay(df$HSI.Close)
portfolio <- advance(portfolio * 20)
plt <- show.pnl.gg(portfolio, ret, title="HSI.Short.Open.Long.Close", 
                   intraday=TRUE, yearly.pnl=TRUE, hit.ratio=TRUE, max.dd=TRUE)
print(plt)

#analyse HSI and SPX return correlation
hsi.c2c <- log(price$HSI.Close) - delay(log(price$HSI.Close))
hsi.c2o <- log(price$HSI.Open) - delay(log(price$HSI.Close))
hsi.o2c <- log(price$HSI.Close) - log(price$HSI.Open)
spx.c2c <- log(price$SPX.Close) - delay(log(price$SPX.Close))
spx.o2c <- log(price$SPX.Close) - log(price$SPX.Open)
print(paste0("correlation of HSI.c2c and SPX.o2c is ", 
      as.character(round(cor(hsi.c2c, spx.o2c, use="pairwise.complete.obs"), 2))))
print(paste0("correlation of HSI.c2o and SPX.o2c is ", 
      as.character(round(cor(hsi.c2o, delay(spx.o2c), use="pairwise.complete.obs"), 2))))
print(paste0("correlation of HSI.o2c and SPX.o2c is ", 
             as.character(round(cor(hsi.o2c, spx.o2c, use="pairwise.complete.obs"), 2))))

#analyze whether the loss is auto-correlated
pnl <- rowSums(delay(portfolio, 1, 0) * ret, na.rm=TRUE)
indi <- mv.sum(pnl, 10) / mv.sd(pnl, 20) / sqrt(10)
indi[is.na(indi)] <- -Inf
portfolio[indi > 1, ] <- 0
plt <- show.pnl.gg(portfolio, ret, title="HSI.Short.Open.Long.Close.pnl.momentum", 
                   intraday=TRUE, yearly.pnl=TRUE, hit.ratio=TRUE, max.dd=TRUE)
print(plt)
