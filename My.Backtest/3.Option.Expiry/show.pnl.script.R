ticker_ret <- ret[, ticker, drop=FALSE]
ticker_price <- price[, ticker, drop=FALSE]
portfolio <- ticker_ret
portfolio[] <- 0
if(!is.null(l_back) & !is.null(l_forward)) {
  for(i in c(l_back:l_forward)) {
    pos <- pos_event + i
    pos <- pos[pos > 0 & pos <= nrow(portfolio)]
    portfolio[pos, ] <- 1
  }
}
if(!is.null(s_back) & !is.null(s_forward)) {
  for(i in c(s_back:s_forward)) {
    pos <- pos_event + i
    pos <- pos[pos > 0 & pos <= nrow(portfolio)]
    portfolio[pos, ] <- -1
  }
}
portfolio <- portfolio * ticker_price
agg.port[, ticker] <- portfolio
plot <- show.pnl.gg(portfolio, ticker_ret, title=ticker,
                    yearly.pnl=TRUE, hit.ratio=TRUE)