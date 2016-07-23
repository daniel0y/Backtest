file <- "C:/Users/Daniel.yang/Documents/My.Backtest/1.HK.Daily.Open.Close/data.csv"
price <- load.factset.datadownloading(file)
save(price, file = "price.dat")