require(ggplot2)
require(reshape2)
require(grid)
require(gridExtra)
bips <- 0.0001

load.hkif <- function() {
    path <- "C:/Users/Daniel.yang/Documents/R folder/backtest/FutureStrategy/data/HK/"
    df <- read.csv(paste0(path, "D_HK_I&F_Downloading.csv"), header = FALSE, sep = ",", as.is = TRUE, 
                   colClasses = c("character"), skip=0, fill = TRUE)
    dates <- as.character(df[1, -1])
    dates[nchar(dates) < 8] <- paste0("0", dates[nchar(dates) < 8])
    dates <- as.character(as.Date(dates, format="%m/%d/%y"))
    
    df <- read.csv(paste0(path, "D_HK_I&F_Downloading.csv"), header = FALSE, sep = ",", as.is = TRUE, 
                   colClasses = c("character", rep("numeric", length(dates))), skip=1, fill = TRUE, na.string="#N/A")
    price <- t(as.matrix(df[, -1]))
    rownames(price) <- dates
    colnames(price) <- c("hsi.close", "hsi.open", "hsif.close", "hsif.open", "hcei.close", "hcei.open", "hceif.close", "hceif.open", "sp50.close", "sp50.open")
    price
}

load.factset.datadownloading <- function(file, ticker.file=NULL) {
    tmp <- read.csv(file = file, header=FALSE, sep=",", as.is=TRUE, fill=TRUE)
    dates <- as.character(tmp[1, -1])
    dates[nchar(dates) < 8] <- paste0("0", dates[nchar(dates) < 8])
    dates <- as.character(as.Date(dates, format="%m/%d/%y"))
    tmp <- read.csv(file=file, header=FALSE, sep=",", as.is=TRUE, fill=TRUE, skip=1, 
                      colClasses=c("character", rep("numeric", length(dates))), na.strings="#N/A")
    core <- t(as.matrix(tmp[, -1]))
    price <- as.data.frame(core)
    rownames(price) <- dates
    col.name <- tmp[, 1]
    if(is.null(ticker.file)) {
        colnames(price) <- col.name
    } else {
        mapping <- read.csv(file=ticker.file, header=FALSE, sep=",", as.is=TRUE, fill=TRUE)
        tickers <- as.character(mapping[, 2])
        names(tickers) <- paste0(as.character(mapping[, 1]), "^")
        colnames(price) <- tickers[col.name]
    }
    price
}

delay <- function(ob, d=1, fill=NA) {
    if(is.null(dim(ob))) {
        n <- length(ob)
        tmp <- c(rep(fill, d), ob[1:(n-d)])
    } else {
        n <- nrow(ob)
        tmp <- ob
        tmp[] <- fill
        tmp[(d+1):n, ] <- ob[1:(n-d), ]
    }
    tmp
}

delay.data.frame <- function(df, d=1, fill=NA) {
    n <- nrow(df)
    tmp <- df
    tmp[] <- fill
    tmp[(d+1):n, ] <- df[1:(n-d), ]
    tmp
}
delay.matrix <- function(mat, d=1, fill=NA) {
    n <- nrow(mat)
    tmp <- mat
    tmp[] <- fill
    tmp[(d+1):n, ] <- mat[1:(n-d), ]
    tmp
}

advance <- function(ob, d=1, fill=NA) {
    if(is.null(dim(ob))) {
        n <- length(ob)
        tmp <- c(ob[(d+1):n], rep(fill, d))
    } else {
        n <- nrow(ob)
        tmp <- ob
        tmp[] <- fill
        tmp[1:(n-d), ] <- ob[(d+1):n, ]
    }
    tmp
}

mv.sd <- function(vec, lag) {
    tmp <- vec
    tmp[] <- NA
    for(m in (lag:length(vec))) {
        t <- vec[(m-lag+1):m]
        tmp[m] <- sd(t, na.rm=TRUE)
    }
    tmp
}
mv.avg <- function(vec, lag) {
    tmp <- vec
    tmp[] <- NA
    for(m in (lag:length(vec))) {
        t <- vec[(m-lag+1):m]
        tmp[m] <- mean(t, na.rm=TRUE)
    }
    tmp
}

mv.sum <- function(vec, lag) {
    tmp <- vec
    tmp[] <- NA
    for(m in (lag:length(vec))) {
        t <- vec[(m-lag+1):m]
        tmp[m] <- sum(t, na.rm=TRUE)
    }
    tmp
}

mv.min <- function(vec, lag) {
    tmp <- vec
    tmp[] <- NA
    for(m in (lag:length(vec))) {
        t <- vec[(m-lag+1):m]
        tmp[m] <- min(t, na.rm=TRUE)
    }
    tmp
}

mv.max <- function(vec, lag) {
    tmp <- vec
    tmp[] <- NA
    for(m in (lag:length(vec))) {
        t <- vec[(m-lag+1):m]
        tmp[m] <- max(t, na.rm=TRUE)
    }
    tmp
}

cum.max <- function(vec) {
    tmp <- vec
    tmp[] <- NA
    for(m in (1:length(vec))) {
        t <- vec[1:m]
        tmp[m] <- max(t, na.rm=TRUE)
    }
    tmp
}

rsi <- function(price, n=14) {
    price.chg <- price - delay(price, d=1, fill=NA)
    price.chg[is.na(price.chg)] <- 0
    u <- pmax(price.chg, 0, na.rm = TRUE)
    d <- - pmin(price.chg, 0, na.rm = TRUE)
    cum.u <- mv.avg(u, n)
    cum.d <- mv.avg(d, n)
    rsi <- 100 - (100 / (1 + cum.u / cum.d) )
    rsi[1:(n*3)] <- NA
    rsi
}
rsi.abs <- rsi
# wrsi <- function(price, n=14) {
#     w <- (1-1/n) ^ c(n:1)
#     w <- w / sum(w, na.rm=TRUE)
#     price.chg <- price - delay(price, d=1, fill=NA)
#     price.chg[is.na(price.chg)] <- 0
#     u <- pmax(price.chg, 0, na.rm = TRUE)
#     d <- - pmin(price.chg, 0, na.rm = TRUE)
#     wrsi <- price
#     wrsi[] <- NA
#     for(i in (n:length(price))) {
#         tu <- u[(i-n+1):i]
#         td <- d[(i-n+1):i]
#         cum.u <- mean(tu * w, na.rm=TRUE)
#         cum.d <- mean(td * w, na.rm=TRUE)
#         wrsi[i] <- 100 - (100 / (1 + cum.u / cum.d))
#     }
#     wrsi
# }

wrsi <- function(price, n=14) {
    price.chg <- price - delay(price, d=1, fill=NA)
    price.chg[is.na(price.chg)] <- 0
    u <- pmax(price.chg, 0, na.rm = TRUE)
    d <- - pmin(price.chg, 0, na.rm = TRUE)
    umma <- u
    umma[] <- NA
    dmma <- umma
    umma[1] <- u[1]
    dmma[1] <- d[1]
    for(i in (2:length(u))) {
        umma[i] <- ((n-1) * umma[i-1] + u[i]) / n
        dmma[i] <- ((n-1) * dmma[i-1] + d[i]) / n
    }
    wrsi <- 100 - 100/(1 + umma / dmma)
    wrsi[1:(n*3)] <- NA
    wrsi
}

ema <- function(price, n) {
    ma <- price
    ma[] <- NA
    ma[1] <- price[1]
    for( i in (2:length(price))) {
        ma[i] <- ((n-1) * ma[i-1] + price[i]) / n
    }
    ma
}

macd <- function(price, nl, ns, nd) {
    ma.s <- ema(price, ns)
    ma.l <- ema(price, nl)
    dif <- ma.s - ma.l
    dem <- ema(dif, nd)
    data.frame(dif = dif, dem = dem)
}

show.pnl <- function(dates, portfolio, ret, title, hitR.calc = FALSE, options="none", part = NULL, hitR.method = 1) {
    portfolio[is.na(portfolio)] <- 0
    ret[is.na(ret)] <- 0
    if(!is.null(part)) {
        dates <- dates[part]
        portfolio <- portfolio[part]
        ret <- ret[part]
    }
 
    if(options == "long.only") {
        portfolio[portfolio < 0] <- 0
    } else if(options == "short.only") {
        portfolio[portfolio > 0] <- 0
    }
    
    pnl <- portfolio * ret
    cpnl <- cumsum(pnl)
    avg.pnl <- mean(pnl, na.rm=TRUE)
    
    tpnl <- sum(pnl, na.rm=TRUE)
    tvr <- sum(abs(diff(c(0, portfolio, 0))), na.rm=TRUE)
    rot <- tpnl / tvr / bips
    rot <- as.integer(rot*100)/100
    
    inv <- mean(abs(portfolio), na.rm=TRUE)
    roi <- avg.pnl/inv*250*100
    roi <- as.integer(roi*100)/100

    pnl.vol <- sd(pnl, na.rm=TRUE)
    sharpe <- avg.pnl / pnl.vol * sqrt(250)
    sharpe <- as.integer(sharpe*10000)/10000
    
    initial.inv <- max(abs(portfolio), na.rm=TRUE)
    max.cpnl <- cum.max(cpnl)
    max.cpnl[max.cpnl <= 0] <- NA
    dd <- (cpnl+initial.inv) / (max.cpnl+initial.inv) - 1
    max.dd <- max(-dd, na.rm=TRUE)
    max.dd <- as.integer(max.dd*100)

    tpnl <- round(tpnl, 0)
        
    main <- paste0(title, " | rot=", rot, "bips")
    main <- paste0(main, " | roi=", roi, "%")
    main <- paste0(main, " | sharpe=", sharpe)
    main <- paste0(main, " | total.pnl=", tpnl)
    main <- paste0(main, " | max.drawdown=", max.dd, "%")
    
    # calculate pnl for long/short side
    port.long <- portfolio
    port.long[port.long < 0] <- 0
    pnl.long <- port.long * ret
    cpnl.long <- cumsum(pnl.long)
    port.short <- portfolio
    port.short[port.short > 0] <- 0
    pnl.short <- port.short * ret
    cpnl.short <- cumsum(pnl.short)

    if(hitR.calc) {
        if(hitR.method == 1) {
            tmp <- pnl
            tmp[] <- 0
            m <- pnl[1]
            tmp[1] <- m
            for(i in (2:length(pnl))) {
                if(portfolio[i] * portfolio[i-1] <= 0) {
                    m <- pnl[i]
                } else {
                    m <- m + pnl[i]
                }
                tmp[i] <- m
            }
            trade.idx <- (portfolio != 0) & ((advance(portfolio, 1, 0) * portfolio) <= 0)
            trade.pnl <- tmp[trade.idx]
            hitR <- sum(trade.pnl > 0, na.rm=TRUE) / length(trade.pnl) * 100
        } else {
            hitR <- sum(pnl > 0, na.rm=TRUE) / sum(pnl!=0, na.rm=TRUE) * 100
        }
        hitR <- as.integer(hitR)
        main <- paste0(main, " | hit.ratio=", hitR, "%")
    }
    
    layout(matrix(c(1, 1, 2), 3, 1, byrow = TRUE))
    y.min <- min(c(cpnl, cpnl.long, cpnl.short))
    y.max <- max(c(cpnl, cpnl.long, cpnl.short))
    plot(x=dates, y=cpnl, type='l', col='black', xlab='', ylab='Cum pnl', main=main,
         ylim = c(y.min, y.max), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    lines(x=dates, y=cpnl.long, type='l', col='red')
    lines(x=dates, y=cpnl.short, type='l', col='blue')
    grid()
    if(abs(y.min) > abs(y.max)) {
        legend("bottomleft", c("Cum Pnl", "Long Cum Pnl", "Short Cum Pnl"), lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), 
               col = c("black", "red", "blue"))
    } else {
        legend("topleft", c("Cum Pnl", "Long Cum Pnl", "Short Cum Pnl"), lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), 
               col = c("black", "red", "blue"))
    }
    
    plot(x=dates, y=portfolio, type='h', col='blue', xlab='Date', ylab = 'Portfolio', main='',
         cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    
    df <- data.frame(dates = dates, pnl = pnl)
    df$year <- substr(as.character(df$dates), 1, 4)
    yearly.pnl <- aggregate(df$pnl, by = list(df$year), FUN = "sum")
    colnames(yearly.pnl) <- c("year", "pnl per 100 investment")
    print(yearly.pnl)
}

show.pnl.intraday <- function(dates, portfolio, ret, title, hitR.calc = FALSE, options="none", part = NULL) {
    portfolio[is.na(portfolio)] <- 0
    ret[is.na(ret)] <- 0
    if(!is.null(part)) {
        dates <- dates[part]
        portfolio <- portfolio[part]
        ret <- ret[part]
    }
    
    if(options == "long.only") {
        portfolio[portfolio < 0] <- 0
    } else if(options == "short.only") {
        portfolio[portfolio > 0] <- 0
    }
    
    pnl <- portfolio * ret
    cpnl <- cumsum(pnl)
    avg.pnl <- mean(pnl, na.rm=TRUE)
    
    tpnl <- sum(pnl, na.rm=TRUE)
    tvr <- sum(abs(portfolio), na.rm=TRUE)
    rot <- tpnl / tvr / bips
    rot <- as.integer(rot*100)/100
    
    inv <- mean(abs(portfolio), na.rm=TRUE)
    roi <- avg.pnl/inv*250*100
    roi <- as.integer(roi*100)/100
    
    pnl.vol <- sd(pnl, na.rm=TRUE)
    sharpe <- avg.pnl / pnl.vol * sqrt(250)
    sharpe <- as.integer(sharpe*10000)/10000
    
    initial.inv <- max(abs(portfolio), na.rm=TRUE)
    max.cpnl <- cum.max(cpnl)
    max.cpnl[max.cpnl <= 0] <- NA
    dd <- (cpnl+initial.inv) / (max.cpnl+initial.inv) - 1
    max.dd <- max(-dd, na.rm=TRUE)
    max.dd <- as.integer(max.dd*100)
    
    tpnl <- round(tpnl, 0)
    
    main <- paste0(title, " | rot=", rot, "bips")
    main <- paste0(main, " | roi=", roi, "%")
    main <- paste0(main, " | sharpe=", sharpe)
    main <- paste0(main, " | total.pnl=", tpnl)
    main <- paste0(main, " | max.drawdown=", max.dd, "%")
    
    # calculate pnl for long/short side
    port.long <- portfolio
    port.long[port.long < 0] <- 0
    pnl.long <- port.long * ret
    cpnl.long <- cumsum(pnl.long)
    port.short <- portfolio
    port.short[port.short > 0] <- 0
    pnl.short <- port.short * ret
    cpnl.short <- cumsum(pnl.short)
    
#     if(hitR.calc) {
#         hitR <- sum(pnl > 0, na.rm=TRUE) /  sum(portfolio!=0, na.rm=TRUE) * 100
#         hitR <- as.integer(hitR)
#         main <- paste0(main, " | hit.ratio=", hitR, "%")
#     }
    if(hitR.calc) {
        hitR <- sum(pnl > 0, na.rm=TRUE) / sum(pnl!=0, na.rm=TRUE) * 100
        hitR <- as.integer(hitR)
        main <- paste0(main, " | hit.ratio=", hitR, "%")
    }
    
    layout(matrix(c(1, 1, 2), 3, 1, byrow = TRUE))
    y.min <- min(c(cpnl, cpnl.long, cpnl.short))
    y.max <- max(c(cpnl, cpnl.long, cpnl.short))
    plot(x=dates, y=cpnl, type='l', col='black', xlab='', ylab='Cum pnl', main=main,
         ylim = c(y.min, y.max), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    lines(x=dates, y=cpnl.long, type='l', col='red')
    lines(x=dates, y=cpnl.short, type='l', col='blue')
    grid()
    if(abs(y.min) > abs(y.max)) {
        legend("bottomleft", c("Cum Pnl", "Long Cum Pnl", "Short Cum Pnl"), lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), 
               col = c("black", "red", "blue"))
    } else {
        legend("topleft", c("Cum Pnl", "Long Cum Pnl", "Short Cum Pnl"), lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), 
               col = c("black", "red", "blue"))
    }
    
    plot(x=dates, y=portfolio, type='h', col='blue', xlab='Date', ylab = 'Portfolio', main='',
         cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    
    df <- data.frame(dates = dates, pnl = pnl)
    df$year <- substr(as.character(df$dates), 1, 4)
    yearly.pnl <- aggregate(df$pnl, by = list(df$year), FUN = "sum")
    colnames(yearly.pnl) <- c("year", "pnl per 1000 investment")
    print(yearly.pnl)
}
quant_ret <- function(x, y, n, interval=NULL, main="", xlab="") {
    if(is.null(interval)) {
        prob <- seq(0, n, length.out=(n+1)) / n
        interval <- quantile(x, probs = prob, na.rm=TRUE)
    } else {
        n <- length(interval) - 1
    }
    ratio.y <- std.y <- mean.y <- rep(NA, n)
    for(i in (1:n)) {
        t1 <- interval[i]
        t2 <- interval[i+1]
        mean.y[i] <- mean(y[!is.na(x) & x > t1 & x <= t2], na.rm=TRUE)
        std.y[i] <- sd(y[!is.na(x) & x > t1 & x <= t2], na.rm=TRUE)
    }
    ratio.y <- mean.y / std.y
    X11()
#    layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
#    barplot(height = mean.y, names.arg = interval[1:n], main = main, xlab = xlab, ylab = "mean return")
    barplot(height = ratio.y, names.arg = interval[1:n], main = main, xlab = xlab, ylab = "mean return / return stdev")
}



show.pnl2 <- function(dates, portfolio, ret, reference, ref.name, title, hitR.calc = FALSE, options="none", part = NULL) {
    portfolio[is.na(portfolio)] <- 0
    ret[is.na(ret)] <- 0
    if(!is.null(part)) {
        dates <- dates[part]
        portfolio <- portfolio[part]
        ret <- ret[part]
    }
    
    if(options == "long.only") {
        portfolio[portfolio < 0] <- 0
    } else if(options == "short.only") {
        portfolio[portfolio > 0] <- 0
    }
    
    pnl <- portfolio * ret
    cpnl <- cumsum(pnl)
    avg.pnl <- mean(pnl, na.rm=TRUE)
    
    tpnl <- sum(pnl, na.rm=TRUE)
    tvr <- sum(abs(diff(c(0, portfolio, 0))), na.rm=TRUE)
    rot <- tpnl / tvr / bips
    rot <- as.integer(rot*100)/100
    
    inv <- mean(abs(portfolio), na.rm=TRUE)
    roi <- avg.pnl/inv*250*100
    roi <- as.integer(roi*100)/100
    
    pnl.vol <- sd(pnl, na.rm=TRUE)
    sharpe <- avg.pnl / pnl.vol * sqrt(250)
    sharpe <- as.integer(sharpe*10000)/10000
    
    initial.inv <- max(abs(portfolio), na.rm=TRUE)
    max.cpnl <- cum.max(cpnl)
    max.cpnl[max.cpnl <= 0] <- NA
    dd <- (cpnl+initial.inv) / (max.cpnl+initial.inv) - 1
    max.dd <- max(-dd, na.rm=TRUE)
    max.dd <- as.integer(max.dd*100)
    
    tpnl <- round(tpnl, 0)
    
    main <- paste0(title, " | rot=", rot, "bips")
    main <- paste0(main, " | roi=", roi, "%")
    main <- paste0(main, " | sharpe=", sharpe)
    main <- paste0(main, " | total.pnl=", tpnl)
    main <- paste0(main, " | max.drawdown=", max.dd, "%")
    
    # calculate pnl for long/short side
    port.long <- portfolio
    port.long[port.long < 0] <- 0
    pnl.long <- port.long * ret
    cpnl.long <- cumsum(pnl.long)
    port.short <- portfolio
    port.short[port.short > 0] <- 0
    pnl.short <- port.short * ret
    cpnl.short <- cumsum(pnl.short)
    
    if(hitR.calc) {
        tmp <- pnl
        tmp[] <- 0
        m <- 0
        for(i in (1:length(pnl))) {
            if(pnl[i] == 0) {
                m <- 0
            } else {
                m <- m + pnl[i]
            }
            tmp[i] <- m
        }
        trade.idx <- (portfolio != 0) & (advance(portfolio, 1, 0) == 0)
        trade.pnl <- tmp[trade.idx]
        hitR <- sum(trade.pnl > 0, na.rm=TRUE) / length(trade.pnl) * 100
        hitR <- as.integer(hitR)
        main <- paste0(main, " | hit.ratio=", hitR, "%")
    }
    
    layout(matrix(c(1,1,2,2), 4, 1, byrow = TRUE))
    par(mar=c(0.5, 8, 4, 4))
    y.min <- min(c(cpnl, cpnl.long, cpnl.short))
    y.max <- max(c(cpnl, cpnl.long, cpnl.short))
    plot(x=dates, y=cpnl, type='l', col='black', xlab='', xaxt='n', ylab='Cum pnl', main=main,
         ylim = c(y.min, y.max), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    lines(x=dates, y=cpnl.long, type='l', col='red')
    lines(x=dates, y=cpnl.short, type='l', col='blue')
    grid()
    if(abs(y.min) > abs(y.max)) {
        legend("bottomleft", c("Cum Pnl", "Long Cum Pnl", "Short Cum Pnl"), lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), 
               col = c("black", "red", "blue"))
    } else {
        legend("topleft", c("Cum Pnl", "Long Cum Pnl", "Short Cum Pnl"), lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), 
               col = c("black", "red", "blue"))
    }
    par(mar=c(8, 8, 0.5, 4))
    
    plot(x=dates, y=reference, type='l', col='blue', xlab='Date', ylab = ref.name, main='',
         cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
}


cum.plot <- function(alpha, ret, main="", xlab="", ylab=""){
    id <- !is.na(alpha) & !is.na(ret)
    alpha <- alpha[id]
    ret <- ret[id]
    
    ret.qtl <- quantile(ret, probs=c(0.01, 0.99), na.rm=TRUE)
    alpha <- alpha[ret > ret.qtl[1] & ret < ret.qtl[2]]
    ret <- ret[ret > ret.qtl[1] & ret < ret.qtl[2]]
    
    ret <- ret[order(alpha)]
    alpha <- alpha[order(alpha)]
    cret <- cumsum(ret)
    plot(1:length(alpha), cret, main=main, type="l", xlab=xlab, ylab=ylab, xaxt='n')
    axis(1, at = seq(1, length(alpha), length.out=10), round(alpha[round(seq(1, length(alpha), length.out=10))], 4))
}


show.pnl.port <- function(dates, portfolio, ret, title, part = NULL) {
    portfolio[is.na(portfolio)] <- 0
    ret[is.na(ret)] <- 0
    if(!is.null(part)) {
        dates <- dates[part]
        portfolio <- portfolio[part, ]
        ret <- ret[part, ]
    }
    
    pnl <- rowSums(delay.data.frame(portfolio, 1, 0) * ret, na.rm=TRUE)
    pnl[is.na(pnl)] <- 0
    cpnl <- cumsum(pnl)
    avg.pnl <- mean(pnl, na.rm=TRUE)
    
    tpnl <- sum(pnl, na.rm=TRUE)
    tvr <- sum(abs(portfolio - delay.data.frame(portfolio, 1, 0) * exp(ret)), na.rm=TRUE)
    rot <- tpnl / tvr / bips
    rot <- as.integer(rot*100)/100
    daily.tvr <- rowSums(abs(portfolio - delay.data.frame(portfolio, 1, 0) * exp(ret)), na.rm=TRUE)

    inv <- mean(rowSums(abs(portfolio)), na.rm=TRUE)
    tvr.ratio <- mean(daily.tvr, na.rm=TRUE) / inv
    roi <- avg.pnl/inv*250*100
    roi <- as.integer(roi*100)/100
    
    pnl.vol <- sd(pnl, na.rm=TRUE)
    sharpe <- avg.pnl / pnl.vol * sqrt(250)
    sharpe <- as.integer(sharpe*10000)/10000
    
    tpnl <- round(tpnl, 0)
    main <- paste0(title, " | rot=", rot, "bips")
    main <- paste0(main, " | roi=", roi, "%")
    main <- paste0(main, " | sharpe=", sharpe)
    main <- paste0(main, " | total.pnl=", tpnl)
    main <- paste0(main, " | tvr.ratio=", round(tvr.ratio * 100, 2), "%")
    
    # calculate pnl for long/short side
    position <- rowSums(abs(portfolio), na.rm=TRUE)
    port.long <- portfolio
    position.long <- rowSums(port.long, na.rm=TRUE)
    port.long[port.long < 0] <- 0
    pnl.long <- rowSums(delay.data.frame(port.long, 1, 0) * ret, na.rm=TRUE)
    pnl.long[is.na(pnl.long)] <- 0
    cpnl.long <- cumsum(pnl.long)
    port.short <- portfolio
    position.short <- rowSums(port.short, na.rm=TRUE)
    port.short[port.short > 0] <- 0
    pnl.short <- rowSums(delay.data.frame(port.short, 1, 0) * ret, na.rm=TRUE)
    pnl.short[is.na(pnl.short)] <- 0
    cpnl.short <- cumsum(pnl.short)
    
    layout(matrix(c(1, 1, 2), 3, 1, byrow = TRUE))
    y.min <- min(c(cpnl, cpnl.long, cpnl.short))
    y.max <- max(c(cpnl, cpnl.long, cpnl.short))
    plot(x=dates, y=cpnl, type='l', col='black', xlab='', ylab='Cum pnl', main=main,
         ylim = c(y.min, y.max), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    lines(x=dates, y=cpnl.long, type='l', col='red')
    lines(x=dates, y=cpnl.short, type='l', col='blue')
    grid()
    if(abs(y.min) > abs(y.max)) {
        legend("bottomleft", c("Cum Pnl", "Long Cum Pnl", "Short Cum Pnl"), lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), 
               col = c("black", "red", "blue"))
    } else {
        legend("topleft", c("Cum Pnl", "Long Cum Pnl", "Short Cum Pnl"), lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), 
               col = c("black", "red", "blue"))
    }
    
    plot(x=dates, y=position, type='l', col='blue', xlab='Date', ylab = 'Portfolio', main='',
         cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    
    
    df <- data.frame(dates = dates, pnl = pnl)
    df$year <- substr(as.character(df$dates), 1, 4)
    yearly.pnl <- aggregate(df$pnl, by = list(df$year), FUN = "sum")
    colnames(yearly.pnl) <- c("year", "pnl")
    print(yearly.pnl)
}


spx.option.expiry.date <- function(dates) {
    tmp <- tail(dates, 1)
    tmp.vec <- seq(tmp, by="1 day", length.out = 80)[-1]
    dates <- c(dates, tmp.vec)
    tmp <- head(dates, 1)
    tmp.vec <- seq(from=(tmp-30), to=(tmp-1), by="1 day")

    dates <- seq(from=dates[1]-30, to=tail(dates, 1)+90, by="1 day")
    year.month <- substr(as.character(dates), 1, 7)
    expiry.dates <- aggregate(dates, list(year.month), FUN=function(dates) {
        weekday <- weekdays(dates)
        id <- which(weekday == "Friday")
        if(length(id) < 3) {
            expd <- NA
        } else {
            expd <- dates[id[3]]
        }
        as.character(expd)
    })
    expiry.dates <- expiry.dates$x
    expiry.dates <- expiry.dates[!is.na(expiry.dates)]
    expiry.dates <- as.Date(expiry.dates)
    expiry.dates <- expiry.dates[expiry.dates > as.Date("2003-01-01")]
    # expiry.dates[expiry.dates < as.Date("2015-02-01")] <- expiry.dates[expiry.dates < as.Date("2015-02-01")] + 1
    expiry.dates
}


show.pnl.gg <- function(portfolio, ret, title, intraday=FALSE, yearly.pnl=FALSE, hit.ratio=FALSE, max.dd=FALSE) {
    portfolio[is.na(portfolio)] <- 0
    ret[is.na(ret)] <- 0
    
    pnl <- rowSums(delay(portfolio, 1, 0) * ret, na.rm=TRUE)
    pnl[is.na(pnl)] <- 0
    if(hit.ratio) {
        hr <- round(sum(pnl > 0) / sum(pnl != 0) * 100, 2)
        print(paste0("hit ratio is ", hr, "%"))
    }
    cpnl <- cumsum(pnl)
    avg.pnl <- mean(pnl, na.rm=TRUE)
    
    tpnl <- sum(pnl, na.rm=TRUE)
    
    if(intraday) {
        daily.tvr <- rowSums(abs(portfolio), na.rm=TRUE)
        tvr <- sum(abs(portfolio), na.rm=TRUE)
    } else {
        daily.tvr <- rowSums(abs(portfolio - delay(portfolio, 1, 0) * exp(ret)), na.rm=TRUE)
        tvr <- sum(abs(portfolio - delay(portfolio, 1, 0) * exp(ret)), na.rm=TRUE)
    }
    rot <- tpnl / tvr / bips
    rot <- as.integer(rot*100)/100
    
    inv <- mean(rowSums(abs(portfolio)), na.rm=TRUE)
    tvr.ratio <- mean(daily.tvr, na.rm=TRUE) / inv
    roi <- avg.pnl/inv*250*100
    roi <- as.integer(roi*100)/100
    
    pnl.vol <- sd(pnl, na.rm=TRUE)
    sharpe <- avg.pnl / pnl.vol * sqrt(250)
    sharpe <- as.integer(sharpe*10000)/10000
    
    tpnl <- round(tpnl, 0)
    main <- paste0(title, " | rot=", rot, "bips")
    main <- paste0(main, " | roi=", roi, "%")
    main <- paste0(main, " | sharpe=", sharpe)
    main <- paste0(main, " | total.pnl=", tpnl)
    main <- paste0(main, " | tvr.ratio=", round(tvr.ratio * 100, 2), "%")
    if(hit.ratio) {
        hr <- round(sum(pnl > 0) / sum(pnl != 0) * 100, 2)
        main <- paste0(main, "| hit ratio=", hr, "%")
    }
    if(max.dd) {
        max.cpnl <- cum.max(cpnl)
        dd <- max.cpnl - cpnl
        m.dd <- max(dd, na.rm=TRUE)
        m.dd.pos <- which.max(dd)
        m.dd.date <- rownames(portfolio)[m.dd.pos]
        max.pos <- which.max(cpnl[1:m.dd.pos])
        max.date <- rownames(portfolio)[max.pos]
        m.dd <- round(m.dd, 2)
        m.dd <- as.character(m.dd)
        print(paste0("max drawdown is ", m.dd, " on ", m.dd.date, ", with HWM on ", max.date))
    }
    
    # calculate pnl for long/short side
    position <- rowSums(abs(portfolio), na.rm=TRUE)
    port.long <- portfolio
    port.long[port.long < 0] <- 0
    position.long <- rowSums(port.long, na.rm=TRUE)
    pnl.long <- rowSums(delay(port.long, 1, 0) * ret, na.rm=TRUE)
    pnl.long[is.na(pnl.long)] <- 0
    cpnl.long <- cumsum(pnl.long)
    port.short <- portfolio
    port.short[port.short > 0] <- 0
    position.short <- rowSums(port.short, na.rm=TRUE)
    pnl.short <- rowSums(delay(port.short, 1, 0) * ret, na.rm=TRUE)
    pnl.short[is.na(pnl.short)] <- 0
    cpnl.short <- cumsum(pnl.short)
    
    if(yearly.pnl) {
        years <- substr(rownames(portfolio), 1, 4)
        pnl.by.year <- aggregate(pnl, list(years), FUN=sum)
        colnames(pnl.by.year) <- c("year", "pnl")
        print(pnl.by.year)
    }
    
    
    plot.df1 <- data.frame(dates = as.Date(rownames(portfolio)), 
                           cpnl=cpnl, 
                           cpnl.long=cpnl.long, 
                           cpnl.short=cpnl.short)
    plot.df1 <- melt(plot.df1, id="dates")
    plot1 <- ggplot(data=plot.df1) + geom_line(aes(x=dates, y=value, color=variable)) + ggtitle(main) + ylab("cpnl")+xlab("")
    plot1 <- plot1 + theme_grey(base_size = 15)
    plot1 <- plot1 + theme(axis.text.x=element_blank(), axis.title.x=element_blank())
    plot1 <- plot1 + theme(legend.text=element_text(size=15), legend.position=c(0.1, 0.8), legend.title=element_blank(), legend.background=element_blank())
    plot1 <- plot1 + theme(legend.key.size=unit(2,"cm"), legend.key.height=unit(1, "cm"))
    plot1 <- plot1 + ggtitle(main) 
    
    plot.df2 <- data.frame(dates=as.Date(rownames(portfolio)), long=position.long, short=position.short)
    plot.df2 <- melt(plot.df2, id="dates")
    plot2 <- ggplot(data=plot.df2, aes(x=dates, y=value, color=variable)) + geom_bar(stat="identity", position="identity") + ylab("position")
    plot2 <- plot2 + theme_grey(base_size = 15)
    plot2 <- plot2 + theme(legend.text=element_text(size=15), legend.position="none")
    
    
    plot <- grid.arrange(plot1, plot2, layout_matrix=rbind(c(1),c(1),c(2)))
    plot
}



cum.plot.event <- function(ticker.ret, flag_event, n_back, n_forward, main, vol.adj=FALSE) {
    event_pos <- which(flag_event)
    event_id <- flag_event
    event_id[] <- NA
    for(i in c(-n_back:n_forward)) {
        pos <- event_pos + i
        pos <- pos[pos > 0 & pos <= length(flag_event)]
        event_id[pos] <- i
    }
    agg.ret <- aggregate(ticker.ret, by=list(event_id), FUN=mean, na.rm=TRUE)
    plot.df1 <- data.frame(offset = agg.ret$Group.1, 
                           cum.ret = cumsum(agg.ret$x))
    plot1 <- ggplot(data=plot.df1) + geom_line(aes(x=offset, y=cum.ret)) + ggtitle(main) + ylab("cumulative return")+xlab("")
    plot1 <- plot1 + theme_grey(base_size = 15)
    
    if(vol.adj) {
        agg.vol <- aggregate(ticker.ret, by=list(event_id), FUN=sd, na.rm=TRUE)
        plot.df1 <- data.frame(offset = agg.ret$Group.1, 
                               vol.adj.ret = agg.ret$x/agg.vol$x)
        plot2 <- ggplot(data=plot.df1, aes(x=offset, y=vol.adj.ret)) + geom_bar(stat="identity", position=position_dodge())
        plot2 <- plot2 + ggtitle("") + ylab("volatility adjusted return")+xlab("")
        plot2 <- plot2 + theme_grey(base_size = 15)
        plot <- grid.arrange(plot1, plot2, layout_matrix=rbind(c(1),c(2)))
    } else {
        plot <- plot1
    }
    plot
}