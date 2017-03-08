# install.packages('zoo')
# install.packages('quantmod')
install.packages('PerformanceAnalytics')
library(zoo)
library(quantmod)
library(PerformanceAnalytics)

df_overlap <- data.frame(id=seq(10,80,by=10),anest=c("baker","baker",rep("dow",6)), start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))
df_overlap$start[1]


a1 = ts(rnorm(40), start=c(2001,1), freq=12)
a2 = ts(rnorm(15), start=c(2002,1), freq=12)
a3 = ts(rnorm(40), start=c(1999,1), freq=12)
List1 = list(a1,a2,a3)
List2 <- setNames(List1, c("a1", "a2", "a3"))
as.ts(do.call("merge", Map(as.zoo, c(List2, all = FALSE))))


pascalTriangle <- function(h) {
  for(i in 0:(h-1)) {
    s <- ""
    for(k in 0:(h-i)) s <- paste(s, "  ", sep="")
    for(j in 0:i) {
      s <- paste(s, sprintf("%3d ", choose(i, j)), sep="")
    }
    print(s)
  }
}

### Pascal Triangle
pascalTriangle <- function(h) {
  lapply(0:h, function(i) choose(i, 0:i))
}



### Calculate VaR
# Define the symbols
Symbols<-c("AAPL","IBM","GOOG","BP","XOM","COST","GS")

from.dat <- as.Date("01/01/16", format="%m/%d/%y")
to.dat <- as.Date("12/31/16", format="%m/%d/%y")
portfolio<-getSymbols(c("AAPL","IBM","GOOG","BP","XOM","COST","GS"), src="yahoo", from = from.dat, to = to.dat)
VaR(GOOG,p=0.95,method="historical")

Stocks = lapply(Symbols, function(sym) {
  dailyReturn(na.omit(getSymbols(sym, src="yahoo", from = from.dat, to = to.dat, auto.assign=FALSE)))
})
do.call(merge, Stocks)

Stocks = lapply(Symbols, function(x) x)



