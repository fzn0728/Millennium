library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(quantmod)
library(zoo)

setwd('C:\\Users\\Chandler\\Desktop\\Millennium')
stock_raw <- read.csv('stock.csv')
# Manipulate the structure
stock <- stock_raw[,c(3,4,5)] %>% spread(tic, prccd)
stock <- rbind(stock, apply(stock, 2, function(x) length(which(!is.na(x)))))
# Delete insufficient ticker
stock <- stock[,(stock[513,]>500)]
stock <- stock[1:512,]

# Put NA value with mean
stock_new <- na.aggregate(stock)
# Calculate return
cal_return<-function(x){
  diff(x)/x[-length(x)]
}
# Output to csv file
stock_return<-apply(stock_new, 2, cal_return)
write.table(stock_return,'stock_return.csv')

### Estimating the mean and variance
mu_r <-colMeans(stock_return)
Sigma_r <-cov(stock_return)

# Normalize data???matrix for conic formulations of risk