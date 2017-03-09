# install.packages('zoo')
# install.packages('quantmod')
# install.packages('PerformanceAnalytics')
# install.packages('ROI')
# install.packages("Rcmdr")
# install.packages("ROI.plugin.glpk")
# install.packages("ROI.plugin.quadprog")

library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)
library(xts)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)
library(ROI)
library(dplyr)
library(tidyr)
library(reshape2)


###### Maximum Overlap Intervals ######
### Import dataframe
AB<-data.frame(id=seq(10,80,by=10),anest=c("baker","baker",rep("dow",6)), start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))
### Construct AB_2 to store the intersection information (s) 
AB$s=0;AB$w=NULL
AB_1=melt(AB,id.vars =c("id",'anest'))
ac=as.integer(AB_1$variable)
ac[ac==2]=-1
A_1<-cbind(AB_1,ac)[AB$anest=='dow',]
B_1<-cbind(AB_1,ac)[AB$anest=='baker',]
A_2<-A_1[order(A_1[,4],A_1[,1]),];A_2$stack=cumsum(c(A_2$ac))
B_2<-B_1[order(B_1[,4],B_1[,1]),];B_2$stack=cumsum(c(B_2$ac))
AB_2 <- rbind(A_2,B_2)
### Add w column for AB_2 to store all intersected id
l <- array()
for (i in 1:length(AB_2$id)){
  if (AB_2$variable[i]=='start'){
    l<-append(l,AB_2$id[i])}
  else if (AB_2$variable[i]=='end'){
    l<-l[!l %in% AB_2$id[i]]
  }
  AB_2$w[i]<- list(sort(l))
}
### Define simple function to find the index of id pair
mid_true <- function(l){
  which(l %in% TRUE)
}
### Summarize all information and append new s, w column to original dataframe AB
for (i in AB$id){
  s <- mid_true(AB_2$id==i)
  AB$s[AB$id==i] <- max(AB_2$stack[s[1]:s[2]])
  AB$w[AB$id==i] <- toString(sort(unique(unlist(AB_2$w[s[1]:s[2]]))))
}


###### Pascal Triangle ######
pascalTriangle <- function(h) {
  lapply(0:h, function(i) choose(i, 0:i))
}


###### Calculate VaR and CVaR ######
### Define the symbols and download historical data
Symbols<-c("AAPL","IBM","GOOG","BP","XOM","COST","GS")
Weights<-c(0.15,0.2,0.2,0.15,0.1,0.15,0.05)
from.dat <- as.Date("01/01/16", format="%m/%d/%y")
to.dat <- as.Date("12/31/16", format="%m/%d/%y")
portfolio<-getSymbols(c("AAPL","IBM","GOOG","BP","XOM","COST","GS"), src="yahoo", from = from.dat, to = to.dat)
### Calculate the daily return
Stocks <- lapply(Symbols, function(sym) {
  dailyReturn(na.omit(getSymbols(sym, src="yahoo", from = from.dat, to = to.dat, auto.assign=FALSE)))
})
Stocks.r <- do.call(merge, Stocks)
colnames(Stocks.r) <- c("AAPL","IBM","GOOG","BP","XOM","COST","GS")
### Construct Portfolio Return given weight
portfolio.r <- data.frame(apply(Stocks.r,1, function(x) x%*%Weights))
colnames(portfolio.r) <- "portfolio return"
### Historical VaR and CVaR
VaR(portfolio.r,p=0.95,method="historical")
ETL(portfolio.r,p=0.95,method="historical")
### parametric  VaR and CVaR
VaR(portfolio.r,p=0.95,method="gaussian")
ETL(portfolio.r,p=0.95,method="gaussian")

### Assumption_1： we assume that we want to minimize portfolio variance and optimze the portfolio weight
port <- portfolio.spec(assets = c("AAPL","IBM","GOOG","BP","XOM","COST","GS")) # Start with the names of the assets
### Set the contrain to be full investment; minimize portfolio risk and no leverage
port <- add.constraint(portfolio = port, type = "full_investment")
port <- add.objective(portfolio = port, type = "risk", name = "var")
port <- add.constraint(portfolio = port, type = "box", min = -1, max = 1)

### Monthly rebalancing with 19 days training period
portfolio.rebalanced <- optimize.portfolio.rebalancing(R = Stocks.r,
                                                       portfolio = port,
                                                       optimize_method = "ROI",
                                                       rebalance_on = "months",
                                                       training_period = 19)
### Optimzed weight under assumption 1
opt_weight_1 <- portfolio.rebalanced$opt_rebalancing

### Assumption_2： we assume that we want to maximize mean return and optimze the portfolio weight
port_2 <- portfolio.spec(assets = c("AAPL","IBM","GOOG","BP","XOM","COST","GS")) # Start with the names of the assets
### Set the contrain to be full investment; minimize portfolio risk and no leverage
port_2 <- add.constraint(portfolio = port_2, type = "full_investment")
port_2 <- add.objective(portfolio = port_2, type = "return", name = "mean")
port_2 <- add.constraint(portfolio = port_2, type = "box", min = -1, max = 1)

### Monthly rebalancing with 19 days training period
portfolio.rebalanced_2 <- optimize.portfolio.rebalancing(R = Stocks.r,
                                                       portfolio = port_2,
                                                       optimize_method = "ROI",
                                                       rebalance_on = "months",
                                                       training_period = 19)
### Optimzed weight under assumption 2
opt_weight_2 <- portfolio.rebalanced_2$opt_rebalancing


###### Position calculator ######
setwd("C:/q/practice")
pos <- read.csv(file='pos.csv',head=TRUE,sep=",")

### Q1 Calculate the net position per each user
net_pos <- pos %>% 
                group_by(user) %>%
                summarise(net_pos = sum(pos))
### Q2 Calculate the boxed position
box_pos <- pos %>% spread(sym, pos)
box_A_list <- colnames(box_pos)[!is.na(colnames(box_pos)[box_pos[1,]*box_pos[2,]<0])]
box_B_list <- colnames(box_pos)[!is.na(colnames(box_pos)[box_pos[3,]*box_pos[4,]<0])]
box_C_list <- colnames(box_pos)[!is.na(colnames(box_pos)[box_pos[5,]*box_pos[6,]<0])]
box_D_list <- colnames(box_pos)[!is.na(colnames(box_pos)[box_pos[7,]*box_pos[8,]<0])]
box_E_list <- colnames(box_pos)[!is.na(colnames(box_pos)[box_pos[9,]*box_pos[10,]<0])]
length(box_E_list) = length(box_D_list) = length(box_C_list) = length(box_B_list) = length(box_A_list)
total_list <- cbind(box_A_list,box_B_list,box_C_list,box_D_list,box_E_list)
### Q3 Calcualte journal and trade columns
trd <- read.csv(file='trd.csv',head=TRUE,sep=",")
trd <- trd[order(trd['sym']),]

trd_qty <- trd %>% spread(sym, qty)
# Sperate the negative and positive value and sum up, then append them to former trd_qty data frame
row_append <- as.matrix(vapply(trd_qty[,-1], function(x)  c(sum(x[x>0], na.rm=TRUE), -sum(x[x<=0], na.rm=TRUE)), double(2L)))
trd_qty <- rbind(trd_qty[,-1],row_append[1:2,])
row_trd <- vapply(trd_qty[6:7,], function(x) min(x), double(1L))
trd_qty <- rbind(trd_qty, row_trd)
# We make an assumption that when the order cannot be fully executed, we use pro rata to split the executed order
trd_qty[9,]<-lapply(trd_qty, function(X) X[8]/X[6])
trd_qty[10,]<-lapply(trd_qty, function(X) X[8]/X[7])
new_trd_qty <- cbind(user = c('A','B','C','D','E','po','neg','trd','po_ratio','neg_ratio'),trd_qty)
ratio.df <- new_trd_qty[9:10,] %>% gather(sym, qty, `1003.T`:`1984.T`)
# Implement the calculation: positive order * po_ratio and negative order * neg_ratio
for (i in (1:length(trd$sym))){
  if (trd$qty[i]>0){
    trd$jrnl[i]=trd$qty[i]*ratio.df[(ratio.df$sym==trd$sym[i]),]$qty[1]
    trd$trd[i]=trd$qty[i] - trd$jrnl[i]
  }
  else if (trd$qty[i]<0){
    trd$jrnl[i]=trd$qty[i]*ratio.df[(ratio.df$sym==trd$sym[i]),]$qty[2]
    trd$trd[i]=trd$qty[i] - trd$jrnl[i]
  }
}
### Q4 Find the total quantity to trade
total_trd <- trd %>% group_by(sym) %>% summarise(Total_trd = sum(trd))

### Q5 Final position
# We make the assumption that, the final position should be total position for each sym plus the total traded order
total_pos_user <- pos %>% group_by(sym,user) %>% summarise(Total_pos = sum(pos))
total_trd_user <- trd %>% group_by(sym,user) %>% summarise(Total_trd = sum(trd))
final_pos <- merge(x=total_pos_user,y=total_trd_user, by=c("sym","user"),all = TRUE)
final_pos[is.na(final_pos)]<-0
final_pos$final_pos <- final_pos$Total_pos+final_pos$Total_trd

### Q6 The question description is very vague, and we could check the solution quality from serveral perspective:
### First is the data quality, we should cross check our data from mutli-sources. Second is the defination, we should 
### check the definition of journal and trades, and compare our theoretical result with the market data. Third, we shoul loose
### our assumption and make our analysis close to the practice. For example, we cannot assume that all the orders arrive at
### the same time. 



