library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(quantmod)
library(zoo)
require(Rmosek)

### Process and clean the data
setwd('C:\\Users\\Chandler\\Desktop\\Millennium')
stock_raw <- read.csv('stock.csv')
# Manipulate the structure and count the non-na value
stock <- stock_raw[,c(3,4,5)] %>% spread(tic, prccd)
stock <- rbind(stock, apply(stock, 2, function(x) length(which(!is.na(x)))))
# Delete insufficient ticker
stock <- stock[,(stock[nrow(stock),]>(max(stock[nrow(stock),-1])-1))]
stock <- stock[-1,]
# Replace NA value with mean
stock_new <- na.aggregate(stock[complete.cases(stock),])
# Calculate return
cal_return<-function(x){
  diff(x)/x[-length(x)]
}
### Output to csv file
stock_return<-na.aggregate(apply(stock_new[,-1], 2, cal_return))# Fix NA for return table
stock_return <- cbind(stock_new[-1,1],stock_return)
write.table(stock_return[-nrow(stock_return),],'stock_return.csv',sep = ",")

ptm <- proc.time()
### Read the data from csv file
return.raw <- as.matrix(read.csv('stock_return.csv',header=T,sep=",",
                                 stringsAsFactors=FALSE))
# Append the month-year indicator for monthly balance
return.raw <- data.frame(substr(return.raw[,'X'],1,6),return.raw)



# Minimum risk Markowitz portfolio selection example
#
# minimize f
# subject to r '*(w0+  x) = t
# G*(w0 + x) = g , ||g||_2 < f
# sum(x) = 0 .
# and optionally w0 + x >= 0
# or equivalently
#
# minimize f
# subject to [r' 0 0] [x] [t-r'*w0]
# [G 0 -I] *[f] = [-G*w0]
# [e' 0 0] [g] [0]
# ||g||_2 <= f
# and optionally w0 + x >= 0

### Define the mean variance optimization function, using conic quodratic optimization
mean.variance.optimization <- function(X){
  ### Estimating the mean and variance
  mu_r <-colMeans(X)
  # Sigma_r <-cov(X,use='complete.obs')
  # Normalize data???matrix for conic formulations of risk
  N <- nrow(X); n<-ncol(X)
  # Calculate the Xbar decomposition matrix and replace NA with approximated value
  Xbar <-na.aggregate(1/sqrt(N-1)*(X-matrix(1,nrow=N)%*%mu_r))
  G<-as.matrix(Xbar)
  G[is.infinite(G)] <-0 # replace all non-finite value with 0
  
  rT<-t(colMeans(X))
  rT[,is.infinite(rT)] <-0 # replace all non-finite value with 0
  
  p <- nrow(G); n<-ncol(G)
  w0 <- matrix(1/n, nrow=n) # initial holdings
  t <- 1.005 # Expected return
  
  # We assume we can do short selling
  SHORTSELLING <- 1 ;
  
  prob <- list(sense="minimize")
  
  # Build up the conic optimization problem
  prob$A <- rbind(
    cbind(rT, Matrix(0, ncol=p+1)),
    cbind(G,Matrix(0,nrow=p),-diag(p)),
    cbind(Matrix(1,ncol=n),Matrix(0,ncol=p+1))
  )
  
  # Define boundary and constraint
  prob$c <- c(rep(0,n),1,rep(0,p));
  prob$cones <- cbind(list('MSK_CT_QUAD',(n+1):(n+1+p)))
  b<-c(t-rT%*%w0,-G%*%w0,0); # Set expected return and min risk constraint
  prob$bc<-rbind(b,b);
  prob$bx<-matrix(c(-1,1),nrow=2,ncol=n+1+p); # no leverage
  
  if (SHORTSELLING==0){
    prob$bx[1,]<-c(-w0,rep(-1,1+p));
  }
  
  # Do mosek optimization
  prob$iparam <- list(LICENSE_WAIT = "MSK_ON");
  res<-mosek(prob)
  x<-res$sol$itr$xx[1:n];
  f<-res$sol$itr$xx[n+1]
  result <- list(x,f)
  return(result)
}


### Execuate optimzation process
# Define output variable
monthly.weight <- data.frame(colnames(return.raw)[c(-1,-2)])
min.risk <- data.frame("minimized.risk")
# Calculate the monthly rebalance weight and minimized risk 
for (month in unique(return.raw[,1])){
  X <- as.matrix(return.raw[return.raw[,1]==month,c(-1,-2)])
  X[is.infinite(X)] <-0 # get rid of infinite element
  result <- mean.variance.optimization(X)
  # clean the output format and save them to seperate data frame
  monthly.weight <- data.frame(monthly.weight,result[1])
  min.risk <- data.frame(min.risk,result[2])
}

colnames(monthly.weight) <- c("TICKER",as.character(unique(return.raw[,1]),format("%Y%m")))
colnames(min.risk) <- c("Name",as.character(unique(return.raw[,1])))

proc.time() - ptm
