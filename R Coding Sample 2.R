##############################################
# Econ 217 Homework 4
# By Christina Louie
# March 13, 2017
##############################################

#library packages
library(quantmod)
library(MSBVAR)

##### Problem 1 ######
# --- (a) ---
ARMA <- function(n,phi,theta){
  p <- length(phi)
  q <- length(theta)
  r <- max(p,q)
  es <- rnorm(n+r)
  Y <- rep(0,n+r)
  for (i in (r+1):(r+n)){
    Y[i] <- es[i] + t(phi)%*%Y[(i-1):(i-p)]+t(theta)%*%es[(i-1):(i-q)]
  }
  Y<- Y[-(1:r)]
  return(Y)
}

# The following function answer PART (a), (b), and (c):

plot.calc.ARMA <-function(N){
  ARMA_pq=ARMA(N,c(0.5, 0.2, 0.1),c(0.5, 0.3))
  #plot of ARMA process
  plot(ARMA_pq,type="l",xlab="t",ylab="ARMA: phi=(0.5, 0.2, 0.1), theta=(0.5, 0.3))",
       main=paste("ARMA(3,2) process for N=", N))
  #plot of ACF
  acf(ARMA_pq,lag.max=10,type="correlation")
  #calculate sample mean and variance
  c(mean(ARMA_pq), var(ARMA_pq))
}

par(mfcol=c(2,3))
plot.calc.ARMA(100)
plot.calc.ARMA(1000)
plot.calc.ARMA(10000)


# --- (d) ---
B=1000
for(rep in 1:B){
  a<-plot.calc.ARMA(100)
  sample_Mean <- as.numeric(a[1])
  sample_Var<-as.numeric(a[2])
  if(rep==1){results<-data.frame(rep,sample_Mean,sample_Var)}
  if(rep>1){results<-rbind(results,data.frame(rep,sample_Mean,sample_Var))}
}

quantile(results$sample_Mean,prob=c(0.05,0.95),na.rm=TRUE)
quantile(results$sample_Var,prob=c(0.05,0.95),na.rm=TRUE)


##### Problem 2 ######
# --- (a) ---
# Get Google and Amazon Prices
getSymbols('AMZN',from='2014-04-01',to='2016-01-01')
A_price=AMZN$AMZN.Open

getSymbols('GOOG',from='2014-04-01',to='2016-01-01')
G_price=GOOG$GOOG.Open

# Plot series in a panel chart
par(mfrow=c(2,1))
plot(A_price, main="Amazon Prices")
plot(G_price, main="Google Prices")


# One-lag vector autoregression 
length(G_price) == length(A_price)

n = length(G_price)
regAmazon = lm(A_price[2:n]~G_price[1:(n-1)]+A_price[1:(n-1)])
regGoogle = lm(G_price[2:n]~G_price[1:(n-1)]+A_price[1:(n-1)])

summary(regAmazon)
summary(regGoogle)


# --- (b) ---
#conduct a one-lag Granger causality test, P=1
y2 <- ts(data.frame(A_price,G_price))
granger.test(y2,p=1)


# --- (c) ---
# Use Reduced Form VAR to find impulse response function 
#coefficients of reduced form VAR
coeff <- rbind(as.numeric(coef(regGoogle)),as.numeric(coef(regAmazon)))

#initialize noise term in VAR (first row Google, second row Amazon)
noise <- matrix(nrow=2,ncol=1)
noise[1,1] <-0
#this is the $60 increase in the noise term for the reduced form VAR that
#predicts price for Amazon
noise[2,1] <-60

#initial price (most recent price)
j<-0 # most recent price is at period zero
priceA <- as.numeric(A_price[n])
priceG <- as.numeric(G_price[n])
price <- data.frame(i,priceG,priceA)

#initialize vector in VAR to multiply with coeff
x0<-matrix(nrow=3,ncol=1)
x0[1,1]<-1
x0[2,1]<-priceG
x0[3,1]<-priceA

#iterate for 10 periods (apply the reduced form VAR)
for (j in 1:10){
  if (j==1){
    newprice<-coeff%*%x0+noise
  }
  if(j>1){
    newprice<-coeff%*%x0
  }
  
  #new update price after shock
  priceG<-newprice[1,1]
  priceA<-newprice[2,1]
  price <- rbind(price,data.frame(i,priceG,priceA))
  
  #replace previous initial price with new price for next period
  x0[2,1]<- priceG
  x0[3,1]<- priceA
}

#plot of impulse response function
par(mfcol=c(2,1))
plot(priceA~i,price,type="l",
     ylab="Price",xlab="Time Period",
     main="Impulse Response Function of Amazon")
plot(priceG~i,price,col="magenta", type="l", ylab="Price",xlab="Time Period",
     main="Impulse Response Function of Google")


