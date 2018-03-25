#install.packages("rugarch")
library(rugarch)

snp500 = read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)
S<- as.matrix(subset(snp500, select = - c(Date, VIX)))

Y <- diff(log(S))

fitGarch <- function(LogDiff){
  
  resids <- function(LogDiff){
    gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="std")
    gfit.ru <- ugarchfit(gspec.ru, LogDiff)
    mu = gfit.ru@fit$solver$sol$pars[[1]]
    df = gfit.ru@model$pars["shape", 1]
    sigmas = gfit.ru@fit$sigma
    rsd = (LogDiff - mu)/sigmas
    print(sigmas[3407])
    return( append(rsd, c(df,mu,sigmas[3407])) )
  }
  
  X= apply(t(Y), 1,resids)
  df = X[3408,]
  mu= X[3409,]
  sigmas = X[3410,]
  X = X[0:3408,]
  Gfit <- list("df"=df, "mu"=mu, "sigma"=sigmas, "X"=X)
  return(Gfit)
  
} 


normalize <- function(x){
  df = tail(x,1)[[1]]
  n = length(x)
  x = x[1:(n-1)]
  z <- qnorm(pt(x, df = df))
  qqnorm(z)
  qqline(z, col = 2, lwd = 2, lty = 2)
  return(z)
}

Z = apply(t(X),1,normalize)
COR = cor(Z)


getParams <- function(Y){
  GARCH = fitGarch(Y)
  Z = apply(t(GARCH$X),1,normalize)
  COR = cor(Z)
  COV = cov(Z)
  params = list("COR"=COR, "df" = GARCH$df, "mu" = GARCH$mu, "sigma" = GARCH$sigma, "COV"=COV )
  return(params)
}

params = getParams(Y)

# suppose we want to generate a k day trajectory
# then we need start by generating our normalized model errors
# we need k of them. Lets pretend k=10
options(digits=4)
library(mvtnorm)
studentize <- function(Z,df){
  Z=rbind(Z,df)
  
  fnhelper <- function(z){
    df=tail(z,1)
    z=z[0:(length(z)-1)]
    z = qt(pnorm(z), df=df)
    return(z)
  }
  return(apply(Z,2,fnhelper))
  
}

z_10 <- rmvnorm(10,mean=rep(0,46),sigma=params$COV)



#now we need to convert these error terms into student t error terms
z_10_t = studentize(z_10, params$df)

y_10 = params$mu + t(t(z_10_t)*params$sigma)

#log returns become simple returns (i.e. the percentage of previous return)
#now we want to calculate the price of the assets ten days from now

S_10 = apply(exp(y_10),2,prod) *S[dim(S)[1],]

returns = S_10-S[dim(S)[1],]

plot(returns, type='n')
text(1:46, returns, colnames(S))

print("hi")
# now we need to take the value of all the assets at day t: S_t, and 


#asdfasdf








