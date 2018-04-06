#install.packages("rugarch")
library(rugarch)

#' fitGarch
#' @param LogDiff Matrix of daily log differences in asset prices.
#' @return List of the following parameters fit to a garch model: df, mu, sigma, and X (the residuals)
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

#' normalize
#' @param x Matrix of student t residuals
#' @return Matrix of corresponding normal residuals
normalize <- function(x){
  df = tail(x,1)[[1]]
  n = length(x)
  x = x[1:(n-1)]
  z <- qnorm(pt(x, df = df))
  qqnorm(z)
  qqline(z, col = 2, lwd = 2, lty = 2)
  return(z)
}

#' getParams
#' @param Y Matrix of daily log differences in asset prices.
#' @return List of parameters of GARCH GC model: COR, df, mu, sigma, COV
getParams <- function(Y){
  GARCH = fitGarch(Y)
  Z = apply(t(GARCH$X),1,normalize)
  COR = cor(Z)
  COV = cov(Z)
  params = list("COR"=COR, "df" = GARCH$df, "mu" = GARCH$mu, "sigma" = GARCH$sigma, "COV"=COV )
  return(params)
}



# suppose we want to generate a k day trajectory
# then we need start by generating our normalized model errors
# we need k of them. Lets pretend k=10
options(digits=4)
library(mvtnorm)


#' normalize
#' @param Z Matrix of MVN residuals
#' @param df Vector of degrees of freedom associated with the student t distributions that were fit to each asset.
#' @return Matrix of corresponding student residuals
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

basictest <- function(){
  snp500 = read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)
  S<- as.matrix(subset(snp500, select = - c(Date, VIX)))
  
  Y <- diff(log(S))
  
  params = getParams(Y)
  
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
}


#' K trajectories
#' @param S Matrix of asset prices. Assumed that each column name is the name of the asset in that column.
#' @param Params List of parameters from fitted GARCH(1, 1) models for each asset. (COR, DF, MU, SIGMA).
#' @param k Natural number, number of timesteps ahead to forecast asset values.
#' @param pct_return Boolean, TRUE = give returns as percentage of current value, FALSE = actual change in stock value.
#' @return Vector containing the forcasted returns. Either as a percentage or current value or simply change in price of stock.
k_trajectories <- function(S, params, k, as_percentage = FALSE){
  
  #Number of timesteps in data, number of assets
  final_timestep = dim(S)[1]
  n_assets = dim(S)[2]
  asset_names = colnames(S)
  
  #Generate model errors from MVN, convert to student t scale
  z_k = rmvnorm(k, rep(0, n_assets), params$COR[asset_names, asset_names])
  z_k_t = studentize(z_k, df = params$df[asset_names])
  
  #Estimates for change in value at each of the k forcasted time steps,
  #as the log of the ratio of the last given value in time series
  y_k = params$mu[asset_names] + z_k_t * params$sigma[asset_names]
  
  #Calculate ratio of final to last given value over k timesteps
  if(k == 1){
    return_ratio = exp(y_k)
  }else{
    return_ratio = apply(exp(y_k), 2, prod)
  }
  
  #Calculate actual forcasted value
  S_k = return_ratio * S[final_timestep,]
  returns = S_k - S[final_timestep,]
  
  if (as_percentage == TRUE){
    pct_returns = return_ratio - 1
    print(pct_returns)
    return(pct_returns)
  }else{
    return(returns)
  }
}







