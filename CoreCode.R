#install.packages("rugarch")
library(rugarch)

#' fitGarch
#' @param Y Matrix of daily log differences in asset prices.
#' @return List of the following parameters fit to a garch model: df, mu, sigma, and X (the residuals)
fitGarch <- function(Y,k){
  
  n_assets = dim(Y)[2]
  n_days = dim(Y)[1]
  
  resids <- function(y_i){
    gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="std")
    gfit.ru <- ugarchfit(gspec.ru, y_i)
    mu = gfit.ru@fit$solver$sol$pars[[1]]
    df = gfit.ru@model$pars["shape", 1]
    
    forecast = ugarchforecast(gfit.ru, n.ahead=k)
    sigmas = as.vector(sigma(forecast)[,1])
    #print(sigmas)
    historic_sigmas = gfit.ru@fit$sigma 
    rsd = (y_i - mu)/historic_sigmas
    
    print(sigmas[1])
    return( append(rsd, c(df,mu,sigmas)) )
  }
  
  X= apply(t(Y), 1,resids)
  df = X[(n_days+1),]
  mu= X[(n_days+2),]
  sigmas = X[(n_days+3):(n_days+2+k),]
  X = X[0:(n_days+1),]
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
  #qqnorm(z)
  #qqline(z, col = 2, lwd = 2, lty = 2)
  return(z)
}

#' getParams
#' @param Y Matrix of daily log differences in asset prices.
#' @return List of parameters of GARCH GC model: COR, df, mu, sigma, COV
getParams <- function(Y,k){
  GARCH = fitGarch(Y,k)
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
#'          In the case of one asset, this means that S must be an nX1 matrix with column name being the name of the asset.
#' @param Params List of parameters from fitted GARCH(1, 1) models for each asset. (COR, DF, MU, SIGMA).
#' @param k Natural number, number of timesteps ahead to forecast asset values.
#' @param pct_return Boolean, TRUE = give returns as percentage of current value, FALSE = actual change in stock value.
#' @return Vector containing the forcasted returns. Either as a percentage or current value or simply change in price of stock.
k_trajectories <- function(S, k, params, as_percentage = FALSE){
  
  num_timesteps <- dim(S)[1]
  num_assets <- dim(S)[2]
  asset_names = colnames(S)
  
  #Number of timesteps in data, number of assets.
  #If only a single asset is passed in a nX1 matrix
  #need to handle differently.
  if(num_assets != 1){
    
    #Generate model errors from MVN, convert to student t scale
    z_k = rmvnorm(k, rep(0, num_assets), params$COR[asset_names, asset_names])
    z_k_t = studentize(z_k, df = params$df[asset_names])
    
  }else{
    
    #Generate model errors from Normal, convert to student t scale
    z_k = rnorm(k, 0, params$COR[asset_names, asset_names])
    z_k_t = studentize(z_k, df = params$df[asset_names])
  }
  
  #Estimates for change in value at each of the k forcasted time steps,
  #as the log of the ratio of the last given value in time series
  y_k = params$mu[asset_names] + z_k_t * params$sigma[,asset_names]
  
  #Calculate ratio of final to last given value over k timesteps
  #Handle cases for predictions one timestep into future and
  #predictions for a single asset separately. 
  if(k == 1){
    return_ratio = exp(y_k)
  }else if (num_assets == 1){
    return_ratio = Reduce(prod, exp(y_k))
  }else{
    return_ratio = apply(exp(y_k), 2, prod)
  }
  
  #Calculate actual forcasted value
  S_k = return_ratio * S[num_timesteps,]
  returns = S_k - S[num_timesteps,]
  
  if (as_percentage == TRUE){
    pct_returns = return_ratio - 1
    return(pct_returns)
  }else{
    return(returns)
  }
}

#****TO DO: *make more efficient: pretty slow after 10,000 or so portfolio forecasts
#           *confidence intervals? etc.
#' #Create portfolio forecast distribtion, get mean, variance of forecasted returns
#' @param P Matrix of asset prices. Assumed that each column name is the name of the asset in that column.
#'          In the case of one asset, this means that P must be an nX1 matrix with column name being the name of the asset.
#' @param Params List of parameters from fitted GARCH(1, 1) models for each asset. (COR, DF, MU, SIGMA).
#' @param q Vector, number of shares of each asset held in portfolio (P), given in order of assets in columns of P from left to right
#' @param k Natural number, number of timesteps ahead to forecast asset values.
#' @param n Natural number, number of times to make k-step prediction. This will be number of elements in vector returned
#' @param plot_hist Boolean, TRUE = plot histogram of predicted portfolio values.
#' @return List containing: (Vector) P_t_k of n forcasted portfolio values, (Vector) mu of mean forecasted returns for each asset, 
#' (Matrix) Sigma variance-covariance matrix of forecasted returns. 
portfolio_k_forecast_distribution <- function(P, q, params, k, n, plot_hist = FALSE){
  
  num_timesteps <- dim(P)[1]
  num_assets <- dim(P)[2]
  s_t = unlist(P[num_timesteps, ]) 
  
  #Produces n forecasts of length k as matrix with asset names as columns
  #and each row being one of the n forecasts
  returns <- t(replicate(n, 
                         k_trajectories(S = P, params = params, k = k), 
                         simplify = "vector"))
  
  #Convert list of returns to martix for computing portfolio values
  returns <- matrix(unlist(returns), 
                    ncol = num_assets, 
                    nrow = n, 
                    dimnames = list(NULL,colnames(returns)))
  
  mu_returns <- apply(returns, 2, mean)
  Sigma_returns <- cov(returns)
  
  #S_t_k is matrix of predicted asset values
  S_t_k <- t(t(returns) + s_t)
  
  #P_t_k is vector of predicted portfolio values
  P_t_k <- S_t_k %*% q
  
  if (plot_hist){
    hist(P_t_k, 
         main = "Portfolio Distribution",
         xlab = "Predicted Portfolio Values ($)",
         ylab = "Frequency",
         freq = TRUE,
         breaks = "FD",
         col = adjustcolor("grey", alpha = 0.5))
  }
  
  forecasted_P_k = list("P_t_k" = P_t_k, "mu" = mu_returns, "Sigma" = Sigma_returns)
  return(forecasted_P_k)
}





