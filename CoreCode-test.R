library(mvtnorm)
library(qqtest)
source("CoreCode.R")

#TEST normalize      
#NUMERICALLY: Check that that E(x) ~= 0 and Var(x) ~= 1 for residuals converted to N(0,1)
generate_normalized_data <- function(){
  
  n <- round(runif(1, min = 2000, max = 5200))
  df <- runif(1, min = 2, max = 50)
  student_t_data <- rt(n, df)
  
  #normalize data
  x <- normalize(c(student_t_data, df))
  return(x)
}

test_normalize <- function(){
  
  X <- replicate(5, generate_normalized_data(), simplify ="vector")
  X_stats <- list("mean" = sapply(X, FUN = mean), "var" = sapply(X, FUN = var))
  
  mean_abs_max <- max(abs(X_stats$mean))
  var_abs_max <- max(abs(X_stats$var))
  mean_average <- mean(X_stats$mean)
  var_average <- mean((X_stats$var))

  normalize_test_results <- list(mean_average = mean_average,
                                 var_average = var_average,
                                 mean_abs_max = mean_abs_max,
                                 var_abs_max = var_abs_max)

  print(normalize_test_results)
}

#TESTING studentize      
#VISUALLY: Check that studentized data could reasonably fits a student-t distribution using qqplot
generate_studentized_data <- function(){
  
  #generate random n student t data points
  n <- round(runif(1, min = 25, max = 46))
  df <- runif(1, min = 2, max = 50)
  mean_vec <- rep(0, n)
  M <- matrix(runif(n*n), nrow = n, ncol = n)
  sigma <- t(M) %*% M
  
  normal_data <- t(rmvnorm(1, mean=mean_vec, sigma=sigma))
  
  #studentize data
  x <- studentize(normal_data, df)

  return(list("data"=x, "df"=df))
}

test_studentize <- function(){
  
  par(mfrow=c(1, 1))
  
  x <- generate_studentized_data()
  t.df <- x$df
  x <- x$data
  qqtest(x, 
         dist = "student", 
         df = t.df, 
         main = "QQ plot - studentized data",
         xlab = paste("Student t(",round(t.df, digits = 3), ") quantiles"))
  
}

#TESTING k_trajectories      
#VISUALLY: Check that distribution of k-trajectories reasonably fits a student t distribution
generate_k_trajectories <- function(){
  
  snp500 <- read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)
  S <- subset(snp500, select = -Date)
  
  n <- round(runif(1, min = 2, max = 45))
  #print(n)
  subset_assets <- sample(seq(1:45), 4, replace = FALSE)
  S_sub <- S[, subset_assets]
  Y <- diff(log(as.matrix(S_sub)))
  
  k <- round(runif(1, min = 2, max = 252))
  #print(k)
  params <- getParams(Y, k)
  
  k_traj <- t(replicate(1000, 
                        k_trajectories(S = S_sub, params = params, k = k, as_percentage = FALSE), 
                        simplify = "vector"))

  results <- list("trajectories" = k_traj, "params" = params)
  return(results)
}

test_k_trajectories <- function(){
  
  traj.params <- generate_k_trajectories()
  print(traj.params)
  dfs <- traj.params$params$df
  trajectories <- traj.params$trajectories
  n_samples <- dim(trajectories)[1]
  
  par(mfrow=c(2, 2))
  
  for (col in 1:4){
    data_i <- unlist(trajectories[, col])
    df <- dfs[col]
    asset_name <- colnames(trajectories)[col]
    qqtest(data_i,
           dist = "student",
           df = df,
           main = paste("qq plot -", asset_name),
           xlab = paste("Student t(", round(df, digits = 3), ") quantiles"))
  }

}

#TESTING portfolio_k_forecast_distribution function       
# NUMERICALLY: test that prediction mean returns for individual assets are consistent,
#i.e check that percentage difference in two seperate .
generate_portfolio_forecast <- function(){

  snp500 <- read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)
  S <- subset(snp500, select = -Date)
  
  Y <- diff(log(as.matrix(S)))
  k <- 22
  params <- getParams(Y, k)
  
  portfolio_forecasts <- t(replicate(2, 
                          portfolio_k_forecast_distribution(P = S, k = k)$mu, 
                          simplify = "vector"))
  
  max_means <- apply(portfolio_forecasts, MARGIN = 2, FUN = max)
  return(abs(diff(portfolio_forecasts))/max_means)
}

test_portfolio_forecast <- function(){
  
  portfolio_means <- generate_portfolio_forecast()
  
  return(max(portfolio_means))
}

