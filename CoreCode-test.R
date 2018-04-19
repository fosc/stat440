#require(testthat)
#library(fitdistrplus)
#library(MASS)
library(mvtnorm)
library(qqtest)
source("CoreCode.R")

#TESTING normalize function       
#Test numerically that that E(x) ~= 0 and Var(x) ~= 1
mu = 0
sig = 1

#Transform randomly generated student t data to N(0,1) data
generate_normalized_data <- function(){
  
  #generate random n student t data points
  n <- round(runif(1, min = 2000, max = 5200))
  df <- runif(1, min = 2, max = 50)
  student_t_data <- rt(n, df)
  
  #print(n)
  
  #normalize data
  x <- normalize(c(student_t_data, df))
  return(x)
}

test_normalize <- function(){
  
  X <- replicate(5, generate_normalized_data(), simplify ="vector")
  
  #print(X)
  
  X_stats <- list("mean" = sapply(X, FUN = mean), "var" = sapply(X, FUN = var))
  
  #print(X_stats)
  
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

test_normalize()


#TESTING studentize function       
#Check visually that studentized data could reasonably come from a student-t using qqplot

#Intermittent Issue: if there is a large negative or large positive value in
#the normal data passed to studentize, pnorm() will produce 0, which will in 
#turn make qt() produce either -Inf or Inf. 
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

test_studentize()

#TESTING k_trajectories function       
# test that distribution of k-trajectories is a student-t distribution

snp500 <- read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)
S <- subset(snp500, select = -Date)
#Y <- diff(log(as.matrix(S)))

generate_k_trajectories <- function(){
  
  n <- round(runif(1, min = 2, max = 45))
  print(n)
  subset_assets <- sample(seq(1:45), 4, replace = FALSE)
  S_sub <- S[, subset_assets]
  Y <- diff(log(as.matrix(S_sub)))
  
  k <- round(runif(1, min = 1, max = 30))
  print(k)
  params <- getParams(Y, k)
  
  k_traj <- t(replicate(1000, 
                        k_trajectories(S = S_sub, params = params, k = k, as_percentage = TRUE), 
                        simplify = "vector"))

  #print(head(S_sub))
  results <- list("trajectories" = k_traj, "params" = params)
  return(results)
}

test_k_trajectories <- function(){
  
  traj.params <- generate_k_trajectories()
  dfs <- traj.params$params$df
  trajectories <-  traj.params$trajectories
  n_samples <- dim(trajectories)[1]
  
  #print(n_samples)
  plot_range = seq(-1, 1, length.out = n_samples)
  #print(plot_range)
  print(colnames(trajectories))
  
  
  par(mfrow=c(2, 2))
  #col = 1
  #data_i <- trajectories[, col]
  #df <- dfs[col]
  #h <- hist(data_i, breaks = "FD", plot = TRUE, freq = FALSE)
  #h$counts <- h$counts/sum(h$counts)
  #plot(h)
  par(mfrow=c(2, 2))
  #print(sum(h$density))
  
  print(df)
  #print(plot_range)
  #print(dt(plot_range, df = df))
  #plot(x = plot_range,
        #y = dt(plot_range, df = df),
        #col="red")
  
  for (col in 1:4){
    data_i <- trajectories[, col]
    df <- dfs[col]
    asset_name <- colnames(trajectories)[col]
    # hist(data_i, breaks = "FD", freq = FALSE)
    # lines(x = plot_range,
    #       y = dt(plot_range, df = df),
    #       col="red")
    qqtest(data_i, 
           dist = "student", 
           df = df, 
           main = paste("qq plot -", asset_name),
           xlab = paste("Student t(", round(df, digits = 3), ") quantiles"))
  }

}

test_k_trajectories()
