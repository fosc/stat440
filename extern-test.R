library(mvtnorm)
library(qqtest)
library(rugarch)

#TEST qqtest from qqtest
#VISUAL: Check that data from various distributions fit well on qq plots against that same distribution
qqtest_student_test <- function(){
  
  par(mfrow=c(2, 2))
  
  df = runif(1, min = 2, max = 40)
  data_t <- rt(n = 1000, df = df)
  data_norm <- rnorm(n = 1000, mean = 0, sd = 1)
  data_exp <- rexp(n = 1000, rate = 1)
  data_uniform <- runif(n = 1000)
  
  
  qqtest(data = data_t, dist = "student", df = df, main = "Student t test")
  qqtest(data = data_norm, dist = "normal", main = "Normal test")
  qqtest(data = data_exp, dist = "exponential", df = df, main = "Exponential test")
  qqtest(data = data_uniform, dist = "uniform", main = "Uniform test")
}

qqtest_student_test()

#TEST rvmnorm from mvtnorm
#NUMERICAL: Check that 100,000 multivariate normal samples approach a specified mean and variance
rmvnorm_test <- function(){
  mu <- 1:3
  sg <- matrix(round(runif(n = 9, min = 1, max = 3)), nrow = 3, ncol = 3)
  sg <- t(sg) %*% sg
  
  samples <- rmvnorm(100000, mu, sg)
  means <- apply(samples, MARGIN = 2, mean)
  var_matrix <- var(samples)
  
  results <- list("means" = means, "var_matrix" = var_matrix, "abs_mean_diff" = abs(means - mu), "abs_var_diff" = abs(sg - var_matrix))
  return(results)
}

rmvnorm_test()


#TEST ugarchfit from rugarch
#VISUAL: Check that residuals of GARCH(1,1) model match theoretical student t distribution using a
#qqplot and density plot.

snp500 = read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)
S <- as.matrix(subset(snp500, select = - c(Date, VIX, GSPC)))

ugarchfit_test <- function(){
  
  n <- round(runif(1, min = 1, max = 45))
  s <- S[, n]
  y <- diff(log(s))
  
  garch_spec <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="std")
  garch_fit <- ugarchfit(garch_spec, y)
  
  plot(garch_fit)
  
}

ugarchfit_test()
