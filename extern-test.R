library(mvtnorm)
library(qqtest)
#library(rugarch)

#Graphical test of qqtest function from qqtest library:
#Generate data from various distributions and verify that qq plots against that same distribution
#fit well.
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

