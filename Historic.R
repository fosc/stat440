if(!exists("k_trajectories", mode="function")) source("CoreCode.R")

snp500 = read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)

S<- as.matrix(subset(snp500, select = - c(Date, VIX, GSPC)))



#' getWindow
#' @param S Matrix of prices for one or more assets
#' @param wLength the length of the window (i.e. slice) of historic data in matrix S that we will take
#' @param wOffset how many days from the start of the data will our window (i.e. slice) of matrix S begin
#' @param trajectLength we ensure that there is enough room for a trajectory of length trajectLength between 
#' the end of the window and the end of the data in matrix S.
#' @return Matrix of corresponding student residuals
getWindow <- function(S, wLength, wOffset){
  
  t_final = dim(S)[1]
  
  #we don't train on less than 2000 daily observations.
  if(wLength<2000) print("window length is below 2000. Fails to meet minimum data requirements.")
  if(wLength+wOffset > t_final ) stop("cannot create window. Doing so would push timesteps beyond t_final.")
  if(wOffset < 0) stop("cannot have negative offset for window")

  return(S[(1+wOffset):(wOffset+wLength),])
}


#some testing for the function
testS = matrix(c(1.00,2,3,4,5,6,7.101,8,9,11,12,13,14,15.999,16,17,18,19), ncol=3, byrow=TRUE, dimnames=list(c(1,2,3,4,5,6),c('WFC','JPM','AIG')))

#install.packages("testthat")
require(testthat)

expect_identical(getWindow(testS,wLength=3, wOffset=1 ), testS[2:4,] )
expect_identical(getWindow(testS,wLength=3, wOffset=0 ), testS[1:3,] )
expect_error(getWindow(testS,wLength=4, wOffset=3 ) )
expect_error(getWindow(testS,wLength=3, wOffset=-1 ) )


#' q_timeseries
#' @param S Matrix of prices for one or more assets
#' @param wLength how much historic data we will use when modelling the future portfolio
#' @param k how often to rebalance. Equivalently, how many days into the future to project when rebalancing
#' @return List with two componenets. A matrix of sequential values of q (i.e. the portfolio weights) 
#' and a vector containing the days on which the portfolio was rebalanced.
q_timeseries <- function(S, k, wLength){
  
  n_assets = dim(S)[2]
  
  n_rebalance = ceiling((dim(S)[1]-wLength)/k)
  print(n_rebalance)
  rebalance_days = wLength+1+(0:(n_rebalance-1))*k
  
  q_values = matrix(0, nrow=n_assets ,ncol=n_rebalance)
  
  start_value = 100
  prev_q = rep(1,n_assets)*start_value
  
  for(day in rebalance_days){
    
    train_data = getWindow(S, wLength, day-(wLength+1) )
    today = train_data[dim(train_data)[1],]
    #calculate how much our portfolio is worth before we re-balance. We cannot add or subtract value by rebalancing.
    value_today=t(prev_q)%*%today
    
    #now calculate the portfolio weightings q using train_data. Save these values of q in a matrix 
    q <- predict_q(train_data, k, value_today)
    
    print(head(q))
    prev_q <- q
    
    i= match(day, rebalance_days)
    q_values[,i] <- q
    
  }
  return(list(q=q_values, day=rebalance_days))
}



#q_timeseries(S, 100, 3000)


#' portfolio_timeseries
#' @param S Matrix of prices for one or more assets
#' @param wLength how much historic data we will use when modelling the future portfolio
#' @param k how often to rebalance. Equivalently, how many days into the future to project when rebalancing
#' @return List with two componenets. A matrix of sequential values of q (i.e. the portfolio weights) 
#' and a vector containing the days on which the portfolio was rebalanced.
portfolio_timeseries <- function(S, k, wLength){
  
  get_q_index <- function(day){
    #print(day)
    i <- floor((day-wLength)/k) + 1
    return(i)
  }
  
  list_of_q <- q_timeseries(S, k, wLength)
  
  first_day = list_of_q$day[1]
  last_day = dim(S)[1]
  
  #asset values for days that we have calculated portfolio
  q_data <- S[first_day:last_day,]
  
  ts <- rep(0,dim(q_data)[1])
  
  print(list_of_q$q)
  
  for(day in first_day:last_day){
    i=get_q_index(day)
    #print(i)
    ts[(day -first_day + 1)] <- as.numeric(t( list_of_q$q[,i] )%*%S[day,] )
  }
  #names(ts) <- first_day:last_day
  print(ts)
  plot(first_day:last_day,ts, type='l')

}
portfolio_timeseries(S,100,3000)






