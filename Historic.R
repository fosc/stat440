source("CoreCode.R")

#' getWindow
#' @param S Matrix of prices for one or more assets
#' @param wLength the length of the window (i.e. slice) of historic data in matrix S that we will take
#' @param wOffset how many days from the start of the data will our window (i.e. slice) of matrix S begin
#' @param trajectLength we ensure that there is enough room for a trajectory of length trajectLength between 
#' the end of the window and the end of the data in matrix S.
#' @return Matrix of corresponding student residuals
getWindow <- function(S, wLength, wOffset){
  
  t_final = dim(S)[1]
  
  if(wLength+wOffset > t_final ) stop("cannot create window. Doing so would push timesteps beyond t_final.")
  if(wOffset < 0) stop("cannot have negative offset for window")

  return(S[(1+wOffset):(wOffset+wLength),])
}


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


#' portfolio_timeseries
#' @param S Matrix of prices for one or more assets
#' @param wLength how much historic data we will use when modelling the future portfolio
#' @param k how often to rebalance. Equivalently, how many days into the future to project when rebalancing
#' @return A vector that contains the daily timeseries values of the portfolio.
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
  
  for(day in first_day:last_day){
    i=get_q_index(day)
    #print(i)
    ts[(day -first_day + 1)] <- as.numeric(t( list_of_q$q[,i] )%*%S[day,] )
  }

  comparison = apply(S,1,sum)
  comparison = comparison*ts[1]/comparison[1]
  
  timeseries_list <- list("ts" = ts, "portfolio.weights" = list_of_q$q, "rebalance.days" = list_of_q$day)

  return(timeseries_list)

}


