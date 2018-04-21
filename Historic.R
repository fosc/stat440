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
  #if(wLength<2000) print("window length is below 2000. Fails to meet minimum data requirements.")
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



#q_timeseries(S, 100, 3000)


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


ts=portfolio_timeseries(S,100,3000)

#the stuff below was for testing the sensibility of the resuls I was getting
comparison = apply(S[3000:(length(ts)+2999),],1,sum)
comparison = comparison*(ts[1]/comparison[1])

plot(3000:(length(ts)+2999), ts, type='l' )
lines(3000:(length(ts)+2999), comparison[1:length(ts)], col='red' )

ts_monthly <- portfolio_timeseries(S, k = 22, wLength = 3000)
ts_monthly_2500 <- portfolio_timeseries(S, k = 22, wLength = 2500)
ts_quarterly <- portfolio_timeseries(S, k = 63, wLength = 3000)
ts_quarterly_2500 <- portfolio_timeseries(S, k = 63, wLength = 2500)
ts_anually <- portfolio_timeseries(S, k = 252, wLength = 3000)
ts_anually_2500 <- portfolio_timeseries(S, k = 252, wLength = 2500)

ts_balanced <- apply(100*S[2500:3408,], MARGIN = 1, FUN = sum)

#ts_temp <- read.csv("C:\\Users\\Isaac\\Desktop\\ts_temp.csv", header = TRUE, sep = ",")
#weights_temp <- read.csv("C:\\Users\\Isaac\\Desktop\\qs_temp.csv", header = TRUE, sep = "\t")
#dates_temp <- read.csv("C:\\Users\\Isaac\\Desktop\\days_temp.csv", header = TRUE, sep = ",")

#ts_monthly_2500 = list("ts" = c(unlist(unname(ts_temp))), "portfolio.weights" = weights_temp, "rebalance.days" = c(unlist(unname(dates_temp))))

# 
# write.table(ts_anually_2500$portfolio.weights,
#            file = "timeseries_results/anually_2500.csv",
#            row.names = FALSE,
#            sep = ",",
#            quote = FALSE)
# 
# write.table(ts_anually_2500$ts,
#             file = "timeseries_results/anually_2500.csv",
#             row.names = FALSE,
#             col.names = "ts_anually$ts",
#             sep = ",",
#             quote = FALSE, append = TRUE)
# 
# write.table(ts_anually_2500$rebalance.days,
#             file = "timeseries_results/anually_2500.csv",
#             row.names = FALSE,
#             col.names = "ts_anually$rebalance.days",
#             sep = ",",
#             quote = FALSE, append = TRUE)

#Plot of all porfolio time series
plot(ts_anually_2500$ts, 
     type = "l",
     main = "Anual Re-balancing",
     ylim = c(250000, 750000),
     xlim = c(0, 908),
     xaxs = "i",
     ylab = "Portfolio Value ($)",
     xlab = "Time Since Start (days)",
     col = "green")
#lines(ts_anually_2500$ts, col = "red")
lines(ts_quarterly_2500$ts, col = "blue")
lines(ts_monthly_2500$ts, col = "red")

lines(ts_balanced, col = "black")

lines(500:907, ts_anually$ts, col = "green", lwd = 3)
lines(500:907, ts_quarterly$ts, col = "blue", lwd = 3)
lines(500:906, ts_monthly$ts, col = "red", lwd = 3)

legend("topleft", 
       legend = c("Portfolio Value", "Rebalance"), 
       lty = c(1, 5),
       col = c("green", "black"), 
       cex = 0.6,
       bty = "n"
       )

abline(v = ts_anually_2500$rebalance.days - 2500, lty = 5)

#####
#FITTING GARCH TO TIME SERIES TO ESTIMATE VARIANCE
#####

library("rugarch")

Y_balanced <- diff(log(ts_balanced))
Y_annual <- diff(log(ts_anually_2500$ts))
Y_quarterly <- diff(log(ts_quarterly_2500$ts))
Y_monthly <- diff(log(ts_monthly_2500$ts))

gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="std")

garch_annual <- ugarchfit(gspec.ru, Y_annual)
sigmas_annual <- garch_annual@fit$sigma 

garch_quarterly <- ugarchfit(gspec.ru, Y_quarterly)
sigmas_quarterly <- garch_quarterly@fit$sigma 

garch_monthly<- ugarchfit(gspec.ru, Y_monthly)
sigmas_monthly <- garch_monthly@fit$sigma 

garch_balanced <- ugarchfit(gspec.ru, Y_balanced)
sigmas_balanced <- garch_balanced@fit$sigma 

######
#Fitting with n=2000 for portfolio forecast
#####
for(i in 1:3){
  if(i == 1){
    ts_monthly_2500_2000 <- portfolio_timeseries(S, k = 22, wLength = 2500)
  }else if (i == 2){
    ts_quarterly_2500_2000 <- portfolio_timeseries(S, k = 63, wLength = 2500)
  }else if (i ==3){
    ts_anually_2500_2000 <- portfolio_timeseries(S, k = 252, wLength = 2500)
  }
}

#ts_bi_monthly_2500_2000 <- portfolio_timeseries(S, k = 44, wLength = 2500)

Y_annual <- diff(log(ts_anually_2500_2000$ts))
Y_quarterly <- diff(log(ts_quarterly_2500_2000$ts))
Y_monthly <- diff(log(ts_monthly_2500_2000$ts))

gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="std")

garch_annual <- ugarchfit(gspec.ru, Y_annual)
sigmas_annual <- garch_annual@fit$sigma 

garch_quarterly <- ugarchfit(gspec.ru, Y_quarterly)
sigmas_quarterly <- garch_quarterly@fit$sigma 

garch_monthly<- ugarchfit(gspec.ru, Y_monthly)
sigmas_monthly <- garch_monthly@fit$sigma 


