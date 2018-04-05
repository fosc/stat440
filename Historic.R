if(!exists("k_trajectories", mode="function")) source("CoreCode.R")

snp500 = read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)

S<- as.matrix(subset(snp500, select = - c(Date, VIX)))

#we don't train on less than 2000 daily observations.

getWindow <- function(S, Wlength, wOffset, trajectLength){
  
  t_final = dim(S)[1]
  
  if(Wlength<2000) stop("window length is below 2000. Fails to meet minimum data requirements.")
  if(Wlength+wOffset+trajectLength > t_final ) stop("cannot create window. Doing so would push timesteps beyond t_final.")
  if(wOffset < 0) stop("cannot have negative offset for window")
  if(trajectLength < 1) stop("trajectLenght < 1. No sense in predicting anything before tomorrow.")
  
  return(S[(1+wOffset):Wlength,])
}
