if(!exists("k_trajectories", mode="function")) source("CoreCode.R")

snp500 = read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)

S<- as.matrix(subset(snp500, select = - c(Date, VIX)))



#' getWindow
#' @param S Matrix of prices for one or more assets
#' @param wLength the length of the window (i.e. slice) of historic data in matrix S that we will take
#' @param wOffset how many days from the start of the data will our window (i.e. slice) of matrix S begin
#' @param trajectLength we ensure that there is enough room for a trajectory of length trajectLength between 
#' the end of the window and the end of the data in matrix S.
#' @return Matrix of corresponding student residuals
getWindow <- function(S, WLength, wOffset, trajectLength){
  
  t_final = dim(S)[1]
  
  #we don't train on less than 2000 daily observations.
  if(WLength<2000) stop("window length is below 2000. Fails to meet minimum data requirements.")
  if(WLength+wOffset+trajectLength > t_final ) stop("cannot create window. Doing so would push timesteps beyond t_final.")
  if(wOffset < 0) stop("cannot have negative offset for window")
  if(trajectLength < 1) stop("trajectLenght < 1. No sense in predicting anything before tomorrow.")
  
  return(S[(1+wOffset):(1+wOffset +WLength),])
}
