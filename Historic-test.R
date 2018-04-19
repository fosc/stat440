source("Historic.R")
require(testthat)

snp500 = read.csv("snp500-adj_close_2004-2018.csv", header = TRUE)

S<- as.matrix(subset(snp500, select = - c(Date, VIX, GSPC)))

#TESTING getWindow Function
#Check that dimensions and content of windows returned are correct. Also check that invalid windows are identified.

#some testing for the function
testS = matrix(c(1.00,2,3,4,5,6,7.101,8,9,11,12,13,14,15.999,16,17,18,19), ncol=3, byrow=TRUE, dimnames=list(c(1,2,3,4,5,6),c('WFC','JPM','AIG')))

test_that("Window Dimensions",{
  expect_equal(dim(getWindow(testS, wLength = 4, wOffset = 0)), c(4, 3))
  expect_equal(dim(getWindow(testS, wLength = 0, wOffset = 0)), c(4, 3))
})

test_that("Valid Windows",{
  expect_identical(getWindow(testS,wLength=3, wOffset=1), testS[2:4,])
  expect_identical(getWindow(testS,wLength=3, wOffset=0), testS[1:3,])
  expect_identical(getWindow(testS, wLength = dim(testS)[1], wOffset = 0), testS)
})

test_that("Invalid Windows", {
  expect_error(getWindow(testS,wLength=4, wOffset=3 ))
  expect_error(getWindow(testS,wLength=3, wOffset=-1 ))
  expect_identical(getWindow(testS, wLength = dim(testS)[1], wOffset = 1))
})


