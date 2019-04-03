# write tests for that R function, in tests/testthat/test-NNetEarlyStoppingCV:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(CodingProject3)
library(testthat)
context("test-NNetEarlyStoppingCV")


test_that("NNetEarlyStoppingCV computes the right demensions", {
  # data(spam, package = "ElemStatLearn")
  # X.mat<-spam[1:100,-58]
  # X.scaled.mat<-scale(X.mat, center = TRUE, scale = TRUE)
  # y.vec<-spam[1:100, 58]
  
  # expect_equal(nrow(res), 1)
})

test_that("NNetEarlyStoppingCV throws errors", {
  # data(spam, package = "ElemStatLearn")
  # X.mat<-spam[1:100,-58]
  # X.scaled.mat<-scale(X.mat, center = TRUE, scale = TRUE)
  # y.vec<-spam[1:100, 58]
  
  # expect_error()
})