# write tests for that R function, in tests/testthat/test-NNetIterations:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(CodingProject3)
library(testthat)
context("test-NNetIterations")


test_that("NNetIterations computes the right demensions", {
  data(ozone, package="ElemStatLearn")
  X.mat <- as.matrix(ozone[,-1])
  y.vec <- as.vector(ozone[,1])
  n.hidden.units <- 5
  max.iterations <- 100
  is.train <- TRUE
  step.size <- 0.1
  res <- NNetIterations( X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train)
  
  expect_equal(length(res), 4)
})

test_that("NNetIterations throws errors", {
  data(ozone, package="ElemStatLearn")
  X.mat <- c(ozone[,-1])
  y.vec <- as.vector(ozone[,1])
  n.hidden.units <- 5
  max.iterations <- 100
  is.train <- TRUE
  step.size <- 0.1
  
  expect_error(NNetIterations( X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train), "Feature matrix is not a matrix")
})