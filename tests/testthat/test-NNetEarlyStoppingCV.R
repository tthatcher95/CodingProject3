# write tests for that R function, in tests/testthat/test-NNetEarlyStoppingCV:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(CodingProject3)
library(testthat)
context("test-NNetEarlyStoppingCV")


test_that("NNetEarlyStoppingCV computes the right demensions", {
  data(SAheart , package = "ElemStatLearn")
  X.mat<-as.matrix(SAheart [1:50,-9])
  y.vec<-SAheart [1:50, 9]
  max.iterations <- 100
  step.size <- .5
  n.hidden.units <- 2
  res <- NNetEarlyStoppingCV(X.mat, y.vec, max.iterations, n.hidden.units)
  
  expect_equal(length(res), 7)
})

test_that("NNetEarlyStoppingCV throws errors", {
  data(SAheart , package = "ElemStatLearn")
  X.mat<-c(SAheart [1:50,-9])
  y.vec<-SAheart [1:50, 9]
  max.iterations <- 100
  step.size <- .5
  n.hidden.units <- 2
  
  expect_error(NNetEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations, n.hidden.units), "Feature matrix is not a matrix")
})
