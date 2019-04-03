#' Title
#'
#' @param X.mat 
#' @param y.vec 
#' @param max.iterations 
#' @param step.size 
#' @param n.hidden.units 
#' @param is.train 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' data(ozone, package="ElemStatLearn")
#' X.mat <- as.matrix(ozone[,-1])
#' y.vec <- as.vector(ozone[,1])
#' n.hidden.units <- 2
#' max.iterations <- 50
#' is.train <- TRUE
#' res <- NNetIterations( X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train)
#' 
NNetIterations <- function(
  X.mat, 
  y.vec, 
  max.iterations, 
  step.size, 
  n.hidden.units, #u 
  is.train
){
  X.unscaled.mat <- as.matrix(X.mat[,-1])
  X.scaled.mat <- scale(X.unscaled.mat)
  v <- matrix(rnorm(ncol(X.scaled.mat) * n.hidden.units),ncol(X.scaled.mat), n.hidden.units)
  w <- rnorm(n.hidden.units)
  
  pred.mat <- matrix(,nrow(X.scaled.mat), max.iterations)
  for(i in 1:max.iterations)
  {
    A <- X.scaled.mat %*% v #1
    sigmoid <- function(a){
      1/(1 + exp(-a))
    }
    z <- sigmoid(A) #2
    b <- as.numeric(z %*% w)
    pred.mat[,i] <- b
    delta.w <- b - y.vec
    A.deriv <- z * (1 - z)
    delta.v <- diag(delta.w) %*% A.deriv %*% diag(w)
    grad.w <- t(z) %*% delta.w / nrow(X.scaled.mat)
    grad.V <- t(X.scaled.mat) %*% delta.v / nrow(X.scaled.mat)
    ## take a step
    w <- w - step.size * grad.w
    v <- v - step.size * grad.v
  }
  
  return(pred.mat)
}