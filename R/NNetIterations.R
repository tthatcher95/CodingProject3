NNetIterations <- function(
  X.mat, 
  y.vec, 
  max.iterations, 
  step.size, 
  n.hidden.units, 
  is.train
){
  head(X.mat)
  X.unscaled.mat <- as.matrix(X.mat[,-1])
  head(X.unscaled.mat)
  X.scaled.mat <- scale(X.unscaled.mat)
  head(X.scaled.mat)
  n.hidden.units <- 2 #u
  v <- matrix(rnorm(ncol(X.scaled.mat) * n.hidden.units),ncol(X.scaled.mat), n.hidden.units)
  (w <- rnorm(ncol(X.scaled.mat) * n.hidden.units))
  
  head(A <- X.scaled.mat %% V) #1
  sigmoid <- function(a){
    1/(1 + exp(-a))
  }
  head(z <- sigmoid(A)) #2
  head(b <- as.numeric(z %% w))
  head( delta.w <- b - y.vec)
  head(A.deriv <- z * (1 - z))
  diag(w)
  head(delta.v <- diag(delta.w) %*% A.deriv %*% diag(w))
  head(grad.w <- t(z) %*% delta.w / nrow(X.scaled.mat))
  head(grad.V <- t(X.scaled.mat) %*% delta.v / nrow(X.scaled.mat))
  ## take a step
  step.size <- 0.01
  w <- w - step.size * grad.w
  v <- v - step.size * grad.v
  sum(abs(c(grad.w, as.numeric(grad.v)))) # to check if it is decreasing
       
}