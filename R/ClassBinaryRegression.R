data(spam, package = "ElemStatLearn")
head(spam)

head(y.vec <- ifelse(spam$spam =="spam", 1, -1))
table(y.vec)
head(X.mat <- as.matrix(subset(spam, select=-spam)))

step.size <- 0.02
n.hidden.units <- 2 #u

# Sigmoid Function
sigmoid <- function(a){
  1/(1+exp(-a))
}

set.seed(1)
n.folds <- 2
(unique.folds <- 1:n.folds)
(fold.vec <- sample(rep(unique.folds, l=nrow(X.mat))))
validation.fold <- 1
is.train <- fold.vec != validation.fold
table(is.train)
X.train <- X.mat[is.train, ]
dim(X.mat)
dim(X.train)
y.train <- y.vec[is.train]
length(y.vec)
length(y.train)

##Scaling
head(X.sc <- scale(X.train))
attr(X.sc, "scaled:center")
attr(X.sc, "scaled:scale")

(V <- matrix(rnorm(ncol(X.sc)*n.hidden.units), ncol(X.sc), n.hidden.units)) # P x U
(w <- rnorm(n.hidden.units))

# Init V vector as random numbers
head(A <- X.sc %*% V) #1
head(Z <- sigmoid(A)) #2 / S[A]
# Init weight vec as random numbers
head(b <- as.numeric(Z %*% w)) #3

Y <- diag(y.train)

#head(delta.w <- -Y %*% (sigmoid(-Y %*% b))) #4
head(delta.w <- -y.train * sigmoid(-y.train * b))
head(A.deriv <- Z * (1 - Z)) #S'[A]
head(delta.v <- diag(delta.w) %*% A.deriv %*% diag((w))) #5
head(dv <- unname(dw * A.deriv * matrix(w, nrow(A.deriv), ncol(A.deriv), byrow=TRUE)))
head(grad.w <- t(Z) %*% delta.w / nrow(X.sc)) #6
head(grad.v <- t(X.train) %*% dv / nrow(X.train)) #7

# Take a step
(w <- w - step.size * grad.w)
(V <- V - step.size * grad.v)

# See how it is working
print(sum(abs(c(grad.w, as.numeric(grad.v)))))

predict.sc <- function(X.tilde) {
  A.mat <- X.tilde %*% V
  sigmoid(A.mat) %*% w
}

predict1.orig <- function(X.unsc) {
  X.tilde <- scale(X.unsc, attr(X.sc, "scaled:center"), attr(X.sc, "scaled:scale"))
  predict.sc(X.tilde)
}

V.orig <- V/attr(X.sc, "scaled:scale")
b.orig <- - t(V/attr(X.sc, "scaled:scale")) %*% attr(X.sc, "scaled:center")
V.with.intercept <- rbind(intercept=as.numeric(b.orig), V.orig)

predict2.orig <- function(X.unsc) {
  A.mat <- cbind(1, X.unsc) %*% V.with.intercept
  sigmoid(A.mat) %*% w
}

# Show that all three work the exact same
rbind(
  as.numeric(head(pred.mat <- predict.sc(X.sc))),
  as.numeric(head(predict1.orig(X.train))),
  as.numeric(head(predict2.orig(X.train))))

# Train / Validation Error
pred.vec <- as.numeric(predict2.orig(X.mat))
set.list <- list(
  train=is.train,
  validation=!is.train)
is.error <- ifelse(pred.vec > 0, 1, -1) != y.vec
log.loss <- log(1 + exp(-y.vec * pred.vec))
sapply(names(set.list), function(set.name) {
  is.set = set.list[[set.name]]
  mean(is.error[is.set])
})
