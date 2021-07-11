# Q1
my_LS <- function(X, Y){
  A <- solve(t(X)%*%X)%*%t(X)%*%Y
  return(as.vector(A))
}
X <- cbind(rep(1, 10), 1:10)
Y <- seq(1,30, length = 10)
my_LS(X, Y)


# Q2
my_ridge <- function(X, Y, lambda){
  I <- diag(1, nrow = nrow(t(X)%*%X), ncol = ncol(t(X)%*%X))
  A <- solve(t(X)%*%X + lambda*I)%*%t(X)%*%Y
  return(as.vector(A))
}
X <- cbind(rep(1, 10), 1:10)
Y <- seq(1,30, length = 10)
lambda <- 1
my_ridge(X, Y, lambda)


# Q3
my_sum <- function(n, m){
  sum <- 0
  for (x in 1:n) {
    for (y in 1:m) {
      sum <- sum + x^2*y/(x+y)
    }
  }
  return(sum)
}
my_sum(n = 5, m = 6)


# Q4
my_sum2 <- function(n, m){
  x <- matrix(1:n, n, m)
  y <- matrix(1:m, n, m, byrow = TRUE)
  return(sum(x^2*y/(x+y)))
}
my_sum2(n = 5, m = 6)


# Q5
floor(runif(1, min = 1, max = 6))


# Q6
sample(c(0,1), size = 10, replace = TRUE, prob = c(0.4,0.6))


# Q7 a
x <- seq(1,1000,by=3)
sum(abs(x))

# Q7 b
x <- seq(1,1000,by=3)
sqrt(sum(x^2))


# Q8
v <- 1:10 # define v be a vector of positive integers
sum(v[v%%2==1]) # find the sum of all odd integers in v
# so, the final answer is sum(v[v%%2==1])


# Q9
x <- 0:10
plot(x, dgeom(x, prob = 0.3), type="h")


# Q10
no_sum <- 100000
result <- rep(0, no_sum)
for (i in 1:no_sum) {
  price <- 100*exp(cumsum(rnorm(40, mean = 0.0002, sd = 0.015)))
  result[i] <- min(price[1:20]) < 95 & max(price) > 101
}
mean(result)


