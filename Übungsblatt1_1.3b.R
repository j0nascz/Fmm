library(microbenchmark)
library(ggplot2)

grid <- expand.grid(
  n = seq.int(10, 1000, length = 4), 
  d = seq.int(10, 1000, length = 4)
)


times <- numeric(nrow(grid))

sigma_2 <- function(X, beta) {
  n <- nrow(X)
  d <- ncol(X)
  Sig <- matrix(0, d, d)
  for (i in 1:n) {
    Sig <- Sig + (X[i, ] - colMeans(X)) %*% t(X[i, ] - colMeans(X))
  }
  as.numeric(t(beta) %*% Sig %*% beta / n)
}

for (k in 1:nrow(grid)) {
  n <- grid[k, 1]
  d <- grid[k, 2]
  X <- replicate(d, rnorm(n))
  cat("n = ", n, ", d = ", d, ", time = ", sep = "")
  times[k] <- median(
    microbenchmark(sigma_2(X, rep(1, d)), times = 10)$time
  )
  cat(times[k] / 1e9, "\n")
}

