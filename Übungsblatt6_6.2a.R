rmat_diagdom <- function(n) {
  A <- matrix(rnorm(n^2), nrow = n)
}

diag(A) <- rowSums(abs(A))

A
