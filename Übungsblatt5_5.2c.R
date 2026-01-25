my_qr <- function(A) { 
  m <- nrow(A) 
  n <- ncol(A) 
  Q <- diag(m) 
  A_k <- A 
  for (k in seq_len(min(m, n))) { 
    # Berechne Householder-Matrix 
    c_bar <- A_k[k:m, k] 
    v <- c_bar + c(norm(c_bar, "2"), rep(0, length(c_bar)- 1)) 
    H <- diag(m- k + 1)- 2 * tcrossprod(v, v) / sum(v^2) 
    # Erstelle Q_k 
    zero <- matrix(0, k- 1, m- k + 1) 
    Q_k <- rbind( cbind(diag(k- 1), zero), cbind(t(zero), H) ) 
    # Wende Householder-Reflexion an 
    A_k <- Q_k %*% A_k 
    # Update von Q 
    Q <- Q %*% t(Q_k) 
    } 
  list(Q = Q, R = A_k) 
  }

set.seed(5) 
m <- 3 
n <- 2 
A <- matrix(rnorm(m*n), m, n) 
A

qr <- my_qr(A)
qr
