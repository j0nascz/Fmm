gauss_seidel <- function(A, b, x_0 = NULL, tol = 1e-6, max_steps = 100) { 
  if (is.null(x_0)) { x_0 <- numeric(dim(A)[2]) 
  # Nullvektor der Länge n 
  } 
  C_inv <- A 
  C_inv[upper.tri(A)] <- 0 
  # Unteres Dreieck von A (inkl. Diagonale)
  x_k <- x_0 
  for (k in 1:max_steps) { 
    x_k_old <- x_k 
    x_k <- x_k_old + forwardsolve(C_inv, b- A %*% x_k_old) 
    # Inverse vermieden 
    if (norm(x_k- x_k_old, type = "I") <= tol) { 
      break 
    } 
  } 
  list(x = x_k, steps = k, C_inv = C_inv) 
}
