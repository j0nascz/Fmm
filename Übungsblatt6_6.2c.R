options(digits = 7) 
set.seed(0)
# Fixiere Seed für Reproduzierbarkeit 
n <- 8 
for (i in 1:10) { 
  A <- rmat_diagdom(n) 
  # Zufällige strikt diagonal dominante Matrix 
  x <- rnorm(n) 
  # Zufällige Lösung 
  b <- A %*% x 
  # Rechte Seite 
  x_0 <- numeric(n) 
  # Nullvektor 
  gs <- gauss_seidel(A, b, x_0) 
  x_gs <- gs$x 
  # Gauss-Seidel-Lösung 
  steps_gs <- gs$steps 
  # Anzahl benötigter Schritte 
  C_inv_gs <- gs$C_inv 
  # Gauss-Seidel-Iterationsmatrix 
  # Spektralradius, Inverse vermieden 
  rho_gs <- norm(diag(n)- forwardsolve(C_inv_gs, A), type = "2") 
  # Approximationsfehler 
  err_gs <- norm(x- x_gs, type = "2") 
  # Obere Schranke 
  upper_bound <- rho_gs^steps_gs * norm(x- x_0, type = "2") 
  cat("Spektralradius: ", rho_gs, "\n") 
  cat("Anzahl benötigter Schritte: ", steps_gs, "\n") 
  cat("Approximationsfehler: ", err_gs, "\n") 
  cat("Obere Schranke: ", upper_bound, "\n") 
  cat("\n") }