qr_algorithm <- function(A, steps = 20) { 
  A_k <- A 
  for (k in 1:steps) {
    QR_k <- qr(A_k) 
    Q_k <- qr.Q(QR_k) 
    R_k <- qr.R(QR_k) 
    A_k <- R_k %*% Q_k 
    } 
  diag(A_k) 
}
