library(pracma)
A <- function(eps) {
  matrix(c(eps, 1, 1, 1), nrow = 2)
  } 
A1 <- A(eps = 1e-1) 
luDec1 <- lu(A1) 
L1 <- luDec1$L 
U1 <- luDec1$U 
L1 %*% U1 
A2 <- A(eps = 1e-17) 
luDec2 <- lu(A2) 
L2 <- luDec2$L 
U2 <- luDec2$U 
L2 %*% U2
