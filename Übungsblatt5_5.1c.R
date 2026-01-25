a <- 1:4 
v <- a + norm(a, "2") * c(1, 0, 0, 0) 
H <- diag(4)- 2 * v %*% t(v) / norm(v, "2")^2
H %*% a
