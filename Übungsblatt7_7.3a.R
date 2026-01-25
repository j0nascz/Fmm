tensor_product <- function(A, b){ 
  prod <- array(NA, dim = c(nrow(A), ncol(A), length(b))) 
  for(i in 1:length(b)){ 
    prod[,,i] <- A*b[i] 
    } 
  prod 
}
