tensor_product2<-function(b, A){ 
  prod <-array(NA,dim=c(length(b),nrow(A),ncol(A))) 
  for(i in 1:length(b))
    { 
    prod[i,,]<-A*b[i] 
  } 
  prod 
}
