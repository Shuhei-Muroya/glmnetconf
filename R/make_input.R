make_input_lars<-function(X){
  X<-as.matrix(X)
  n<-nrow(X)
  p<-ncol(X)
  V<-cov(X)
  eigen<-eigen(V)$values
  a<- tail(eigen, 5)
  tail_input <- a[5:1]
  vec<-c(n,p,head(eigen,5),tail_input)
  return(vec)
}


make_input<-function(X){
  X<-as.matrix(X)
  n<-nrow(X)
  p<-ncol(X)
  V<-cov(X)
  eigen<-eigen(V)$values
  tail_input<- rev(tail(eigen, 10))
  vec<-c(n,p,head(eigen,10),tail_input)
  return(vec)
}
