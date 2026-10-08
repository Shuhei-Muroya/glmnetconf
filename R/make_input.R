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


make_input <- function(X){
  X <- as.matrix(X)
  n <- nrow(X); p <- ncol(X)
  col_sd <- apply(X, 2, sd)
  col_sd[col_sd == 0] <- 1
  X <- sweep(X, 2, col_sd, FUN = "/")
  V <- (t(X) %*% X) / n
  ev <- eigen(V, only.values = TRUE)$values
  ev <- eigen(V)$values
  vec <- c(n, p, head(ev, 10), rev(tail(ev, 10)))
  return(vec)
}
