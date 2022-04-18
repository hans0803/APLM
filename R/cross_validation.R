hcv <- function(data, seed, prop=0.2) {
  set.seed(seed)
  datasize <- dim(data)[1]
  cvlist <- list(NULL)
  cvlist[[2]] <- sort(sample(1:datasize, ceiling(datasize*prop), replace=F))
  cvlist[[1]] <- sort(c(1:datasize)[-cvlist[[2]]])
  return(cvlist)
}

loocv <- function(data) {
  datasize <- dim(data)[1]
  cvlist <- list(NULL)
  cvlist <- lapply(1:datasize, function(x) c(1:datasize)[-x])
  return(cvlist)
}

kfcv <- function(data, seed, k=5) {
  set.seed(seed)
  datasize <- dim(data)[1]
  cvlist <- list(NULL)
  n <- rep(1:k, ceiling(datasize/k))[1:datasize]
  temp <- sample(n, datasize)
  dataseq <- 1:datasize
  cvlist <- lapply(1:k, function(x) dataseq[temp==x])
  return(cvlist)
}
