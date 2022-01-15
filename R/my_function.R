round45 <- function(x, digit=0) {
  x <- x*(10^digit)
  if (x-floor(x)<0.5) {
    return(floor(x)/(10^digit))
  } else {
    return(ceiling(x)/(10^digit))
  }
}

inmid <- function(x, vec){
  vec <- sort(vec, decreasing=F)
  ifelse(x>=vec[1] & x<=vec[2], return(T), return(F))
}
