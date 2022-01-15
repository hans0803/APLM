\name{APLM_function}
\alias{yget}
\title{Use matrix and vector build y}
\usage{
yget(data, beta, e, way)
}
\description{
use the method of multiplying the beta vector by the data matrix to calculate the value of y.
}
\arguments{
  \item{data}{data matrix}
  \item{beta}{beta is vector of (beta0, beta1, ... , betan)}
  \item{e}{random error tern e, length is data length}
  \item{war}{way=1 or 2 is 2 way to calculate y}
}
\examples{
x <- data.frame(matrix(rnorm(300), ncol=3))
beta <- c(4,2,3)
y1 <- yget(data=x, beta=beta, e=rnorm(dim(x)[1]), way=1)
y2 <- yget(data=x, beta=beta, e=rnorm(dim(x)[1]), way=2)
}

