\name{my_function}
\alias{inmid}
\title{In the Mid}
\usage{
inmid(x, vec)
}
\description{
check x is biggger than vec[1] and smaller than vec[2].
}
\arguments{
  \item{x}{a numeric parameter}
  \item{vec}{a numeric vector, vector length is equal to two}
}
\examples{
x <- 3
vec1 <- c(1, 3)
isTRUE(inmid(x, vec1))
vec2 <- c(3, 4)
isTRUE(inmid(x, vec2))
vec3 <- c(1, 2)
isTRUE(inmid(x, vec3))
}
