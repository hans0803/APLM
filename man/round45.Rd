\name{my_function}
\alias{round45}
\title{True Rounding}
\usage{
round45(x, digit)
}
\description{
r round is not real round, it solve r round to even number problem.
}
\arguments{
  \item{x}{a numeric parameter}
  \item{digit}{x round to digit decimal places}
}
\examples{
x <- 2.25
r_round <- round(x, digit=1)
my_round <- round45(x, digit=1)
print(paste("r_round: 2.25 round digit=1 is", r_round))
print(paste("my_round: 2.25 round digit=1 is", my_round))
}
