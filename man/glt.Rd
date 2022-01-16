\name{APLM_function}
\alias{glt}
\title{GLT test for model}
\usage{
glt(model1, model2, alpha)
}
\description{
use GLT test choose which model is better than the other.
}
\arguments{
  \item{model1}{first linear model}
  \item{model2}{second linear model}
  \item{alpha}{confidence level}

}
\examples{
data <- as.data.frame(matrix(rnorm(1000), ncol=10))
# beta <- c(beta_0, beta_1, ..., beta_n)
beta <- c(4,2,3,0,1,7,0,5)
# use function set y
y <- yget(data, beta)
fit <- lm(y ~ ., data)
f_fit <- fselect(y, data, alpha_in=0.001, alpha_out=0.005)
glt(fit, f_fit)
}

