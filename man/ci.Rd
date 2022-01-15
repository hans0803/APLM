\name{APLM_function}
\alias{ci}
\title{Confidence Interval}
\usage{
ci(x, y, who, beta, alpha)
}
\description{
get who's Confidence Interval for lm(y~., x) from beta and alpha
}
\arguments{
  \item{x}{all x of linear model, independent variable}
  \item{y}{y of linear model, a strain variable}
  \item{who}{whos Confidence Interval, it can be one value or vector, if choose vector, it will show joint Confidence Interval}
  \item{beta}{beta is vector of (beta0, beta1, ... , betan)}
  \item{alpha}{Confidence Interval width, alpha=0.05 is 95\% Confidence Interval}
}
\examples{
data <- as.data.frame(matrix(rnorm(1000), ncol=10))
data[,2] <- data[,1]*5+rnorm(100)
# beta <- c(beta_0, beta_1, ..., beta_n)
beta <- c(4,2,3,0,1,7,0,5)
# use function set y
y <- yget(data, beta)
single_ci <- ci(x=data, y=y, who=0, beta=beta, alpha=0.05)
joint_ci <- ci(x=data, y=y, who=c(1,2), beta=beta,alpha=0.05)
}

