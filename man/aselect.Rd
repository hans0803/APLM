\name{model_selection}
\alias{aselect}
\title{AIC Model selection}
\usage{
aselect(y, data)
}
\description{
do linear model from data and y, and use AIC selection to choose min aic value model return.
}
\arguments{
  \item{y}{y of linear model, a strain variable}
  \item{data}{all x of linear model, independent variable}
}
\examples{
y <- iris[,1]
x <- iris[,2:4]
fit <- aselect(y=y, data=x)
summary(fit)
}

