\name{model_selection}
\alias{fselect}
\title{F-value stepwise Model selection}
\usage{
fselect(y, data, alpha_in, alpha_out, mode)
}
\description{
do linear model from data and y, and use F-value selection to choose model return.
can select forward or backward run this function, alpha value decide p-value select accuracy.
and if model have Multicollinearity problem, function will warning it.
}
\arguments{
  \item{y}{y of linear model, a strain variable}
  \item{data}{all x of linear model, independent variable}
  \item{alpha_in}{new variable p-value should smaller than alpha in, it can be select into model}
  \item{alpha_out}{if p-value bigger than alpha out, the variable will be dropout to model}
  \item{mode}{can choose forward or backward stepwise selection}
}
\examples{
y <- iris[,1]
x <- iris[,2:4]
fit <- fselect(y=y, data=x, alpha_in=0.01, alpha_out=0.05, mode="backward")
summary(fit)

y <- MASS::Boston[,14]
x <- MASS::Boston[,-14]
fit <- fselect(y=y, data=x, alpha_in=0.01, alpha_out=0.05, mode="forward")
summary(fit)

data <- as.data.frame(matrix(rnorm(1000), ncol=10))
data[,2] <- data[,1]*5+rnorm(100)
# beta <- c(beta_0, beta_1, ..., beta_n)
beta <- c(4,2,3,0,1,7,0,5)
# use function set y
y <- yget(data, beta)
mult_fit <- fselect(y, data, alpha_in=0.001, alpha_out=0.005)
summary(mult_fit)
}

