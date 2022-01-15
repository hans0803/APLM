\name{cross_validation}
\alias{hcv}
\title{Holdout Cross Vaildation}
\usage{
hcv(data, seed, prob)
}
\description{
split data randomly obtain prob part data on seed, and return 1-prob part with prob part data by numbering list.
}
\arguments{
  \item{data}{what data need split}
  \item{seed}{random seed from r}
  \item{prob}{obtain how much prob part}
}
\examples{
hcv(data=iris, seed=123, prob=0.2)
hcv(data=iris, seed=123, prob=0.6)
}
