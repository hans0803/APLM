\name{cross_validation}
\alias{kfcv}
\title{K-fold Cross Vaildation}
\usage{
kfcv(data, seed, k)
}
\description{
split data to k parts by r seed by numbering list.
}
\arguments{
  \item{data}{what data need split}
  \item{seed}{random seed from r}
  \item{k}{need to split k part}
}
\examples{
kfcv(data=iris, seed=123, k=5)
kfcv(data=cars, seed=123, k=6)
}
