\name{cross_validation}
\alias{loocv}
\title{Leave one out Cross Vaildation}
\usage{
loocv(data)
}
\description{
Split data into datasets of length numbers by numbering list according to their length.
}
\arguments{
  \item{data}{what data need split}
}
\examples{
loocv(data=iris)
loocv(data=cars)
}
