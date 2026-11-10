\name{value_function}
\alias{value_function}
\title{Calculate additive multi-attribute value}
\usage{value_function(data, weights)}
\arguments{
\item{data}{A data frame containing \code{alternative} and numeric attribute columns scaled from 0 (least preferred) to 1 (most preferred).}
\item{weights}{A named numeric vector of non-negative attribute weights summing to one. Names must match attribute columns in \code{data}.}
}
\value{A data frame with \code{alternative} and \code{value}.}
\description{Combine pre-scaled attribute values with weights using an additive value model. This model assumes appropriate preferential independence and weights representing trade-offs across the full attribute scales; it does not elicit or validate these assumptions.}
\examples{
data <- data.frame(alternative = c("A", "B"),
                   cost = c(0.8, 0.4), impact = c(0.3, 0.9))
value_function(data, c(cost = 0.6, impact = 0.4))
}
