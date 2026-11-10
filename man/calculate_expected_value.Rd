\name{calculate_expected_value}
\alias{calculate_expected_value}
\title{Calculate expected outcomes}
\usage{calculate_expected_value(data)}
\arguments{
\item{data}{A validated outcome table; see \code{\link{validate_risk_data}}.}
}
\value{A data frame with one row per alternative and its \code{expected_value}.}
\description{Calculate the probability-weighted mean outcome for each alternative. Outcomes must use a common scale and direction.}
\examples{
data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
                   probability = c(0.2, 0.8))
calculate_expected_value(data)
}
