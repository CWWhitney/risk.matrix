\name{calculate_risk_metrics}
\alias{calculate_risk_metrics}
\title{Calculate risk metrics}
\usage{calculate_risk_metrics(data, threshold = 0)}
\arguments{
\item{data}{An outcome table; see \code{\link{validate_risk_data}}.}
\item{threshold}{Numeric outcome threshold for the downside probability.}
}
\value{A data frame with one row per alternative and the four metrics.}
\description{Summarize each alternative's outcome distribution using its expected value, population variance, standard deviation, and probability of an outcome strictly below a chosen threshold.}
\examples{
data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
                   probability = c(0.2, 0.8))
calculate_risk_metrics(data, threshold = 0)
}
