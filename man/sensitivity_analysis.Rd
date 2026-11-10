\name{sensitivity_analysis}
\alias{sensitivity_analysis}
\title{Explore sensitivity to a branch probability}
\usage{sensitivity_analysis(data, alternative, branch, probabilities = seq(0, 1, 0.1))}
\arguments{
\item{data}{An outcome table; see \code{\link{validate_risk_data}}.}
\item{alternative}{Name of the alternative to vary.}
\item{branch}{Row number in \code{data} of the outcome to vary.}
\item{probabilities}{Numeric vector of trial probabilities between 0 and 1.}
}
\value{A data frame with trial \code{probability}, \code{alternative}, and \code{expected_value} columns.}
\description{Vary one outcome probability for one alternative, keeping the relative probabilities of its other outcomes fixed. Expected values of all alternatives are returned for comparison. This is a one-way sensitivity analysis, not a joint uncertainty or value-of-information analysis.}
\examples{
data <- data.frame(alternative = c("A", "A", "B"),
                   outcome = c(-10, 20, 5), probability = c(0.2, 0.8, 1))
sensitivity_analysis(data, "A", branch = 1, probabilities = c(0.1, 0.5))
}
