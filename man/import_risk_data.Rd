\name{import_risk_data}
\alias{import_risk_data}
\title{Import a CSV risk assessment}
\usage{
import_risk_data(file)
}
\arguments{
\item{file}{Path to a CSV risk assessment file.}
}
\value{
A validated data frame.
}
\description{
Reads a CSV file with one mutually exclusive outcome per row. The required
columns are \code{alternative}, \code{uncertain_variable}, \code{outcome},
\code{probability}, and \code{value}; probabilities must sum to one within
each alternative.
}
\examples{
path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
if (nzchar(path)) import_risk_data(path)
}
