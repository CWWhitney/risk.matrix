\name{generate_risk_matrix}
\alias{generate_risk_matrix}
\title{Create a decision-quality risk visualization}
\usage{
generate_risk_matrix(data, plot = TRUE, labels = TRUE)
}
\arguments{
\item{data}{A risk assessment data frame or CSV path.}
\item{plot}{If \code{TRUE}, draw the risk visualization using base graphics.}
\item{labels}{If \code{TRUE}, label plotted points with their outcome names.}
}
\value{
A data frame containing the original outcomes, absolute impact, and
descriptive probability and impact bands.
}
\description{
Returns all outcome-level data with continuous probabilities and impacts
intact. When plotting is enabled, it draws an outcome scatterplot rather
than reducing alternatives to a single binned risk score.
}
\examples{
path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
if (nzchar(path)) generate_risk_matrix(import_risk_data(path))
}
