\name{calculate_risk_metrics}
\alias{calculate_risk_metrics}
\title{Calculate outcome metrics for each decision alternative}
\usage{
calculate_risk_metrics(data)
}
\arguments{
\item{data}{A risk assessment data frame or CSV path.}
}
\value{
A data frame with one row per alternative.
}
\description{
Treats rows for an alternative as mutually exclusive outcomes in a probability
distribution. In addition to expected value, it reports outcome spread and
the probability of a negative outcome.
}
\examples{
path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
if (nzchar(path)) calculate_risk_metrics(import_risk_data(path))
}
