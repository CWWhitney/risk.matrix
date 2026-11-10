\name{decision_tree_summary}
\alias{decision_tree_summary}
\title{Summarize decision alternatives and their outcomes}
\usage{
decision_tree_summary(data)
}
\arguments{
\item{data}{A risk assessment data frame or CSV path.}
}
\value{
A data frame with one row per alternative, ordered by expected value,
including the number of outcomes, expected value, outcome range, and
probability of a negative outcome.
}
\description{
Provides a compact alternative-level summary suitable for reviewing the
consequences and expected value of a decision tree.
}
\examples{
path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
if (nzchar(path)) decision_tree_summary(import_risk_data(path))
}
