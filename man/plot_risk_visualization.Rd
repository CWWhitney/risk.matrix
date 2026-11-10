\name{plot_risk_visualization}
\alias{plot_risk_visualization}
\title{Plot expected outcomes by alternative}
\usage{plot_risk_visualization(data, ylab = "Expected outcome")}
\arguments{
\item{data}{An outcome table; see \code{\link{validate_risk_data}}.}
\item{ylab}{Label describing the units and direction of outcome values.}
}
\value{The expected value summary, invisibly; called for its plot.}
\description{Draw a bar chart of expected outcomes. Expected value alone does not describe downside risk or preferences under risk; inspect the underlying distributions alongside this plot.}
\examples{
data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
                   probability = c(0.2, 0.8))
plot_risk_visualization(data, ylab = "Expected net benefit")
}
