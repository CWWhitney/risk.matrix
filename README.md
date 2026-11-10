# risk.decision

An R package foundation for risk assessment and decision analysis using
decision-quality principles. The package makes alternatives, uncertain
outcomes, probabilities, and outcome values explicit instead of reducing a
decision to a single probability-impact cell.

## Install and try

```r
# Install from a local checkout
install.packages(".", repos = NULL, type = "source")

library(risk.decision)
project <- import_risk_data(system.file(
  "extdata", "project-risk-template.csv", package = "risk.decision"
))

calculate_risk_metrics(project)
decision_tree_summary(project)
generate_risk_matrix(project)
```

The editable CSV examples are in `inst/extdata/`: `project-risk-template.csv`,
`strategic-alternatives.csv`, and `portfolio-selection.csv`. They use synthetic
illustrative values, not data copied from cited authors. Each row represents a
mutually exclusive outcome, and probabilities must sum to one separately for
each alternative. `value` must be a common, decision-relevant scale.

`generate_risk_matrix()` plots outcome-level probability against absolute
outcome value and returns the original rows with descriptive risk bands. The
bands do not replace decision-maker preferences or provide a standalone
decision rule. See the three vignettes for decision quality concepts, one-to-one
project analysis, and uncertain strategic outcomes.

## Development

Documentation uses roxygen2 comments. Tests use testthat edition 3. Vignettes
are built with knitr and rmarkdown.

## References

- Howard, R. A., and Abbas, A. E. (2015). *Foundations of Decision Analysis*.
  Pearson.
- Keeney, R. L., and Raiffa, H. (1976). *Decisions with Multiple Objectives:
  Preferences and Value Tradeoffs*. Wiley.
- Hubbard, D. W. (2009). *The Failure of Risk Management: Why It's Broken and
  How to Fix It*. Wiley.
