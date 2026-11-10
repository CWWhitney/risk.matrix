# risk.decision

An R package foundation for transparent risk assessment and decision analysis.
It compares alternatives using explicit probabilities and outcomes, rather than
ordinal risk scores. The project examples are illustrative, not published cases.

Install the package from a local checkout with `R CMD INSTALL .`, then:

```r
library(risk.decision)
path <- system.file("extdata", "project_selection.csv", package = "risk.decision")
projects <- import_risk_data(path)
calculate_risk_metrics(projects)
generate_risk_matrix(projects, ylab = "Net benefit")
```

Input CSV files have one row per mutually exclusive outcome per alternative,
with columns `alternative`, `outcome`, and `probability`; probabilities must sum
to one within each alternative. An optional `uncertain_variable` column can name
each scenario. See `data-raw/risk_template.csv` and the installed examples.
Outcomes should share a scale and direction. See the vignettes for assumptions,
limitations, and decision-quality context.
