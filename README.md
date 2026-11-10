# risk.decision

An R package for risk assessment and decision analysis. It compares alternatives
using probabilities and outcomes, and draws risk matrices shaded by expected
loss. `project_selection` is an invented example; `case_study()` provides inputs
from published decision analyses with Crossref-verified DOIs.

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

## Tools

All plots are ggplot2 objects. `theme_risk()` and `risk_palette()` give custom
plots the same look.

- `expected_loss_table()`, `simulate_annual_loss()`, `loss_exceedance_curve()`,
  `plot_loss_exceedance()`: expected loss and simulated annual loss from an annual
  probability and a 90% interval for impact per risk.
- `import_risk_register()`, `plot_risk_register()`, `plot_expected_loss()`: a risk
  report from a spreadsheet of estimates; see `inst/extdata/risk_register.csv`.
- `plot_risk_heatmap()`, `compare_matrix_ranking()`: the probability-by-impact
  grid, shaded by expected loss, and a comparison of matrix scores with expected loss.
- `certain_monetary_equivalent()`, `fit_risk_tolerance()`: exponential-utility
  risk tolerance from just-barely-acceptable bets.
- `net_benefit_matrix()` with `evpi()` (re-exported from the voi package):
  expected value of perfect information.
- `dawes_score()`: equal-weight z-score ranking.
- `case_study()`: published case studies (see `inst/REFERENCES.bib`).
