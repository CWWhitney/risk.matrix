#' Summarize decision alternatives and their outcomes
#'
#' Provides a compact alternative-level summary suitable for reviewing the
#' consequences and expected value of a decision tree.
#'
#' @param data A risk assessment data frame or CSV path.
#'
#' @return A data frame with one row per alternative, ordered by expected value,
#'   including the number of outcomes, expected value, outcome range, and
#'   probability of a negative outcome.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
#' if (nzchar(path)) decision_tree_summary(import_risk_data(path))
decision_tree_summary <- function(data) {
  if (is.character(data) && length(data) == 1L) {
    data <- import_risk_data(data)
  }
  data <- validate_risk_data(data)
  summary <- calculate_risk_metrics(data)
  summary$outcome_count <- as.integer(table(data$alternative)[summary$alternative])
  summary <- summary[order(summary$expected_value, decreasing = TRUE), , drop = FALSE]
  summary$preferred_by_expected_value <- seq_len(nrow(summary)) == 1L
  rownames(summary) <- NULL
  summary
}
