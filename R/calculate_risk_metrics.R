#' Calculate outcome metrics for each decision alternative
#'
#' Treats rows for an alternative as mutually exclusive outcomes in a
#' probability distribution. In addition to expected value, it reports outcome
#' spread and the probability of a negative outcome.
#'
#' @param data A risk assessment data frame or CSV path.
#'
#' @return A data frame with one row per alternative.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
#' if (nzchar(path)) calculate_risk_metrics(import_risk_data(path))
calculate_risk_metrics <- function(data) {
  if (is.character(data) && length(data) == 1L) {
    data <- import_risk_data(data)
  }
  data <- validate_risk_data(data)
  groups <- split(data, data$alternative)
  summaries <- lapply(groups, function(outcomes) {
    probability <- outcomes$probability
    value <- outcomes$value
    expected_value <- sum(probability * value)
    data.frame(
      alternative = as.character(outcomes$alternative[[1L]]),
      expected_value = expected_value,
      variance = sum(probability * (value - expected_value)^2),
      standard_deviation = sqrt(sum(probability * (value - expected_value)^2)),
      minimum_outcome = min(value),
      maximum_outcome = max(value),
      probability_negative = sum(probability[value < 0]),
      stringsAsFactors = FALSE
    )
  })
  rownames_result <- do.call(rbind, summaries)
  rownames(rownames_result) <- NULL
  rownames_result
}
