#' @title Calculate expected outcomes
#' @description Calculate the probability-weighted mean outcome for each
#'   alternative. Outcomes must use a common scale and direction.
#' @param data A validated outcome table; see [validate_risk_data()].
#' @return A data frame with one row per alternative and its `expected_value`.
#' @examples
#' data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
#'                    probability = c(0.2, 0.8))
#' calculate_expected_value(data)
#' @export
calculate_expected_value <- function(data) {
  validate_risk_data(data)
  alternatives <- unique(as.character(data$alternative))
  values <- vapply(alternatives, function(alt) {
    rows <- data$alternative == alt
    sum(data$outcome[rows] * data$probability[rows])
  }, numeric(1))
  data.frame(alternative = alternatives, expected_value = unname(values))
}
