#' @title Calculate risk metrics
#' @description Summarize each alternative's outcome distribution using its
#'   expected value, population variance, standard deviation, and probability
#'   of an outcome strictly below a chosen threshold.
#' @param data An outcome table; see [validate_risk_data()].
#' @param threshold Numeric outcome threshold for the downside probability.
#' @return A data frame with one row per alternative and the four metrics.
#' @examples
#' data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
#'                    probability = c(0.2, 0.8))
#' calculate_risk_metrics(data, threshold = 0)
#' @export
calculate_risk_metrics <- function(data, threshold = 0) {
  validate_risk_data(data)
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold)) {
    stop("threshold must be one finite number.")
  }
  result <- calculate_expected_value(data)
  result$variance <- vapply(seq_len(nrow(result)), function(i) {
    rows <- data$alternative == result$alternative[i]
    sum(data$probability[rows] * (data$outcome[rows] - result$expected_value[i])^2)
  }, numeric(1))
  result$standard_deviation <- sqrt(result$variance)
  result$downside_probability <- unname(vapply(result$alternative, function(alt) {
    sum(data$probability[data$alternative == alt & data$outcome < threshold])
  }, numeric(1)))
  result
}
