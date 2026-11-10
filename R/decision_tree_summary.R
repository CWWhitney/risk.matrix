#' @title Summarize a one-stage decision tree
#' @description Summarize decision alternatives and their conditional outcome
#'   distributions. This is a one-decision, one-chance-stage representation,
#'   not a general solver for sequential decision trees.
#' @param data An outcome table; see [validate_risk_data()].
#' @param threshold Numeric threshold used for downside probability.
#' @return A data frame with the number of outcome branches and risk metrics for
#'   each alternative.
#' @examples
#' data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
#'                    probability = c(0.2, 0.8))
#' decision_tree_summary(data)
#' @export
decision_tree_summary <- function(data, threshold = 0) {
  result <- calculate_risk_metrics(data, threshold)
  result$branches <- vapply(result$alternative, function(alt) {
    sum(data$alternative == alt)
  }, integer(1))
  result
}
