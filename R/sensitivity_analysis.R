#' @title Explore sensitivity to a branch probability
#' @description Vary one outcome probability for one alternative, keeping
#'   the relative probabilities of its other outcomes fixed. Expected values of
#'   all alternatives are returned for comparison. This is a one-way
#'   sensitivity analysis, not a joint uncertainty or value-of-information
#'   analysis.
#' @param data An outcome table; see [validate_risk_data()].
#' @param alternative Name of the alternative to vary.
#' @param branch Row number in `data` of the outcome to vary.
#' @param probabilities Numeric vector of trial probabilities between 0 and 1.
#' @return A data frame with trial `probability`, `alternative`, and
#'   `expected_value` columns.
#' @examples
#' data <- data.frame(alternative = c("A", "A", "B"),
#'                    outcome = c(-10, 20, 5), probability = c(0.2, 0.8, 1))
#' sensitivity_analysis(data, "A", branch = 1, probabilities = c(0.1, 0.5))
#' @export
sensitivity_analysis <- function(data, alternative, branch, probabilities = seq(0, 1, 0.1)) {
  validate_risk_data(data)
  if (!is.character(alternative) || length(alternative) != 1L ||
      is.na(alternative) || !alternative %in% data$alternative) {
    stop("alternative must name an alternative in data.")
  }
  if (!is.numeric(branch) || length(branch) != 1L || !is.finite(branch) ||
      branch != floor(branch) || branch < 1 || branch > nrow(data) ||
      data$alternative[branch] != alternative) {
    stop("branch must be a row number belonging to the selected alternative.")
  }
  if (!is.numeric(probabilities) || !length(probabilities) ||
      any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1)) {
    stop("probabilities must be finite numbers between 0 and 1.")
  }
  other <- which(data$alternative == alternative & seq_len(nrow(data)) != branch)
  other_total <- sum(data$probability[other])
  if (other_total == 0 && any(probabilities < 1)) {
    stop("cannot redistribute probability when the other branches have zero total probability.")
  }
  results <- lapply(probabilities, function(p) {
    trial <- data
    trial$probability[branch] <- p
    if (other_total > 0) {
      trial$probability[other] <- data$probability[other] * (1 - p) / other_total
    }
    summary <- calculate_expected_value(trial)
    data.frame(probability = p, summary)
  })
  do.call(rbind, results)
}
