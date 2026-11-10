#' @title Validate a risk outcome table
#' @description Check that each alternative has a complete probability
#'   distribution over numeric outcomes. Rows represent mutually exclusive,
#'   collectively exhaustive outcomes conditional on the alternative.
#' @param data A data frame with `alternative`, `outcome`, and `probability`
#'   columns. Optional columns may describe the uncertain scenarios.
#' @return The input data frame, invisibly. Invalid inputs cause an error.
#' @examples
#' validate_risk_data(data.frame(alternative = c("A", "A"),
#'                               outcome = c(-10, 20), probability = c(0.2, 0.8)))
#' @export
validate_risk_data <- function(data) {
  if (!is.data.frame(data) || !nrow(data) ||
      !all(c("alternative", "outcome", "probability") %in% names(data))) {
    stop("data must be a non-empty data frame with alternative, outcome, and probability columns.")
  }
  if (anyDuplicated(names(data))) {
    stop("data must not have duplicate column names.")
  }
  if (!is.character(data$alternative) && !is.factor(data$alternative)) {
    stop("alternative must contain names.")
  }
  if (anyNA(data$alternative) || any(!nzchar(trimws(as.character(data$alternative))))) {
    stop("alternative names must not be missing or blank.")
  }
  if (!is.numeric(data$outcome) || any(!is.finite(data$outcome))) {
    stop("outcome must contain finite numbers.")
  }
  if (!is.numeric(data$probability) || any(!is.finite(data$probability)) ||
      any(data$probability < 0 | data$probability > 1)) {
    stop("probability must contain finite numbers between 0 and 1.")
  }
  for (alternative in unique(as.character(data$alternative))) {
    if (abs(sum(data$probability[data$alternative == alternative]) - 1) > 1e-8) {
      stop("probabilities must sum to 1 for each alternative: ", alternative)
    }
  }
  invisible(data)
}
