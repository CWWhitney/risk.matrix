#' @title Plot probability and consequence for each outcome
#' @description Draw an outcome-level probability-consequence display as a
#'   quantitative alternative to a categorical risk matrix. It preserves
#'   conditional probabilities and signed outcomes instead of multiplying
#'   ordinal ranks. Points are outcomes, not independent risks; overlapping
#'   points may conceal one another. Compare complete distributions as well.
#' @param data An outcome table; see [validate_risk_data()].
#' @param xlab Label for probability axis.
#' @param ylab Label describing the units and direction of outcome values.
#' @return The validated input data, invisibly; called for its plot.
#' @examples
#' data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
#'                    probability = c(0.2, 0.8))
#' generate_risk_matrix(data, ylab = "Net benefit")
#' @export
generate_risk_matrix <- function(data, xlab = "Probability conditional on alternative",
                                 ylab = "Outcome") {
  validate_risk_data(data)
  alternatives <- unique(as.character(data$alternative))
  groups <- match(as.character(data$alternative), alternatives)
  graphics::plot(data$probability, data$outcome, xlim = c(0, 1),
                 xlab = xlab, ylab = ylab, col = groups, pch = 19)
  graphics::legend("topright", legend = alternatives, col = seq_along(alternatives),
                   pch = 19, title = "Alternative")
  invisible(data)
}
