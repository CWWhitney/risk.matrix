#' @title Plot expected outcomes by alternative
#' @description Draw a bar chart of expected outcomes. Expected value alone
#'   does not describe downside risk or preferences under risk; inspect the
#'   underlying distributions alongside this plot.
#' @param data An outcome table; see [validate_risk_data()].
#' @param ylab Label describing the units and direction of outcome values.
#' @return The expected value summary, invisibly; called for its plot.
#' @examples
#' data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
#'                    probability = c(0.2, 0.8))
#' plot_risk_visualization(data, ylab = "Expected net benefit")
#' @export
plot_risk_visualization <- function(data, ylab = "Expected outcome") {
  result <- calculate_expected_value(data)
  graphics::barplot(result$expected_value, names.arg = result$alternative,
                    ylab = ylab)
  invisible(result)
}
