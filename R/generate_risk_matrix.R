#' Create a decision-quality risk visualization
#'
#' Returns all outcome-level data with continuous probabilities and impacts
#' intact. When plotting is enabled, it draws an outcome scatterplot rather
#' than reducing alternatives to a single binned risk score.
#'
#' @param data A risk assessment data frame or CSV path.
#' @param plot If `TRUE`, draw the risk visualization using base graphics.
#' @param labels If `TRUE`, label plotted points with their outcome names.
#'
#' @return A data frame containing the original outcomes, absolute impact, and
#'   descriptive probability and impact bands.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
#' if (nzchar(path)) generate_risk_matrix(import_risk_data(path))
generate_risk_matrix <- function(data, plot = TRUE, labels = TRUE) {
  if (is.character(data) && length(data) == 1L) {
    data <- import_risk_data(data)
  }
  data <- validate_risk_data(data)
  data$impact <- abs(data$value)
  data$probability_level <- cut(
    data$probability,
    breaks = c(-Inf, 1 / 3, 2 / 3, Inf),
    labels = c("low", "medium", "high"),
    ordered_result = TRUE
  )
  impact_cutoffs <- stats::quantile(data$impact, c(1 / 3, 2 / 3), names = FALSE)
  data$impact_level <- ifelse(
    data$impact <= impact_cutoffs[[1L]],
    "low",
    ifelse(data$impact <= impact_cutoffs[[2L]], "medium", "high")
  )
  data$impact_level <- factor(data$impact_level, levels = c("low", "medium", "high"), ordered = TRUE)

  if (isTRUE(plot)) {
    impact_range <- range(c(0, data$impact))
    if (diff(impact_range) == 0) impact_range <- c(0, 1)
    alternative_colors <- seq_along(unique(data$alternative))
    names(alternative_colors) <- unique(data$alternative)
    graphics::plot(
      data$probability,
      data$impact,
      xlim = c(0, 1),
      ylim = impact_range,
      pch = 19,
      col = alternative_colors[as.character(data$alternative)],
      xlab = "Outcome probability",
      ylab = "Absolute outcome value (impact)",
      main = "Outcome-level risk by decision alternative"
    )
    if (isTRUE(labels)) {
      graphics::text(data$probability, data$impact, labels = data$outcome, pos = 3, cex = 0.7)
    }
    graphics::legend(
      "topright",
      legend = names(alternative_colors),
      col = alternative_colors,
      pch = 19,
      title = "Alternative",
      cex = 0.8
    )
  }
  data
}
