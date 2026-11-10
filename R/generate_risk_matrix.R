#' @title Plot probability and consequence for each outcome
#' @description Plot each outcome of each alternative at its conditional
#'   probability (horizontal) and its outcome value (vertical). Overlapping
#'   points can conceal one another, so read the plot with the full
#'   distributions.
#' @param data An outcome table; see [validate_risk_data()].
#' @param xlab Label for probability axis.
#' @param ylab Label describing the units and direction of outcome values.
#' @return A ggplot object.
#' @examples
#' data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
#'                    probability = c(0.2, 0.8))
#' generate_risk_matrix(data, ylab = "Net benefit")
#' @export
generate_risk_matrix <- function(data, xlab = "Probability conditional on alternative",
                                 ylab = "Outcome") {
  validate_risk_data(data)
  data$alternative <- factor(as.character(data$alternative),
                             levels = unique(as.character(data$alternative)))
  p <- ggplot2::ggplot(data, ggplot2::aes(probability, outcome, fill = alternative))
  if (min(data$outcome) < 0 && max(data$outcome) > 0) {
    p <- p + ggplot2::geom_hline(yintercept = 0, color = "#52514e", linewidth = 0.4)
  }
  p <- p +
    ggplot2::geom_point(shape = 21, size = 4.5, stroke = 1.2, color = "#fcfcfb") +
    ggplot2::scale_fill_manual(values = risk_palette(nlevels(data$alternative))) +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::label_percent(),
                                expand = ggplot2::expansion(mult = 0.06)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(x = xlab, y = ylab) +
    theme_risk()
  if ("uncertain_variable" %in% names(data)) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = uncertain_variable),
                                      color = "#52514e", size = 3.4, seed = 1,
                                      box.padding = 0.5, min.segment.length = 0.2,
                                      segment.color = "#c3c2b7", show.legend = FALSE)
  }
  p
}
