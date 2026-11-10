#' @title Plot expected outcomes by alternative
#' @description Draw a bar chart of expected outcomes. Expected value alone
#'   leaves downside risk and risk preferences to the underlying distributions;
#'   read this plot alongside them.
#' @param data An outcome table; see [validate_risk_data()].
#' @param ylab Label describing the units and direction of outcome values.
#' @return A ggplot object.
#' @examples
#' data <- data.frame(alternative = c("A", "A"), outcome = c(-10, 20),
#'                    probability = c(0.2, 0.8))
#' plot_risk_visualization(data, ylab = "Expected net benefit")
#' @export
plot_risk_visualization <- function(data, ylab = "Expected outcome") {
  result <- calculate_expected_value(data)
  result$alternative <- factor(result$alternative, levels = unique(result$alternative))
  result$label <- format(round(result$expected_value, 2), big.mark = ",", trim = TRUE)
  ggplot2::ggplot(result, ggplot2::aes(alternative, expected_value, fill = alternative)) +
    ggplot2::geom_col(width = 0.55, show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0, color = "#52514e", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = label), vjust = ifelse(result$expected_value < 0, 1.5, -0.5),
                       color = "#0b0b0b", size = 4) +
    ggplot2::scale_fill_manual(values = risk_palette(nlevels(result$alternative))) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                expand = ggplot2::expansion(mult = c(0.1, 0.12))) +
    ggplot2::labs(x = NULL, y = ylab) +
    theme_risk() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#e6e5e1", linewidth = 0.4))
}
