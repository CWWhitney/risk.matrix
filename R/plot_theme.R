#' @title Color palette for risk plots
#' @description Categorical colors in a fixed order, used by every plot in
#'   the package. Use with [theme_risk()] in your own ggplot2 plots.
#' @param n Number of colors, from 1 to 8.
#' @return A character vector of hex colors.
#' @examples
#' risk_palette(3)
#' @export
risk_palette <- function(n) {
  colors <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
               "#e87ba4", "#008300", "#6250d6", "#e34948")
  if (n > length(colors)) {
    stop("at most ", length(colors), " series can be plotted.")
  }
  colors[seq_len(n)]
}

#' @title ggplot2 theme for risk plots
#' @description The theme used by the package's plots: light grid, legend on
#'   top, dark-gray text. All package plots are ggplot objects, so further
#'   layers, scales and themes apply.
#' @param base_size Base font size.
#' @return A ggplot2 theme.
#' @examples
#' ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 4, 3)), ggplot2::aes(x, y)) +
#'   ggplot2::geom_line() + theme_risk()
#' @export
theme_risk <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#fcfcfb", color = NA),
      panel.background = ggplot2::element_rect(fill = "#fcfcfb", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e6e5e1", linewidth = 0.4),
      axis.title = ggplot2::element_text(color = "#52514e"),
      axis.text = ggplot2::element_text(color = "#52514e"),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = "#0b0b0b"),
      plot.title = ggplot2::element_text(color = "#0b0b0b", face = "bold"),
      plot.margin = ggplot2::margin(12, 16, 12, 12)
    )
}
