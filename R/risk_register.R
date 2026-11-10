#' @title Import a risk register
#' @description Read a CSV risk register and check it. Each row is one risk
#'   with an annual probability and a 90% interval for its impact; see
#'   [expected_loss_table()] for the columns.
#' @param path Path to a CSV file with columns `risk`, `probability`, `lower`,
#'   `upper` and optionally `control_cost` and `control_effectiveness`.
#' @return The register as a data frame.
#' @examples
#' path <- system.file("extdata", "risk_register.csv", package = "risk.decision")
#' import_risk_register(path)
#' @export
import_risk_register <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("path must name an existing CSV file.")
  }
  check_risk_register(utils::read.csv(path, stringsAsFactors = FALSE))
}

#' @title Risk map: probability against impact
#' @description Plot each risk at its annual probability (horizontal) and its
#'   expected impact if it occurs (vertical, log scale), optionally with a bar
#'   for its 90% interval. Dashed lines join points of equal expected annual
#'   loss (probability times expected impact). When a register lists control
#'   effectiveness, arrows lead from each risk to its position after controls.
#' @param risks A risk register; see [expected_loss_table()].
#' @param controls Show residual positions after controls (default: when any
#'   control has non-zero effectiveness).
#' @param intervals Draw the 90% impact interval of each risk.
#' @param ylab Label for the impact axis.
#' @return A ggplot object.
#' @examples
#' path <- system.file("extdata", "risk_register.csv", package = "risk.decision")
#' plot_risk_register(import_risk_register(path))
#' @export
plot_risk_register <- function(risks, controls = any(risks$control_effectiveness > 0),
                               intervals = FALSE, ylab = "Impact if the risk occurs") {
  risks <- check_risk_register(risks)
  params <- lognormal_from_interval(risks$lower, risks$upper)
  d <- data.frame(risk = risks$risk, probability = risks$probability,
                  impact = exp(params$meanlog + params$sdlog^2 / 2),
                  lower = risks$lower, upper = risks$upper,
                  residual = risks$probability * (1 - risks$control_effectiveness))
  loss <- d$probability * d$impact
  xs <- seq(0.01, 1, length.out = 200)
  levels <- 10^seq(floor(log10(min(loss * (1 - risks$control_effectiveness)))),
                   ceiling(log10(max(loss))), by = 1)
  iso <- do.call(rbind, lapply(levels, function(l) {
    data.frame(level = l, probability = xs, impact = l / xs)
  }))
  y_range <- range(c(d$lower, d$upper))
  iso <- iso[iso$impact >= y_range[1] / 1.5 & iso$impact <= y_range[2] * 1.5, ]
  iso_labels <- do.call(rbind, lapply(split(iso, iso$level), function(g) {
    g[which.max(g$probability), ]
  }))
  iso_labels$label <- paste0(scales::label_number(scale_cut = scales::cut_short_scale())(iso_labels$level),
                             " a year")
  p <- ggplot2::ggplot(d, ggplot2::aes(probability, impact)) +
    ggplot2::geom_line(data = iso, ggplot2::aes(group = level), color = "#c3c2b7",
                       linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_text(data = iso_labels, ggplot2::aes(label = label), color = "#8a897f",
                       size = 3.1, hjust = 1, vjust = -0.6)
  if (isTRUE(intervals)) {
    p <- p + ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper),
                                     color = "#2a78d6", linewidth = 0.8, alpha = 0.35)
  }
  if (isTRUE(controls)) {
    p <- p +
      ggplot2::geom_segment(ggplot2::aes(xend = residual, yend = impact),
                            color = "#52514e", linewidth = 0.5,
                            arrow = grid::arrow(length = grid::unit(0.12, "inches"))) +
      ggplot2::geom_point(ggplot2::aes(x = residual), shape = 21, size = 3.5,
                          fill = "#fcfcfb", color = "#2a78d6", stroke = 1.2)
  }
  p +
    ggplot2::geom_point(shape = 21, size = 4.5, fill = "#2a78d6", color = "#fcfcfb",
                        stroke = 1.2) +
    ggrepel::geom_text_repel(ggplot2::aes(label = risk), color = "#0b0b0b", size = 3.6,
                             seed = 1, box.padding = 0.8, nudge_y = 0.12, min.segment.length = 0.2,
                             segment.color = "#c3c2b7") +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::label_percent(),
                                expand = ggplot2::expansion(mult = c(0.02, 0.04))) +
    ggplot2::scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    ggplot2::labs(x = "Probability in a year", y = ylab) +
    theme_risk() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "#e6e5e1", linewidth = 0.4))
}

#' @title Expected annual loss by risk
#' @description Horizontal bars of expected annual loss for each risk, largest
#'   first, before and after controls when the register lists control
#'   effectiveness.
#' @param risks A risk register; see [expected_loss_table()].
#' @param xlab Label for the loss axis.
#' @return A ggplot object.
#' @examples
#' path <- system.file("extdata", "risk_register.csv", package = "risk.decision")
#' plot_expected_loss(import_risk_register(path))
#' @export
plot_expected_loss <- function(risks, xlab = "Expected annual loss") {
  tab <- expected_loss_table(risks)
  tab$risk <- factor(tab$risk, levels = rev(tab$risk[order(tab$expected_inherent_loss)]))
  long <- data.frame(
    risk = rep(tab$risk, 2),
    stage = factor(rep(c("Before controls", "After controls"), each = nrow(tab)),
                   levels = c("Before controls", "After controls")),
    loss = c(tab$expected_inherent_loss, tab$expected_residual_loss))
  if (all(tab$expected_inherent_loss == tab$expected_residual_loss)) {
    long <- long[long$stage == "Before controls", ]
  }
  ggplot2::ggplot(long, ggplot2::aes(loss, risk, fill = stage)) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(width = 0.7, reverse = TRUE), width = 0.7) +
    ggplot2::scale_fill_manual(values = c("Before controls" = "#9bbfee",
                                          "After controls" = "#2a78d6")) +
    ggplot2::scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = xlab, y = NULL) +
    theme_risk() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(color = "#e6e5e1", linewidth = 0.4))
}
