#' @title Risk matrix shaded by expected loss
#' @description Draw a probability-by-impact grid as used in risk registers:
#'   rows are probability bands, columns are impact bands and each risk sits in
#'   its cell. Cell shading is the expected loss (probability times impact) at
#'   the center of the cell, printed in the cell. The key below the plot gives
#'   each risk's own expected loss.
#'
#'   The default bands are those of the example matrix in Thomas, Bratvold and
#'   Bickel (2014, Fig. 1): six probability bands from 1% to 40% and six impact
#'   bands from 100 thousand to 20 million. The top impact band has no upper
#'   limit; its center assumes the same ratio of high to low as the band below.
#' @param risks A risk register; see [expected_loss_table()]. Risks with an
#'   interval use the expected impact given the event.
#' @param probability_breaks Increasing probability band limits from 0 to 1.
#' @param impact_breaks Increasing impact band limits starting at 0. The last
#'   limit may be `Inf`.
#' @param probability_labels,impact_labels Band names, one per band.
#' @param colors Three colors for low, middle and high expected loss.
#' @param currency Symbol shown before money values.
#' @return A ggplot object.
#' @examples
#' risks <- case_study("thomas_2014_drilling")
#' plot_risk_heatmap(risks)
#' @export
plot_risk_heatmap <- function(risks,
                              probability_breaks = c(0, 0.01, 0.05, 0.10, 0.20, 0.40, 1),
                              impact_breaks = c(0, 1e5, 2.5e5, 1e6, 5e6, 2e7, Inf),
                              probability_labels = c("Rare", "Remote", "Unlikely", "Seldom",
                                                     "Occasional", "Likely"),
                              impact_labels = c("Incidental", "Minor", "Moderate", "Major",
                                                "Severe", "Catastrophic"),
                              colors = c("#8fc98f", "#f4df7a", "#e8675b"),
                              currency = "$") {
  g <- matrix_grid(probability_breaks, impact_breaks, probability_labels, impact_labels)
  tab <- expected_loss_table(risks)
  risks <- check_risk_register(risks)
  params <- lognormal_from_interval(risks$lower, risks$upper)
  impact <- exp(params$meanlog + params$sdlog^2 / 2)
  money <- function(x, accuracy = 0.1) short_money(x, currency, accuracy)
  cells <- expand.grid(prow = seq_len(g$np), icol = seq_len(g$ni))
  cells$loss <- g$p_mid[cells$prow] * g$i_mid[cells$icol]
  cells$text <- money(cells$loss)
  px <- cut(risks$probability, probability_breaks, labels = FALSE, include.lowest = TRUE)
  ix <- cut(impact, impact_breaks, labels = FALSE, include.lowest = TRUE)
  if (anyNA(px) || anyNA(ix)) {
    stop("every risk must fall inside the probability and impact bands.")
  }
  pts <- data.frame(n = seq_len(nrow(risks)), risk = risks$risk, prow = px, icol = ix)
  pts$k <- ave(pts$n, pts$prow, pts$icol, FUN = seq_along)
  pts$m <- ave(pts$n, pts$prow, pts$icol, FUN = length)
  pts$dx <- ifelse(pts$m == 1, 0, (pts$k - (pts$m + 1) / 2) * 0.3)
  key <- paste0(pts$n, " ", pts$risk, " (", money(tab$expected_inherent_loss, 0.01), " a year)")
  ggplot2::ggplot(cells, ggplot2::aes(icol, prow)) +
    ggplot2::geom_tile(ggplot2::aes(fill = loss), color = "#fcfcfb", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(y = prow - 0.36, label = text), color = "#52514e",
                       size = 2.9) +
    ggplot2::geom_point(data = pts, ggplot2::aes(icol + dx, prow + 0.08), shape = 21, size = 7.5,
                        fill = "#fcfcfb", color = "#0b0b0b", stroke = 1) +
    ggplot2::geom_text(data = pts, ggplot2::aes(icol + dx, prow + 0.08, label = n),
                       color = "#0b0b0b", size = 3.6, fontface = "bold") +
    ggplot2::scale_fill_gradientn(colors = colors, trans = "log10",
                                  labels = scales::label_number(scale_cut = scales::cut_short_scale(),
                                                                prefix = currency),
                                  name = "Expected loss\nat cell center") +
    ggplot2::scale_x_continuous(breaks = seq_len(g$ni), labels = g$i_axis, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq_len(g$np), labels = g$p_axis, expand = c(0, 0)) +
    ggplot2::labs(x = "Impact if the risk occurs", y = "Probability in a year",
                  caption = paste(strwrap(paste(key, collapse = "   "), 95), collapse = "\n")) +
    theme_risk() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right",
                   legend.title = ggplot2::element_text(color = "#52514e", size = 9),
                   axis.text = ggplot2::element_text(color = "#52514e", size = 9),
                   plot.caption = ggplot2::element_text(color = "#0b0b0b", hjust = 0, size = 9))
}

#' @title Compare a matrix ranking with the expected-loss ranking
#' @description Score each risk the way a risk matrix does (the product of its
#'   probability band rating and impact band rating) and rank the risks by that
#'   score, then rank them by expected loss. Differences show where the
#'   banding changes the order. Ratings run from 1 (lowest band) upward with
#'   `scale = "ascending"` and downward with `scale = "descending"`; both
#'   conventions appear in practice (Thomas, Bratvold and Bickel 2014, Table 4).
#' @param risks A risk register; see [expected_loss_table()].
#' @inheritParams plot_risk_heatmap
#' @param scale `"ascending"` (a higher rating means a higher band, the highest
#'   score ranks first) or `"descending"` (a lower rating means a higher band,
#'   the lowest score ranks first).
#' @return A data frame with the risk, its bands and ratings, `matrix_score`,
#'   `matrix_rank`, `expected_loss` and `expected_loss_rank`.
#' @examples
#' compare_matrix_ranking(case_study("thomas_2014_drilling"))
#' compare_matrix_ranking(case_study("thomas_2014_drilling"), scale = "descending")
#' @export
compare_matrix_ranking <- function(risks,
                                   probability_breaks = c(0, 0.01, 0.05, 0.10, 0.20, 0.40, 1),
                                   impact_breaks = c(0, 1e5, 2.5e5, 1e6, 5e6, 2e7, Inf),
                                   scale = c("ascending", "descending")) {
  scale <- match.arg(scale)
  tab <- expected_loss_table(risks)
  risks <- check_risk_register(risks)
  params <- lognormal_from_interval(risks$lower, risks$upper)
  impact <- exp(params$meanlog + params$sdlog^2 / 2)
  px <- cut(risks$probability, probability_breaks, labels = FALSE, include.lowest = TRUE)
  ix <- cut(impact, impact_breaks, labels = FALSE, include.lowest = TRUE)
  if (anyNA(px) || anyNA(ix)) {
    stop("every risk must fall inside the probability and impact bands.")
  }
  if (scale == "descending") {
    px <- length(probability_breaks) - px
    ix <- length(impact_breaks) - ix
  }
  score <- px * ix
  data.frame(risk = risks$risk, probability_rating = px, impact_rating = ix,
             matrix_score = score,
             matrix_rank = rank(if (scale == "ascending") -score else score,
                                ties.method = "min"),
             expected_loss = tab$expected_inherent_loss,
             expected_loss_rank = rank(-tab$expected_inherent_loss, ties.method = "min"),
             stringsAsFactors = FALSE)
}

matrix_grid <- function(pb, ib, plab, ilab) {
  np <- length(pb) - 1L
  ni <- length(ib) - 1L
  if (np < 1L || ni < 1L || pb[1] != 0 || ib[1] != 0 || is.unsorted(pb, strictly = TRUE) ||
      is.unsorted(ib, strictly = TRUE) || pb[np + 1L] != 1 ||
      length(plab) != np || length(ilab) != ni) {
    stop("breaks must increase from 0 (probability up to 1) with one label per band.")
  }
  ib_hi <- ib[-1]
  if (is.infinite(ib_hi[ni])) {
    ib_hi[ni] <- ib[ni] * (ib[ni] / ib[ni - 1L])
  }
  i_mid <- (ib[-(ni + 1L)] + ib_hi) / 2
  fmt <- function(x) short_money(x, "", 0.1)
  i_range <- vapply(seq_len(ni), function(j) {
    if (j == 1L) paste0("up to ", fmt(ib[2]))
    else if (is.infinite(ib[j + 1L])) paste0("over ", fmt(ib[j]))
    else paste0(fmt(ib[j]), " to ", fmt(ib[j + 1L]))
  }, "")
  pf <- function(x) paste0(format(round(x * 100, 1), trim = TRUE, drop0trailing = TRUE), "%")
  p_range <- vapply(seq_len(np), function(j) {
    if (j == 1L) paste0("up to ", pf(pb[2]))
    else if (j == np) paste0("over ", pf(pb[j]))
    else paste0(pf(pb[j]), " to ", pf(pb[j + 1L]))
  }, "")
  list(np = np, ni = ni, p_mid = (pb[-(np + 1L)] + pb[-1]) / 2, i_mid = i_mid,
       p_axis = paste0(plab, "\n", p_range), i_axis = paste0(ilab, "\n", i_range))
}

short_money <- function(x, prefix = "", accuracy = 0.1) {
  out <- scales::label_number(scale_cut = scales::cut_short_scale(), prefix = prefix,
                              accuracy = accuracy)(x)
  gsub("(\\.[0-9]*?)0+([KMBT]?)$", "\\1\\2", gsub("\\.0+([KMBT]?)$", "\\1", out))
}
