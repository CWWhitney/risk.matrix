#' @title Expected loss from a risk register
#' @description Analytic expected inherent and residual loss for each risk in
#'   a one-for-one substitution model. Each risk has an annual probability of
#'   occurring and a lognormal impact fitted to a 90% confidence interval
#'   (lower and upper bounds). Controls reduce the probability of the event by
#'   `control_effectiveness`; the impact distribution stays the same.
#' @param risks A data frame with columns `risk`, `probability` (annual,
#'   0 to 1), and either `lower` and `upper` (positive 90% interval bounds for
#'   impact, given that the event occurs) or `impact` (one value). Optional columns `control_cost` (default 0)
#'   and `control_effectiveness` (0 to 1, default 0).
#' @return A data frame with `expected_inherent_loss`,
#'   `expected_residual_loss`, and `return_on_control`, defined as the
#'   reduction in expected loss divided by control cost, minus one. It is `NA`
#'   when the control cost is zero.
#' @examples
#' risks <- data.frame(risk = "Recession", probability = 0.4, lower = 5e5,
#'                     upper = 1e7, control_cost = 3e5,
#'                     control_effectiveness = 0.35)
#' expected_loss_table(risks)
#' @export
expected_loss_table <- function(risks) {
  risks <- check_risk_register(risks)
  params <- lognormal_from_interval(risks$lower, risks$upper)
  mean_impact <- exp(params$meanlog + params$sdlog^2 / 2)
  inherent <- risks$probability * mean_impact
  residual <- (1 - risks$control_effectiveness) * inherent
  roc <- ifelse(risks$control_cost > 0,
                (inherent - residual) / risks$control_cost - 1, NA_real_)
  data.frame(risk = risks$risk, expected_inherent_loss = inherent,
             expected_residual_loss = residual, return_on_control = roc,
             stringsAsFactors = FALSE)
}

#' @title Simulate total annual loss
#' @description Monte Carlo simulation of one year of losses from a risk
#'   register; see [expected_loss_table()] for the input format and
#'   assumptions. Risks are simulated as independent. In each trial the same
#'   random draw decides whether a risk occurs inherently and, with its
#'   reduced probability, after controls, so the two totals are comparable.
#' @param risks A risk register data frame.
#' @param n Number of trials, a positive whole number.
#' @param seed Optional integer seed for reproducibility.
#' @return A data frame with one row per trial and total `inherent` and
#'   `residual` annual loss.
#' @examples
#' risks <- data.frame(risk = c("A", "B"), probability = c(0.4, 0.15),
#'                     lower = c(5e5, 2e6), upper = c(1e7, 5e6),
#'                     control_effectiveness = c(0.35, 0.25))
#' head(simulate_annual_loss(risks, n = 1000, seed = 1))
#' @export
simulate_annual_loss <- function(risks, n = 10000, seed = NULL) {
  risks <- check_risk_register(risks)
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1 || n != round(n)) {
    stop("n must be a positive whole number.")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  params <- lognormal_from_interval(risks$lower, risks$upper)
  k <- nrow(risks)
  u_event <- matrix(stats::runif(n * k), n, k)
  u_impact <- matrix(stats::runif(n * k), n, k)
  impact <- sapply(seq_len(k), function(j) {
    stats::qlnorm(u_impact[, j], params$meanlog[j], params$sdlog[j])
  })
  impact <- matrix(impact, n, k)
  p_in <- matrix(risks$probability, n, k, byrow = TRUE)
  p_res <- p_in * (1 - matrix(risks$control_effectiveness, n, k, byrow = TRUE))
  data.frame(trial = seq_len(n),
             inherent = rowSums(impact * (u_event < p_in)),
             residual = rowSums(impact * (u_event < p_res)))
}

#' @title Loss exceedance curve
#' @description Estimate the probability that annual loss exceeds each of a
#'   set of loss levels, as an empirical proportion of simulated trials.
#' @param losses A numeric vector of simulated losses, or a data frame such as
#'   the output of [simulate_annual_loss()] (every numeric column except
#'   `trial` becomes a curve).
#' @param thresholds Loss levels at which to evaluate the curve. By default,
#'   100 values from zero to the 99.9th percentile of the losses.
#' @return A data frame with `loss`, `curve` and `exceedance_probability`.
#' @examples
#' loss_exceedance_curve(c(0, 0, 1e5, 4e5, 2e6), thresholds = c(0, 1e5, 1e6))
#' @export
loss_exceedance_curve <- function(losses, thresholds = NULL) {
  if (is.data.frame(losses)) {
    losses <- losses[setdiff(names(losses), "trial")]
  } else {
    losses <- data.frame(loss = losses)
  }
  if (!ncol(losses) || !all(vapply(losses, function(x) {
    is.numeric(x) && length(x) > 0 && all(is.finite(x)) && all(x >= 0)
  }, logical(1)))) {
    stop("losses must be finite, non-negative numbers.")
  }
  if (is.null(thresholds)) {
    thresholds <- seq(0, stats::quantile(unlist(losses), 0.999, names = FALSE),
                      length.out = 100)
  }
  if (!is.numeric(thresholds) || !length(thresholds) || any(!is.finite(thresholds))) {
    stop("thresholds must be finite numbers.")
  }
  do.call(rbind, lapply(names(losses), function(curve) {
    data.frame(loss = thresholds, curve = curve,
               exceedance_probability = vapply(thresholds, function(x) {
                 mean(losses[[curve]] > x)
               }, numeric(1)), stringsAsFactors = FALSE)
  }))
}

#' @title Plot loss exceedance curves
#' @description Draw one or more loss exceedance curves, optionally with a
#'   stated risk tolerance curve. Curves that stay below the tolerance curve
#'   are within tolerance at every loss level shown.
#' @param curves Output of [loss_exceedance_curve()].
#' @param tolerance Optional data frame with `loss` and
#'   `exceedance_probability`: the maximum acceptable exceedance probability
#'   at each loss level.
#' @param xlab,ylab Axis labels.
#' @return A ggplot object.
#' @examples
#' sim <- data.frame(inherent = c(0, 1e5, 4e5, 2e6), residual = c(0, 0, 1e5, 1e6))
#' plot_loss_exceedance(loss_exceedance_curve(sim))
#' @export
plot_loss_exceedance <- function(curves, tolerance = NULL, xlab = "Annual loss",
                                 ylab = "Probability of exceeding") {
  if (!is.data.frame(curves) ||
      !all(c("loss", "curve", "exceedance_probability") %in% names(curves))) {
    stop("curves must come from loss_exceedance_curve().")
  }
  levels_c <- unique(as.character(curves$curve))
  colors <- risk_palette(length(levels_c))
  lines_c <- rep("solid", length(levels_c))
  curves <- curves[c("loss", "curve", "exceedance_probability")]
  curves$curve <- as.character(curves$curve)
  if (!is.null(tolerance)) {
    if (!is.data.frame(tolerance) ||
        !all(c("loss", "exceedance_probability") %in% names(tolerance))) {
      stop("tolerance must have loss and exceedance_probability columns.")
    }
    tolerance <- data.frame(loss = tolerance$loss, curve = "Risk tolerance",
                            exceedance_probability = tolerance$exceedance_probability)
    curves <- rbind(curves, tolerance)
    levels_c <- c(levels_c, "Risk tolerance")
    colors <- c(colors, "#0b0b0b")
    lines_c <- c(lines_c, "dashed")
  }
  curves$curve <- factor(curves$curve, levels = levels_c)
  names(colors) <- names(lines_c) <- levels_c
  p <- ggplot2::ggplot(curves, ggplot2::aes(loss, exceedance_probability,
                                            color = curve, linetype = curve)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_linetype_manual(values = lines_c) +
    ggplot2::scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    ggplot2::scale_y_continuous(limits = c(0, NA), labels = scales::label_percent(),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = xlab, y = ylab) +
    theme_risk()
  p
}

lognormal_from_interval <- function(lower, upper) {
  z <- stats::qnorm(0.95)
  list(meanlog = (log(lower) + log(upper)) / 2,
       sdlog = (log(upper) - log(lower)) / (2 * z))
}

check_risk_register <- function(risks) {
  if (!is.data.frame(risks) || !nrow(risks) || !all(c("risk", "probability") %in% names(risks)) ||
      !(all(c("lower", "upper") %in% names(risks)) || "impact" %in% names(risks))) {
    stop("risks must be a non-empty data frame with risk, probability and either lower and upper or impact columns.")
  }
  if (!all(c("lower", "upper") %in% names(risks))) {
    risks$lower <- risks$upper <- risks$impact
  }
  if (is.null(risks$control_cost)) risks$control_cost <- 0
  if (is.null(risks$control_effectiveness)) risks$control_effectiveness <- 0
  num <- c("probability", "lower", "upper", "control_cost", "control_effectiveness")
  if (!all(vapply(risks[num], function(x) is.numeric(x) && all(is.finite(x)), logical(1)))) {
    stop("risk register values must be finite numbers.")
  }
  if (any(risks$probability < 0 | risks$probability > 1) ||
      any(risks$control_effectiveness < 0 | risks$control_effectiveness > 1)) {
    stop("probability and control_effectiveness must be between 0 and 1.")
  }
  if (any(risks$lower <= 0) || any(risks$upper < risks$lower)) {
    stop("impact bounds must satisfy 0 < lower <= upper.")
  }
  if (any(risks$control_cost < 0)) {
    stop("control_cost must not be negative.")
  }
  risks$risk <- as.character(risks$risk)
  risks
}
