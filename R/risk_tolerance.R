#' @title Certain monetary equivalent under exponential utility
#' @description The certain amount a person with constant absolute
#'   risk aversion would accept in place of a gamble:
#'   `-R * log(sum(p * exp(-x / R)))`, where `R` is the risk tolerance in the
#'   same units as the outcomes. A larger `R` is closer to risk neutrality.
#' @param outcome Numeric outcomes (gains positive, losses negative).
#' @param probability Probabilities of the outcomes; they must sum to one.
#' @param risk_tolerance One positive number, in outcome units.
#' @return One number.
#' @examples
#' certain_monetary_equivalent(c(5, -3), c(0.8, 0.2), risk_tolerance = 2)
#' @export
certain_monetary_equivalent <- function(outcome, probability, risk_tolerance) {
  if (!is.numeric(outcome) || !is.numeric(probability) ||
      length(outcome) != length(probability) || !length(outcome) ||
      any(!is.finite(outcome)) || any(!is.finite(probability)) ||
      any(probability < 0 | probability > 1) || abs(sum(probability) - 1) > 1e-8) {
    stop("outcome and probability must match, and probabilities must sum to 1.")
  }
  if (!is.numeric(risk_tolerance) || length(risk_tolerance) != 1L ||
      !is.finite(risk_tolerance) || risk_tolerance <= 0) {
    stop("risk_tolerance must be one positive number.")
  }
  m <- max(-outcome / risk_tolerance)
  -risk_tolerance * (m + log(sum(probability * exp(-outcome / risk_tolerance - m))))
}

#' @title Fit a risk tolerance to just-barely-acceptable bets
#' @description Find the exponential-utility risk tolerance that makes a set
#'   of two-outcome bets, each judged just barely acceptable by the decision
#'   maker, have certain monetary equivalents as close to zero as possible
#'   (least squares).
#' @param bets A data frame with `gain`, `loss` (negative) and `p_gain`.
#' @param interval Search range for the risk tolerance, in outcome units.
#' @return A list with `risk_tolerance`, `fit` (the sum of squared certain
#'   monetary equivalents; zero is a perfect fit) and `bets` with each bet's
#'   certain monetary equivalent at the fitted value. A tolerance at the edge
#'   of `interval` means the bets are close to risk neutral or inconsistent.
#' @examples
#' bets <- data.frame(gain = c(5, 2), loss = c(-3, -1), p_gain = c(0.8, 0.6))
#' fit_risk_tolerance(bets)$risk_tolerance
#' @export
fit_risk_tolerance <- function(bets, interval = c(0.01, 1000)) {
  if (!is.data.frame(bets) || !nrow(bets) ||
      !all(c("gain", "loss", "p_gain") %in% names(bets)) ||
      !all(vapply(bets[c("gain", "loss", "p_gain")], function(x) {
        is.numeric(x) && all(is.finite(x))
      }, logical(1))) || any(bets$loss >= 0) || any(bets$gain <= 0) ||
      any(bets$p_gain < 0 | bets$p_gain > 1)) {
    stop("bets need finite gain > 0, loss < 0 and p_gain between 0 and 1.")
  }
  if (!is.numeric(interval) || length(interval) != 2L || any(interval <= 0) ||
      interval[1] >= interval[2]) {
    stop("interval must be two increasing positive numbers.")
  }
  cme <- function(r) {
    vapply(seq_len(nrow(bets)), function(i) {
      certain_monetary_equivalent(c(bets$gain[i], bets$loss[i]),
                                  c(bets$p_gain[i], 1 - bets$p_gain[i]), r)
    }, numeric(1))
  }
  best <- stats::optimize(function(log_r) sum(cme(exp(log_r))^2), log(interval))
  r <- exp(best$minimum)
  out <- bets
  out$certain_monetary_equivalent <- cme(r)
  list(risk_tolerance = r, fit = best$objective, bets = out)
}

#' @title Linear z-score model for ranking risks
#' @description Standardize each factor to a z-score, orient it by direction,
#'   weight it, and add (a Dawes-style improper linear model). Higher totals
#'   mean more of the thing the directions describe, such as risk. Standard
#'   deviations are the sample (n - 1) standard deviations.
#' @param data A data frame of numeric factors, one row per item.
#' @param direction Named vector of `+1` (more increases the score) or `-1`.
#' @param weights Optional named non-negative weights; default equal.
#' @return `data` with a `score` column added.
#' @examples
#' d <- data.frame(cost = c(12, 4, 25), duration = c(2, 1, 5))
#' dawes_score(d, c(cost = 1, duration = 1))
#' @export
dawes_score <- function(data, direction, weights = NULL) {
  if (!is.data.frame(data) || nrow(data) < 2L || is.null(names(direction)) ||
      !all(names(direction) %in% names(data)) || !all(direction %in% c(-1, 1))) {
    stop("data needs at least 2 rows and direction must be named +1/-1 for its columns.")
  }
  f <- names(direction)
  if (is.null(weights)) {
    weights <- stats::setNames(rep(1, length(f)), f)
  }
  if (!is.numeric(weights) || !setequal(names(weights), f) || any(!is.finite(weights)) ||
      any(weights < 0)) {
    stop("weights must be non-negative and named like direction.")
  }
  if (!all(vapply(data[f], function(x) is.numeric(x) && all(is.finite(x)) && stats::sd(x) > 0,
                  logical(1)))) {
    stop("factors must be finite and vary across items.")
  }
  z <- vapply(f, function(n) {
    (data[[n]] - mean(data[[n]])) / stats::sd(data[[n]]) * direction[[n]] * weights[[n]]
  }, numeric(nrow(data)))
  data$score <- rowSums(matrix(z, nrow(data)))
  data
}
