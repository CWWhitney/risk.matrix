#' @title Net benefit matrix for value-of-information analysis
#' @description Expand an outcome table into a matrix with one column per
#'   alternative and one row per equally likely draw of the uncertain state,
#'   the input format of [evpi()] and the other functions of the voi package
#'   (Jackson and Heath 2024). Probabilities are matched to the nearest
#'   `1 / 10^6`.
#' @param data An outcome table (see [validate_risk_data()]) with an
#'   `uncertain_variable` column naming the state. Every alternative has the
#'   same states with the same probabilities.
#' @return A numeric matrix with alternatives as columns.
#' @examples
#' d <- data.frame(alternative = rep(c("Invest", "Hold"), each = 2),
#'                 uncertain_variable = rep(c("Success", "Failure"), 2),
#'                 outcome = c(40, -5, 0, 0), probability = rep(c(0.6, 0.4), 2))
#' evpi(net_benefit_matrix(d))
#' @export
net_benefit_matrix <- function(data) {
  validate_risk_data(data)
  if (!"uncertain_variable" %in% names(data) || anyNA(data$uncertain_variable)) {
    stop("data needs an uncertain_variable column naming each state.")
  }
  alts <- unique(as.character(data$alternative))
  if (length(alts) < 2L) {
    stop("at least two alternatives are needed.")
  }
  states <- unique(as.character(data$uncertain_variable))
  by_alt <- lapply(alts, function(a) {
    d <- data[data$alternative == a, ]
    if (anyDuplicated(d$uncertain_variable) || !setequal(d$uncertain_variable, states)) {
      stop("every alternative must have the same, distinct states.")
    }
    d[match(states, d$uncertain_variable), ]
  })
  p <- by_alt[[1]]$probability
  if (any(vapply(by_alt, function(d) any(abs(d$probability - p) > 1e-8), logical(1)))) {
    stop("state probabilities must not depend on the alternative.")
  }
  scales <- 10^(0:6)
  exact <- vapply(scales, function(k) all(abs(p * k - round(p * k)) < 1e-8), logical(1))
  scale <- if (any(exact)) scales[which(exact)[1]] else 1e6
  counts <- round(p * scale)
  out <- vapply(by_alt, function(d) rep(d$outcome, counts), numeric(sum(counts)))
  colnames(out) <- alts
  out
}
