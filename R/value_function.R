#' @title Calculate additive multi-attribute value
#' @description Combine pre-scaled attribute values with weights using an
#'   additive value model. This model assumes appropriate preferential
#'   independence and weights representing trade-offs across the full
#'   attribute scales; it does not elicit or validate these assumptions.
#' @param data A data frame containing `alternative` and numeric attribute
#'   columns scaled from 0 (least preferred) to 1 (most preferred).
#' @param weights A named numeric vector of non-negative attribute weights
#'   summing to one. Names must match attribute columns in `data`.
#' @return A data frame with `alternative` and `value`.
#' @examples
#' data <- data.frame(alternative = c("A", "B"),
#'                    cost = c(0.8, 0.4), impact = c(0.3, 0.9))
#' value_function(data, c(cost = 0.6, impact = 0.4))
#' @export
value_function <- function(data, weights) {
  if (!is.data.frame(data) || !nrow(data) || anyDuplicated(names(data)) ||
      !"alternative" %in% names(data) ||
      !(is.character(data$alternative) || is.factor(data$alternative)) ||
      anyNA(data$alternative) || any(!nzchar(trimws(as.character(data$alternative)))) ||
      anyDuplicated(data$alternative)) {
    stop("data must have one row per named alternative and unique column names.")
  }
  if (!is.numeric(weights) || !length(weights) || is.null(names(weights)) ||
      anyNA(names(weights)) || any(!nzchar(names(weights))) ||
      anyDuplicated(names(weights)) || "alternative" %in% names(weights) ||
      !all(names(weights) %in% names(data)) || any(!is.finite(weights)) ||
      any(weights < 0) || abs(sum(weights) - 1) > 1e-8) {
    stop("weights must be named, non-negative, sum to 1, and match attribute columns.")
  }
  attributes <- data[names(weights)]
  if (!all(vapply(attributes, function(x) {
    is.numeric(x) && all(is.finite(x)) && all(x >= 0 & x <= 1)
  }, logical(1)))) {
    stop("attribute values must be finite numbers between 0 and 1.")
  }
  data.frame(alternative = as.character(data$alternative),
             value = as.vector(as.matrix(attributes) %*% weights))
}
