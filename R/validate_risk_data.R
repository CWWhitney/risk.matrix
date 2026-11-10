validate_risk_data <- function(data, tolerance = 1e-8) {
  required <- c("alternative", "uncertain_variable", "outcome", "probability", "value")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(data)) {
    stop("Risk data must contain at least one row.", call. = FALSE)
  }
  if (anyNA(data[required])) {
    stop("Required risk data columns cannot contain missing values.", call. = FALSE)
  }
  text_columns <- c("alternative", "uncertain_variable", "outcome")
  if (any(vapply(data[text_columns], function(x) any(!nzchar(trimws(as.character(x)))), logical(1)))) {
    stop("Alternative, uncertain_variable, and outcome values cannot be blank.", call. = FALSE)
  }
  if (!is.numeric(data$probability) || !is.numeric(data$value)) {
    stop("The probability and value columns must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(data$probability)) || any(data$probability < 0 | data$probability > 1)) {
    stop("Probabilities must be finite values between 0 and 1.", call. = FALSE)
  }
  if (any(!is.finite(data$value))) {
    stop("Outcome values must be finite.", call. = FALSE)
  }

  probability_totals <- tapply(data$probability, as.character(data$alternative), sum)
  if (any(abs(probability_totals - 1) > tolerance)) {
    invalid <- names(probability_totals)[abs(probability_totals - 1) > tolerance]
    stop(
      "Outcome probabilities must sum to 1 for each alternative. Check: ",
      paste(invalid, collapse = ", "),
      call. = FALSE
    )
  }
  data
}
