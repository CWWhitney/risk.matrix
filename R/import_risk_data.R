#' Import a CSV risk assessment
#'
#' Reads a CSV file with one mutually exclusive outcome per row. The required
#' columns are `alternative`, `uncertain_variable`, `outcome`, `probability`,
#' and `value`; probabilities must sum to one within each alternative.
#'
#' @param file Path to a CSV risk assessment file.
#'
#' @return A validated data frame.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
#' if (nzchar(path)) import_risk_data(path)
import_risk_data <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !file.exists(file)) {
    stop("`file` must name an existing CSV file.", call. = FALSE)
  }
  data <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  validate_risk_data(data)
}
