#' @title Import risk outcomes from CSV
#' @description Read and validate a CSV file containing conditional outcome
#'   distributions. See `data-raw/risk_template.csv` in the source repository
#'   or the installed project selection example for the expected format.
#' @param path Path to a CSV file with `alternative`, `outcome`, and
#'   `probability` columns.
#' @return A validated data frame; extra columns are retained.
#' @examples
#' path <- system.file("extdata", "project_selection.csv", package = "risk.decision")
#' import_risk_data(path)
#' @export
import_risk_data <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("path must be a single non-empty file path.")
  }
  data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  validate_risk_data(data)
  data
}
