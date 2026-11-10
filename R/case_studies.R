#' @title Case studies from published decision analyses
#' @description Outcome tables transcribed from open-access, peer-reviewed
#'   decision analyses whose DOIs were checked in Crossref. They are examples
#'   of the package's input format. Outcomes are the published utilities,
#'   ratings or values, and probabilities are as published.
#' @param id A case study identifier. Omit to list all case studies.
#' @return With no `id`, a data frame describing each case study (DOI,
#'   citation, outcome scale, source tables and verification date). With an
#'   `id`, the outcome table or, for a risk register case, the register.
#' @examples
#' case_study()
#' syndesmosis <- case_study("schulte_2021_syndesmosis")
#' calculate_expected_value(syndesmosis)
#' @export
case_study <- function(id = NULL) {
  dir <- system.file("extdata", "case_studies", package = "risk.decision")
  info <- utils::read.csv(file.path(dir, "case_studies.csv"), stringsAsFactors = FALSE)
  if (is.null(id)) {
    return(info)
  }
  if (!is.character(id) || length(id) != 1L || !id %in% info$id) {
    stop("unknown case study; see case_study() for the available ids.")
  }
  file <- file.path(dir, paste0(id, ".csv"))
  if (identical(info$type[info$id == id], "risk register")) {
    import_risk_register(file)
  } else {
    import_risk_data(file)
  }
}
