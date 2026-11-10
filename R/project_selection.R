#' @title Project selection outcomes
#' @description Invented conditional outcome distributions for a choice between
#'   two projects.
#' @format A data frame with four rows and four columns:
#' \describe{
#'   \item{alternative}{Project name.}
#'   \item{uncertain_variable}{Demand scenario.}
#'   \item{outcome}{Net benefit in arbitrary monetary units.}
#'   \item{probability}{Scenario probability conditional on the project.}
#' }
#' @examples
#' data(project_selection)
#' calculate_expected_value(project_selection)
"project_selection"
