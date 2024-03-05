#' results card
#'
#' @description a card with the results of scale calibration.
#'
#' @param mincal minimum of the calibration range.
#' @param maxcal maximum of the calibration range.
#' @param eccentricity_flag the flag for the eccentricity test. TRUE for passed
#' and FALSE for failed test.
#' @param repeatability_flag the flag for the repeatability test. TRUE for passed
#' and FALSE for failed test.
#' @param uncertainty_flag the flag for the uncertainty test. TRUE for passed
#' and FALSE for failed test.
#' @param ucertainty_usage extended usage uncertainty of the scale.
#' @return a html list with the calibration summary.
#' @importFrom glue glue
#' @noRd
calibration_summary <- function(mincal,
                                maxcal,
                                eccentricity_flag,
                                repeatability_flag,
                                uncertainty_flag,
                                uncertainty_usage){
  stopifnot(
    is.numeric(mincal),
    is.numeric(maxcal),
    is.logical(eccentricity_flag),
    is.logical(repeatability_flag),
    is.logical(uncertainty_flag),
    is.numeric(uncertainty_usage)
    )

  eccentricity_test <- tick_cross(eccentricity_flag)
  repeatability_test <- tick_cross(repeatability_flag)
  linearity_test <- tick_cross(uncertainty_flag)

  eccentricity_color <- success_danger(eccentricity_flag)
  repeatability_color <- success_danger(repeatability_flag)
  linearity_color <- success_danger(uncertainty_flag)

  myuncertainty <- ifelse(is.null(uncertainty_usage), "", paste0(uncertainty_usage, " g"))

  myinterval <- glue::glue("Intervallo di calibrazione: {mincal} - {maxcal} g </br>")
  mysummarylist <- glue::glue(
  "<ul>
    <li class = {eccentricity_color}> eccentricità: {eccentricity_test} </li>
    <li class = {repeatability_color}> ripetibilità: {repeatability_test} </li>
    <li class = {linearity_color}> linearità: {linearity_test} </li>
    <li class = {linearity_color}> incertezza d'uso: {myuncertainty} {linearity_test} </li>
  </ul>")

  paste0(myinterval, mysummarylist)

}
