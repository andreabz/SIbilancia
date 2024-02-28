#' Uncertainty table
#'
#' @description A DT for scale uncertainty results
#'
#' @param df a dataframe with three columns to be included in the DT.
#' @param mydigits the significant digits for scale readings.
#' @return a DT table.
#' @importFrom DT datatable formatRound
#' @noRd
DTuncertainty <- function(df, mydigits) {
stopifnot(
  is.data.frame(df),
  is.numeric(mydigits),
  dim(df)[2] == 3
)

DT::datatable(df,
              style = "bootstrap",
              selection = "none",
              colnames = c("Valore nominale (g)", "Correzioni (g)", "Incertezza estesa di taratura (g)"),
              rownames = FALSE,
              fillContainer = FALSE
              ) |>
  DT::formatRound(1:2, digits = mydigits) |>
  DT::formatRound(3, digits = mydigits + 1)
}

#' Usage scale extended uncertainty
#'
#' @description the function computes the usage scale uncertainty from a vector
#' of adjustments and scale calibration uncertainties, and the number of digits
#' of the scale.
#'
#' @param adjustments a numeric vector of adjustments.
#' @param uncertainties a numeric vector of calibration uncertainties.
#' @param mydigits the significant digits for scale readings.
#' @return a numeric value with the extended usage uncertainty of the scale.
#' @noRd
usageuncertainty <- function(adjustments,
                             uncertainties,
                             mydigits) {
  stopifnot(
    is.numeric(adjustments),
    is.numeric(uncertainties),
    is.numeric(mydigits)
  )

  # reading format
  myformat <- paste0("0.", paste0(rep("0", mydigits-1), collapse = ""), "1") |>
    as.numeric()

  # uncertainty contributions
  unc_adj <- max(adjustments) / sqrt(3) # non ci vorrà il valore assoluto?
  unc_cal <- max(uncertainties) / 2
  unc_read <-  2 * myformat / sqrt(3)   # perché il due?

  unc_usage <- 2 * sqrt(unc_adj^2 + unc_cal^2 + unc_read^2)

  unc_usage |>
    mysigformat(mydigits + 1) |>
    as.numeric()
}

#' testing if the usage extended uncertainty is suitable
#'
#' @description the function compares the extended uncertainty with four times
#' the given standard deviation for the scale and returns a textual result.
#'
#' @param givensd given scale repeatability standard deviation.
#' @param uncusage extended usage uncertainty of the scale.
#' @return a list with HTML and Markdown test results.
#' @importFrom glue glue
#' @noRd
usageuncertainty_result <- function(gsd,
                                    uncusage) {

  stopifnot(
    is.numeric(gsd),
    is.numeric(uncusage)
  )

  myres <- ifelse(uncusage > 4 * givensd,
                  "non conforme, l'incertezza estesa d'uso è maggiore di quattro volte lo scarto tipo di ripetibilità.",
                  "conforme,  l'incertezza estesa d'uso è minore di quattro volte lo scarto tipo di ripetibilità.")
  mycol <- ifelse(uncusage > 4 * givensd, "text-warning", "text-success")

  myhtmlresult <- glue::glue(
  "<ul>
    <li class = {mycol}> esito: {myres} </li>
    <li> incertezza estesa d'uso al livello di fiducia del 95% (k = 2) = {uncusage} g </li>
  </ul>")

  mymrkdresult <- glue::glue(
    "* esito: {myres}
     * incertezza estesa d'uso al livello di fiducia del 95% (k = 2) = {uncusage} g")

  list(htmlresult = myhtmlresult,
       mrkdresult = mymrkdresult)

}

#' results card
#'
#' @description a card with the results of scale calibration.
#'
#' @param header header of the card.
#' @param table DT with the results of the calibration.
#' @param htmlresult textual result of the calibration.
#' @param minuse minimum of the usage range.
#' @param maxuse maximum of the usage range.
#' @param mincal minimum of the calibration range.
#' @param maxcal maximum of the calibration range.
#' @param givensd given scale repeatability standard deviation.
#' @param uncusage extended usage uncertainty of the scale.
#' @return a bslib card
#' @importFrom bslib card card_body card_header
#' @importFrom glue glue
#' @noRd
result_card <- function(header,
                        table,
                        htmlresult,
                        minuse,
                        maxuse,
                        mincal,
                        maxcal,
                        givensd,
                        uncusage){

  stopifnot(
    is.character(header),
    is.character(htmlresult),
    is.numeric(minuse),
    is.numeric(maxuse),
    is.numeric(mincal),
    is.numeric(maxcal),
    is.numeric(givensd),
    is.numeric(uncusage),
  )

  myuserange <- glue::glue("campo di utilizzo: {minuse}-{maxuse} g")
  mycalrange <- glue::glue("intervallo di calibrazione: {mincal}-{maxcal} g")
  mygivensd <- glue::glue("scarto tipo di ripetibilità: {givensd} g")

  bslib::card(
    bslib::card_header(header),
    bslib::card_body(

      glue::glue(
        "<ul>
          <li> {myuserange} </li>
          <li> {mycalrange} </li>
          <li> {mygivensd} </li>
        </ul>"
      ),

    table,
    htmlresult

    )
  )

}
