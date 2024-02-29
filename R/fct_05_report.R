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
