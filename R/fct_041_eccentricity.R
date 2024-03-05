#' Eccentricity table
#'
#' @description A DT for eccentricity results
#'
#' @param df a dataframe with three columns to be included in the DT.
#' @param mydigits the significant digits for scale readings.
#' @return a DT table.
#' @importFrom DT datatable formatRound
#' @noRd
DTeccentricity <- function(df, mydigits) {
  stopifnot(
    is.data.frame(df),
    is.numeric(mydigits),
    dim(df)[2] == 3
    )

  DT::datatable(df,
                style = "bootstrap",
                selection = "none",
                colnames = c("Posizione", "Lettura (g)", "Differenza rispetto alla posizione 1 (g)"),
                rownames = FALSE,
                fillContainer = FALSE,
                editable = list(
                  target = "column",
                  numeric = c(1, 2),
                  disable = list(columns = c(0, 2))
                )
  ) |>
    DT::formatRound(c(2, 3), digits = mydigits)
}

#' Absolute differences for eccentricity results
#'
#' @description absolute differences of weights are computed vs the central,
#' first, measurement.
#'
#' @param readings a numeric vector of weight readings.
#' @param mydigits the significant digits for scale readings.
#' @return a vector of absolute differences.
#' @noRd
eccdiff <- function(readings, mydigits) {
  stopifnot(
    is.numeric(readings),
    is.numeric(mydigits)
  )

  myreadings <- readings |> as.numeric()
  abs(myreadings - myreadings[1]) |> round(mydigits)
}

#' Results for eccentricity test
#'
#' @description returns the results for the eccentricity test.
#'
#' @details
#' The eccentricity test is passed when the standard deviation inferred from
#' the maximum measured difference, considering a rectangular constant distribution,
#' is below three times the given standard deviation of the scale.
#'
#' @param differences a numeric vector of weight differences obtained from eccentricity test.
#' @param mydigits the significant digits for scale readings.
#' @param massload the mass loaded on the scale for the test.
#' @param minuse the minimum of the usage range to be tested.
#' @param maxuse the maximum of the usage range field to be tested.
#' @param mincal the minimum of the calibration range.
#' @param maxcal the maximum of the calibration range.
#' @param givensd the given standard deviation of the scale.
#' @return a list with the standard deviation inferred from the maximum
#' difference (eccentricity_maxdiff), the uncertainty contribution (eccentricity_uncertainty),
#' a HTML formatted result (eccentricity_html), a Markdown
#' formatted result (eccentricity_md) and a flag (eccentricity_flag). The flag
#' is TRUE for a passed test and FALSE for a failed test.
#' @importFrom glue glue
#' @noRd
eccentricity_result <- function(differences,
                               mydigits = 4,
                               massload,
                               minuse = 0,
                               maxuse,
                               mincal = 0,
                               maxcal = maxuse,
                               givensd) {

  req(differences |> is.numeric())
  req(mydigits |> is.numeric())
  req(massload |> is.numeric())
  req(maxcal |> is.numeric())
  req(mincal |> is.numeric())
  req(maxuse |> is.numeric())
  req(minuse |> is.numeric())
  req(differences[!is.na(differences)] |> length())

  ecclength <- differences[!is.na(differences)] |> length()
  maxdiff <- max(differences)
  myunc <- (maxdiff/(2 * sqrt(3))) |>
    mysigformat(mydigits + 1) |>
    as.numeric()
  myflag <- ifelse(maxdiff > 3 * givensd, FALSE, TRUE)
  mycol <- ifelse(isFALSE(myflag), "text-warning", "text-success")
  myres <- ifelse(isFALSE(myflag),
                  "non conforme, la differenza massima non è inferiore a tre volte lo scarto tipo di ripetibilità.",
                  "conforme, la differenza massima è inferiore a tre volte lo scarto tipo di ripetibilità.")
  mytext <- if(ecclength < 5) {
    "Servono 5 misure." |> mywarning()
  } else if (massload > maxcal) {
    "La massa impiegata non può eccedere il massimo dell'intervallo di taratura." |> mywarning()
  } else if (massload < mincal) {
    "La massa impiegata non può essere inferiore al minimo dell'intervallo di taratura." |> mywarning()
  } else if (maxcal > maxuse) {
    "Il massimo dell'intervallo di taratura non può eccedere il massimo del campo di utilizzo." |> mywarning()
  } else if (mincal < minuse) {
    "Il minimo dell'intervallo di taratura non può essere inferiore al minimo del campo di utilizzo." |> mywarning()
  } else {
    glue::glue(
      "<ul>
              <li class = {mycol}> esito: {myres} </li>
              <li> differenza massima = {maxdiff} g </li>
              <li> contributo all'incertezza u(e) = {myunc} g </li>
          </ul>")
  }
  myplaintext <- if(ecclength < 5) {
    "Servono 5 misure."
  } else if (massload > maxcal) {
    "La massa impiegata non può eccedere il massimo dell'intervallo di taratura."
  } else if (massload < mincal) {
    "La massa impiegata non può essere inferiore al minimo dell'intervallo di taratura."
  } else if (maxcal > maxuse) {
    "Il massimo dell'intervallo di taratura non può eccedere il massimo del campo di utilizzo."
  } else if (mincal < minuse) {
    "Il minimo dell'intervallo di taratura non può essere inferiore al minimo del campo di utilizzo."
  } else {
    glue::glue(
      "* esito: {myres}
                * differenza massima = {maxdiff} g
                * contributo all'incertezza u(e) = {myunc} g"
    )
  }

  list(eccentricity_maxdiff = maxdiff,
       eccentricity_uncertainty = myunc,
       eccentricity_html = mytext,
       eccentricity_md = myplaintext,
       eccentricity_flag = myflag
  )
}
