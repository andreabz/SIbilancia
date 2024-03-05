#' Repeatability table
#'
#' @description A DT for repeatability results
#'
#' @param df a dataframe with three columns to be included in the DT.
#' @param mydigits the significant digits for scale readings.
#' @return a DT table.
#' @importFrom DT datatable
#' @noRd
DTrepeatability <- function(df,
                            mydigits) {
  stopifnot(
    is.data.frame(df),
    is.numeric(mydigits),
    dim(df)[2] == 2
  )

  DT::datatable(df,
               style = "bootstrap",
               selection = "none",
               colnames = c("Ripetizione", "Lettura (g)"),
               rownames = FALSE,
               fillContainer = FALSE,
               editable = list(target = "column",
                               numeric = 1,
                               disable = list(columns = 0))
  ) |>
    DT::formatRound(2, digits = mydigits)
}

#' Results for repeatability test
#'
#' @description returns the results for the repeatability test.
#'
#' @details
#' The repeatability test is passed when the standard deviation of the measures
#' is the given standard deviation of the scale.
#'
#' @param measures a numeric vector of replicated measures obtained from repeatability test.
#' @param mydigits number of significant digits.
#' @param massload the mass loaded on the scale for the test.
#' @param minuse the minimum of the usage range to be tested.
#' @param maxuse the maximum of the usage range to be tested.
#' @param mincal the minimum of the calibration range.
#' @param maxcal the maximum of the calibration range.
#' @param givensd the given standard deviation of the scale.
#' @return a list with the uncertainty contribution (repeatability_uncertainty),
#' a HTML formatted result (repeatability_html), a Markdown
#' formatted result (repeatability_md) and a flag (repeatability_flag). The flag
#' is TRUE for a passed test and FALSE for a failed test.
#' @importFrom glue glue
#' @noRd
repeatability_result <- function(measures,
                                 mydigits,
                                 massload,
                                 minuse = 0,
                                 maxuse,
                                 mincal = 0,
                                 maxcal = maxuse,
                                 givensd) {

  req(measures |> is.numeric())
  req(massload |> is.numeric())
  req(maxcal |> is.numeric())
  req(mincal |> is.numeric())
  req(maxuse |> is.numeric())
  req(minuse |> is.numeric())
  req(measures[!is.na(measures)] |> length())

  replength <- measures[!is.na(measures)] |> length()
  myunc <- sd(measures) |>
    mysigformat(mydigits + 1) |>
    as.numeric()
  myflag <- ifelse(myunc > givensd, FALSE, TRUE)
  mycol <- ifelse(isFALSE(myflag), "text-danger", "text-success")
  myres <- ifelse(isFALSE(myflag),
                  "non conforme, lo scarto tipo delle prove non è inferiore allo scarto tipo di ripetibilità.",
                  "conforme, lo scarto tipo delle prove è inferiore allo scarto tipo di ripetibilità.")
  mytext <- if(replength < 10) {
    "Servono 10 misure." |> mywarning()
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
              <li> contributo all'incertezza u(e) = {myunc} g </li>
          </ul>")
  }
  myplaintext <- if(replength < 5) {
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
       * contributo all'incertezza u(e) = {myunc} g"
    )
  }

  list(repeatability_uncertainty = myunc,
       repeatability_html = mytext,
       repeatability_md = myplaintext,
       repeatability_flag = myflag
  )
}
