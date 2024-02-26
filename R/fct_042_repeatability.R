#' Repeatability table
#'
#' @description A DT for repeatability results
#'
#' @param df a dataframe with three columns to be included in the DT.
#' @return a DT table.
#' @importFrom DT datatable
#' @noRd
DTrepeatability <- function(df) {
  stopifnot(
    is.data.frame(df),
    dim(df)[2] == 2
  )

  DT::datatable(df,
               style = "bootstrap",
               selection = "none",
               colnames = c("Ripetizione", "Lettura (g)"),
               rownames = FALSE,
               fillContainer = TRUE,
               editable = list(target = "column",
                               numeric = 1,
                               disable = list(columns = 0))
  )
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
#' @param minfield the minimum of the measurement field to be tested.
#' @param maxfield the maximum of the measurement field to be tested.
#' @param mincal the minimum of the calibration range.
#' @param maxcal the maximum of the calibration range.
#' @param givensd the given standard deviation of the scale.
#' @return a list with the standard deviation of the measures (meassd),
#' the uncertainty contribution (uncrep), an HTML formatted result (result) and a Markdown
#' formatted result (plairesult).
#' @importFrom glue glue
#' @noRd
repeatabilityresult <- function(measures,
                                mydigits,
                                massload,
                                minfield = 0,
                                maxfield,
                                mincal = 0,
                                maxcal = maxfield,
                                givensd) {

  req(measures |> is.numeric())
  req(massload |> is.numeric())
  req(maxcal |> is.numeric())
  req(mincal |> is.numeric())
  req(maxfield |> is.numeric())
  req(minfield |> is.numeric())
  req(measures[!is.na(measures)] |> length())

  replength <- measures[!is.na(measures)] |> length()
  myunc <- sd(measures) |>
    mysigformat(mydigits + 1) |>
    as.numeric()
  mycol <- ifelse(myunc > givensd, "text-warning", "text-success")
  myres <- ifelse(myunc > givensd, "non conforme", "conforme")
  mytext <- if(replength < 5) {
    "Servono 5 misure." |> mywarning()
  } else if (massload > maxcal) {
    "La massa impiegata non può eccedere il massimo dell'intervallo di taratura." |> mywarning()
  } else if (massload < mincal) {
    "La massa impiegata non può essere inferiore al minimo dell'intervallo di taratura." |> mywarning()
  } else if (maxcal > maxfield) {
    "Il massimo dell'intervallo di taratura non può eccedere il massimo del campo di utilizzo." |> mywarning()
  } else if (mincal < minfield) {
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
  } else if (maxcal > maxfield) {
    "Il massimo dell'intervallo di taratura non può eccedere il massimo del campo di utilizzo."
  } else if (mincal < minfield) {
    "Il minimo dell'intervallo di taratura non può essere inferiore al minimo del campo di utilizzo."
  } else {
    glue::glue(
      "* esito: {myres}
       * contributo all'incertezza u(e) = {myunc} g"
    )
  }

  list(uncrep = myunc,
       result = mytext,
       plainresult = myplaintext
  )
}
