#' Linearity table
#'
#' @description A DT for linearity results
#'
#' @param df a dataframe with three columns to be included in the DT.
#' @param mydigits the significant digits for scale readings.
#' @return a DT table.
#' @importFrom DT datatable formatRound
#' @noRd
DTlinearity <- function(df, mydigits) {
  stopifnot(
    is.data.frame(df),
    is.numeric(mydigits),
    dim(df)[2] == 9
  )

  DT::datatable(df,
                style = "bootstrap",
                selection = "none",
                colnames = c("Prova",
                             "Valore nominale (g)",
                             "Valore convenzionale (g)",
                             "Lettura (g)",
                             "Errore (g)",
                             "Prova",
                             "Lettura (g)",
                             "Errore (g)",
                             "Errore medio (g)"
                ),
                rownames = FALSE,
                fillContainer = TRUE,
                editable = list(target = "column",
                                numeric = 0:8,
                                disable = list(columns = c(0, 4, 5, 7, 8))
                )
  ) |> DT::formatRound(9, digits = mydigits + 1) |>
    DT::formatRound(c(4, 5, 7, 8), digits = mydigits)
}


