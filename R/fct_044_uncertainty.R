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

#' calculating the extended calibration uncertainty of the scale
#'
#' @description extended calibration uncertainty is calculated considering
#' the contribution from format of the result ($u_read$), the contribution from
#' the maximum permissible error of the weights for e2 class ($u_mpe$),
#' the contribution from the uncertainty of the calibrated weights ($u_cert$), and
#' contribution estimated from the eccentricity ($U_ecc$), repeatability ($u_rep$)
#' and loading ($u_load$) $tests.
#'
#' @param mass a numeric vector with the masses for which the uncertainty is to be calculated.
#' @param readformat a number with the format of the reading on the scale.
#' @param ucert a numeric vector with the uncertainty of the calibrated reference weights.
#' @param uecc a number with the uncertainty contribution from eccentricity test.
#' @param eccload a number with the tested mass used during the eccentricity test.
#' @param urep a number with the uncertainty contribution from the repeatability test.
#' @param avglin a numeric vector with the average error from a linearity test.
#' @return a numeric vector with the extended calibration uncertainty.
#' @noRd
caluncertainty <- function(mass,
                           readformat,
                           ucert,
                           uecc,
                           eccload,
                           urep,
                           avglin) {

  stopifnot(
    is.numeric(mass),
    is.numeric(readformat),
    is.numeric(ucert),
    is.numeric(uecc),
    is.numeric(eccload),
    is.numeric(urep),
    is.numeric(avglin)
  )

  u_dmM <- 0.00002
  u_mpe <- oiml_r111[which(c(oiml_r111$nominal_g %in% mass)), "e2"] / (4 * sqrt(3)) / 1000 # table value in mg
  u_mc <- 0 # add the contribution from the certified reference masses
  u_md <- 2 * 1.7 * u_mc / sqrt(3)
  u_ecc <- uecc / (4 * sqrt(3)) * (mass / eccload)
  u_rep <- urep
  u_read <- readformat / (2 * sqrt(3))
  u_readL <- u_read
  u_lin <- avglin / (4 * sqrt(3))

  2 * sqrt(u_dmM^2 + u_mpe^2 + u_mc^2 + u_md^2 + u_ecc^2 + u_rep^2 + u_read^2 +
           u_readL^2 + u_lin^2)
}
