#' Get the decimal places of a number
#'
#' @description The function returns the decimal places of a number.
#' @details
#' The function was copied from the nisetama answer at
#' https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
#'
#' @param x a number.
#' @return the decimal places of the input number, formatted as a numeric value.
#'
#' @noRd
decimalplaces <- function(x) {
  stopifnot(is.numeric(x),
            length(x) == 1)

  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    sapply(x, function(y){
      nchar(
        sub("^-?\\d*\\.?","",
            format(y, scientific = F)
        )
      )
    })
  } else {
    return(0)
  }
}

#' Format a number with the required decimal places
#'
#' @description The function format a number with the required the decimal places.
#'
#' @param x a number.
#' @param digits the number of required decimal places, formatted as a numeric value.
#' @return a character value.
#'
#' @noRd
mysigformat <- function(x, digits) {
  stopifnot(
    is.numeric(x),
    is.numeric(digits),
    digits > 0
  )

  round(x, digits) |>
    format(scientific = FALSE)

}

#' Make a warning text.
#'
#' @description Put some text in a HTML paragraph with warning colors.
#'
#' @param x a string.
#' @return a HTML p using the text-warning class.
#'
#' @importFrom glue glue
mywarning <- function(x) {
  glue::glue("<p class = text-warning> {x} </p>")
}

#' Add an image.
#'
#' @description adding a png image in a HTML block.
#'
#' @param filename the name of a png file in /inst/app/www/.
#' @return a HTML block with the required image.
#'
#' @importFrom shiny tags
add_www_img <- function(filename) {
  myfilename <- paste0("inst/app/www/", filename)

  stopifnot(
    is.character(filename),
    file.exists(myfilename),
    grepl("\\.png$", filename)
    )

  shiny::tags$img(src = paste0("www/", filename),
                  style = "height:90%;
                           width:auto;
                           display:block;
                           margin-left:auto;
                           margin-right:auto;")
}
