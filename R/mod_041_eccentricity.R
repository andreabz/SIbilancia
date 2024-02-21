##' 03_method UI Function
#'
#' @description A shiny Module for the eccentricity tests in
#' the {SIbilancia} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} list.
#' @return a list text and numeric inputs, along with an action button
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib card card_header card_body navset_card_tab nav_panel layout_column_wrap
mod_041_eccentricity_ui <- function(id){
  ns <- NS(id)
  tagList(




  )
}

#' 031_eccentricity Server Functions
#'
#' @noRd
mod_041_eccentricity_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_031_eccentricity_ui("031_eccentricity_1")

## To be copied in the server
# mod_031_eccentricity_server("031_eccentricity_1")
