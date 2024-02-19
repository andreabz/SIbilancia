##' 03_method UI Function
#'
#' @description A shiny Module for the measurement tests in
#' the {SIbilancia} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} list.
#' @return a list text and numeric inputs, along with an action button
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib card card_header card_body navset_tab nav_panel
mod_03_measure_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::navset_tab(
      bslib::nav_panel(title = "Condizioni preliminari",
                       checkboxGroupInput(ns("check"), "Controlli preliminari",
                                          choices = c("Planarità" = "plan",
                                                      "Pulizia piatto" = "clean",
                                                      "Assenza instabilità meccaniche" = "mech"),
                                          inline = TRUE)
                       )
    )



  )
}

#' 03_measure Server Functions
#'
#' @noRd
mod_03_measure_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_03_measure_ui("03_measure_1")

## To be copied in the server
# mod_03_measure_server("03_measure_1")
