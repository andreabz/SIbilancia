#' 032_repeatability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_042_repeatability_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' 032_repeatability Server Functions
#'
#' @noRd
mod_042_repeatability_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_032_repeatability_ui("032_repeatability_1")

## To be copied in the server
# mod_032_repeatability_server("032_repeatability_1")
