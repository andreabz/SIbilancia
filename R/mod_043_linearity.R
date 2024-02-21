#' 033_linearity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_043_linearity_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' 033_linearity Server Functions
#'
#' @noRd
mod_043_linearity_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_033_linearity_ui("033_linearity_1")

## To be copied in the server
# mod_033_linearity_server("033_linearity_1")
