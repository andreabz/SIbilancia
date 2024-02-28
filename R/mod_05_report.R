##' 05_method UI Function
#'
#' @description A shiny Module for the report creating in
#' the {SIbilancia} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} list.
#' @return a list text and numeric inputs, along with an action button
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib card card_header card_body layout_columns
#' @importFrom glue glue
mod_05_report_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::layout_columns(
      col_widths = c(7, 5),
      fill = FALSE,

      bslib::card(
        bslib::card_header(shiny::icon("square-poll-vertical"), "Risultati"),
        bslib::card_body(

        )
      ),

      bslib::card(
        bslib::card_header(shiny::icon("pencil"), "Dati del certificato"),

        bslib::card_body(
          textInput(ns("ncert"), "Numero del certificato"),
          dateInput(ns("datecert"), "Data di emissione del certificato",
                    language = "it"),
          textInput(ns("destcert"), "Destinatario del certificato"),
          textInput(ns("operatorcert"), "Operatore")

        )
      )

    ),

    tags$div(style = "padding-bottom: 15px",
             downloadButton(ns("makereport"), label = "Crea il certificato",
                            width = '25%')
    )

  )
}

#' 05_report Server Functions
#'
#' @noRd
mod_05_report_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(r$range1, {
      ranges <- names(r)[grepl("range", names(r))]


    })

  })
}

## To be copied in the UI
# mod_05_report_ui("05_report_1")

## To be copied in the server
# mod_05_report_server("05_report_1")
