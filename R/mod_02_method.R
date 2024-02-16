##' 02_method UI Function
#'
#' @description A shiny Module for describing the calibration method in
#'  the {SIbilancia} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} list.
#' @return a list text and numeric inputs, along with an action button
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib card card_header card_body layout_column_wrap
#' @importFrom htmltools css tags
mod_02_method_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::layout_column_wrap(
      width = NULL,
      style = htmltools::css(grid_template_columns = "2fr 2fr 1fr"),

      #### calibration details ----
      bslib::card(
        bslib::card_header(shiny::icon("book"), "Metodo"),
        bslib::card_body(

          textInput(
            ns("docref"),
            "Documento di riferimento",
            "Gestione bilance e pesiere (IOP-GEAP-01-AR)"
          ),
          textInput(ns("doced"), "Edizione:"),
          textInput(ns("docver"), "Versione:"),
          dateInput(ns("docdate"), "Data di emissione:", language = "it")
        )
      ),

      #### sample details ----
      bslib::card(
        bslib::card_header(shiny::icon("weight-hanging"), "Campioni"),
        bslib::card_body(

            textInput(ns("refsampleid"), "Sigla dei campioni:"),
            textInput(ns("refcertid"), "Certificato di taratura:"),
            textInput(ns("reflatid"), "Identificativo dei centri LAT:"),
            dateInput(ns("certval"), "Data di scadenza dei certificati:", language = "it")
        )

      ),

      bslib::accordion(
        id = "help",
        open = "todo",
        bslib::accordion_panel(
          icon = shiny::icon("circle-info"),
          title = "Cosa ti serve",
          value = "todo",
          ""
        ),
        bslib::accordion_panel(
          icon = shiny::icon("lightbulb"),
          title = "Suggerimento",
          value = "tips",
          ""
        ),
        bslib::accordion_panel(
          icon = shiny::icon("vials"),
          title = "Cosa otterrai",
          tips = "toget",
          ""
        )
      )
    ),

      tags$div(style = "padding-bottom: 15px",
               actionButton(ns("next"), "Avanti", width = '10%')
      )

  )
}

#' 02_method Server Functions
#'
#' @noRd
mod_02_method_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_02_method_ui("02_method_1")

## To be copied in the server
# mod_02_method_server("02_method_1")
