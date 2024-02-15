##' 01_device UI Function
#'
#' @description A shiny Module for describing the scale in
#'  the {SIbilancia} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} list.
#' @return a list text and numeric inputs, along with an action button
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib card card_header card_footer card_body layout_column_wrap
#' @importFrom htmltools css
mod_01_device_ui <- function(id) {
  ns <- NS(id)
  tagList(

    bslib::layout_column_wrap(
      width = NULL,
      style = htmltools::css(grid_template_columns = "2fr 2fr 1fr"),

      #### instrument details ----
      bslib::card(
        bslib::card_header(
          shiny::icon("scale-balanced"),
          "Caratteristiche della bilancia"
        ),
        bslib::card_body(
          bslib::layout_column_wrap(
            width = 1 / 2,

            textInput(ns("inst"), "Strumento:"),
            textInput(ns("place"), "Collocazione:"),
            textInput(ns("manu"), "Produttore:"),
            textInput(ns("model"), "Modello:"),
            textInput(ns("idn"), "Matricola:"),
            radioButtons(
              ns("type"),
              "Tipo di bilancia:",
              choices = c("tecnica" = "tech",
                          "analitica" = "anal"),
              inline = TRUE
            ),
            numericInput(
              ns("nfor"),
              "Unità di formato (g):",
              0.0001,
              min = 0,
              max = 0.01
            )
          )

        )

      ),

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

    bslib::accordion(
      id = "help",
      open = "todo",
      bslib::accordion_panel(icon = shiny::icon("circle-info"),
                             title = "Cosa ti serve",
                             value = "todo",
                             ""),
      bslib::accordion_panel(icon = shiny::icon("lightbulb"),
                             title = "Suggerimento",
                             value = "tips",
                             ""),
      bslib::accordion_panel(icon = shiny::icon("vials"),
                             title = "Cosa otterrai",
                             tips = "toget",
                             "")
    )

    ),

    #### sample details
    bslib::card(
      bslib::card_header(shiny::icon("weight-hanging"), "Campioni"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(6, 6),

          textInput(ns("refsampleid"), "Sigla dei campioni:"),
          textInput(ns("refcertid"), "Certificato di taratura:"),
          textInput(ns("reflatid"), "Identificativo dei centri LAT:"),
          dateInput(ns("certval"), "Data di scadenza dei certificati:", language = "it")
        )
      )

    )
  )
}

#' 01_device Server Functions
#'
#' @noRd
mod_01_device_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_01_device_ui("01_device_1")

## To be copied in the server
# mod_01_device_server("01_device_1")
