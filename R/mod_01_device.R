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
#' @importFrom bslib card card_header card_body layout_column_wrap
#' @importFrom htmltools css tags
mod_01_device_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = NULL,
      style = htmltools::css(grid_template_columns = "2fr 1fr"),

      #### instrument details ----
      bslib::card(
        fill = FALSE,
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
            ),
            radioButtons(ns("nint"),
                         "Numero di intervalli di taratura:",
                         choices = c("1" = 1L,
                                     "2" = 2L),
                         inline = TRUE)
          )

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

      tags$div(style = "padding-bottom: 30px",
          actionButton(ns("nextbtn"), "Avanti", width = '10%')
      )

  )
}

#' 01_device Server Functions
#'
#' @noRd
mod_01_device_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mynamespace <- environment(ns)[['namespace']]
    r[[mynamespace]] <- shiny::reactiveValues()

    #### saving all the inputs ----
    observeEvent(input$nextbtn, {

      inputnames <- names(input)
      savename <- inputnames[inputnames %not_in% "nextbtn"]

      lapply(savename, function (x) {
        r[[mynamespace]][[x]] <- input[[x]]
      })

      #### adding the significant digits ----
      r[[mynamespace]][["signifdigits"]] <- decimalplaces(r[[mynamespace]][["nfor"]])

    })

  })
}

## To be copied in the UI
# mod_01_device_ui("01_device_1")

## To be copied in the server
# mod_01_device_server("01_device_1")
