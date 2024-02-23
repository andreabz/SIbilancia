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
#' @importFrom bslib card card_header card_body navset_card_tab nav_panel layout_column_wrap
#' @importFrom glue glue
mod_04_measure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      numericInput(
        ns("minfield"),
        "Minimo del campo di utilizzo (g):",
        value = 0,
        min = 0
      ),
      numericInput(
        ns("maxfield"),
        "Massimo del campo di utilizzo (g):",
        value = 0,
        min = 0
      ),
      numericInput(
        ns("mincal"),
        "Minimo dell'intervallo di taratura (g):",
        value = 0,
        min = 0
      ),
      numericInput(
        ns("maxcal"),
        "Massimo dell'intervallo di taratura (g):",
        value = 0,
        min = 0
      ),
      numericInput(
        ns("gsd"),
        "Scarto tipo di ripetibilità (g):",
        value = 0,
        min = 0
      )

    ),

    bslib::navset_card_tab(
      height = 600,

      ##### eccentricity ui ----
      bslib::nav_panel(
        fillable = FALSE,
        title = "Eccentricità",

        bslib::layout_columns(
          col_widths = c(6, 3, 3),

          bslib::card(
            bslib::card_body(
              numericInput(
                ns("eccload"),
                "Massa utilizzata (g):",
                value = 0,
                min = 0
              ),

              DT::DTOutput(ns("eccentricity")),
              htmlOutput(ns("eccresult"))
            )
          ),

          bslib::card(bslib::card_body(add_www_img("scale_scheme.png"))),

          bslib::card(
            bslib::card_header(shiny::icon("circle-info"), "Cosa devi fare"),
            bslib::card_body(
              list(
                one = "Compila i campi sovrastanti;",
                two = "carica la massa nelle posizioni indicate nello schema;",
                three = "fai doppio click nella tabella per inserire le letture;",
                four = "una volta completato l'inserimento, conferma con Ctrl + Invio."
              ) |>
                list_to_li()
            )
          )

        )
      ),

      ##### repeatability ui ----
      bslib::nav_panel(title = "Ripetibilità",

                       DT::DTOutput(ns("repeatability"))),

      ##### linearity ui ----
      bslib::nav_panel(title = "Linearità",

                       DT::DTOutput(ns("linearity")))


    ),

    tags$div(style = "padding-bottom: 30px",
             actionButton(ns("nextbtn"), "Avanti", width = '10%'))

  )
}

#' 03_measure Server Functions
#'
#' @noRd
mod_04_measure_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mynamespace <- environment(ns)[['namespace']]
    r[[mynamespace]] <- shiny::reactiveValues()

    #### eccentricity server ----
    # empty dataframe for eccentricity
    eccentricity_df <- data.frame(posizione = 1:5,
                                  lettura = rep(NA, times = 5),
                                  DI = rep(NA, times = 5)
    )

    # for the first set of eccentricity measurements
    eccrv <- reactiveValues(data = eccentricity_df)

    # eccentricity table
    output$eccentricity <- DT::renderDT({
     DTeccentricity(eccrv$data, r$scale$signifdigits)
    })

    # when the table is edited the differences are computed
    observeEvent(input$eccentricity_cell_edit, {

      eccrv$data <- DT::editData(eccrv$data, input$eccentricity_cell_edit, "eccentricity", rownames = FALSE)
      eccrv$data$DI <- eccdiff(eccrv$data$lettura, r$scale$signifdigits)

    })

    # eccentricity results
    eccresult <- reactive({
      eccentricityresult(differences = eccrv$data$DI,
                          mydigits = r$scale$signifdigits,
                          massload = input$eccload,
                          minfield = input$minfield,
                          maxfield = input$maxfield,
                          mincal = input$mincal,
                          maxcal = input$maxcal,
                          givensd = input$gsd)
    })

    output$eccresult <- renderText({eccresult()$result})

    #### repeatability server ----


    #### linearity server ----


    #### saving inputs and outputs ----
    observeEvent(input$nextbtn, {

      inputnames <- names(input)
      savename <- inputnames[inputnames %not_in% "nextbtn"]

      #### saving inputs ----
      lapply(savename, function (x) {
        r[[mynamespace]][[x]] <- input[[x]]
      })


      #### saving the eccentricity output ----
      resultname <- names(eccresult())

      lapply(resultname, function (x) {
        r[[mynamespace]][[x]] <- eccresult()[[x]]
      })

    })


  })
}

## To be copied in the UI
# mod_03_measure_ui("03_measure_1")

## To be copied in the server
# mod_03_measure_server("03_measure_1")
