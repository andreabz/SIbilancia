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
mod_04_measure_ui <- function(id) {
  ns <- NS(id)
  tagList(

    bslib::layout_columns(
      width = 2,

    numericInput(ns("minfield"), "Minimo del campo di utilizzo (g):", value = 0, min = 0),
    numericInput(ns("maxfield"), "Massimo del campo di utilizzo (g):", value = 0, min = 0),
    numericInput(ns("mincal"), "Minimo dell'intervallo di taratura (g):", value = 0, min = 0),
    numericInput(ns("maxcal"), "Massimo dell'intervallo di taratura (g):", value = 0, min = 0),
    numericInput(ns("gsd"), "Scarto tipo di ripetibilità (g):", value = 0, min = 0)

    ),

    bslib::navset_card_tab(
      height = 500,

    bslib::nav_panel(
      fillable = FALSE,
      title = "Eccentricità",

      DT::DTOutput(ns("eccentricity")),
      htmlOutput(ns("eccresult"))
    ),

    bslib::nav_panel(
      title = "Ripetibilità",

      DT::DTOutput(ns("repeatability"))
    ),

    bslib::nav_panel(
      title = "Linearità",

      DT::DTOutput(ns("linearity"))
    )


  ),

  tags$div(style = "padding-bottom: 30px",
           actionButton(ns("nextbtn"), "Avanti", width = '10%')
  )

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

    #### eccentricity ----
    # empty dataframe for eccentricity
    eccentricity_df <- data.frame(posizione = 1:5,
                                  lettura = rep(NA, times = 5),
                                  DI = rep(NA, times = 5)
    )

    # for the first set of eccentricity measurements
    eccrv <- reactiveValues(data = eccentricity_df)



    # eccentricity table
    output$eccentricity <- DT::renderDT(eccrv$data,
                                         style = "bootstrap",
                                         selection = "none",
                                         colnames = c("Posizione", "Lettura (g)", "Differenza rispetto alla posizione 1 (g)"),
                                         rownames = FALSE,
                                         editable = list(target = "column",
                                                         numeric = 2,
                                                         disable = list(columns = c(0, 2)))
    )

    # when the table is edited the differences are computed
    observeEvent(input$eccentricity_cell_edit, {

      eccrv$data <- DT::editData(eccrv$data, input$eccentricity_cell_edit, "eccentricity", rownames = FALSE)
      eccrv$data$DI <- abs(eccrv$data$lettura - eccrv$data$lettura[1]) |> round(r$scale$signifdigits)

    })

    # non NA elements in the first eccentricity table
    ecclength <- reactive({
      eccrv$data$DI[!is.na(eccrv$data$DI)] |>
        length()
    })

    # max difference due to eccentricity
    eccresult <- reactive({
      req(ecclength() == 5)

      maxdiff <- max(eccrv$data$DI)
      unc <- (maxdiff/(2 * sqrt(3))) |>
        mysigformat(r$scale$signifdigits)

      list(maxdiff = maxdiff,
           uncecc = unc,
           result = ifelse(maxdiff > 3 * input$gsd,
                           "non conforme",
                           "conforme"
           )
      )

    })


    output$eccresult <- renderText({

      eccresult()$result
    })





    #### saving all the inputs ----
    observeEvent(input$nextbtn, {

      inputnames <- names(input)
      savename <- inputnames[inputnames %not_in% "nextbtn"]

      lapply(savename, function (x) {
        r[[mynamespace]][[x]] <- input[[x]]
      })

    })


  })
}

## To be copied in the UI
# mod_03_measure_ui("03_measure_1")

## To be copied in the server
# mod_03_measure_server("03_measure_1")
