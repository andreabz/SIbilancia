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

    numericInput(ns("minfield"), "Minimo del campo di utilizzo (g):", value = 0, min = 0),
    numericInput(ns("maxfield"), "Massimo del campo di utilizzo (g):", value = 0, min = 0),
    numericInput(ns("mincal"), "Minimo dell'intervallo di taratura (g):", value = 0, min = 0),
    numericInput(ns("maxcal"), "Massimo dell'intervallo di taratura (g):", value = 0, min = 0),
    numericInput(ns("gsd"), "Scarto tipo di ripetibilità (g):", value = 0, min = 0)

    ),

    bslib::navset_card_tab(
      height = 600,

    bslib::nav_panel(
      fillable = FALSE,
      title = "Eccentricità",

      bslib::layout_columns(
        col_widths = c(6, 3, 3),

        bslib::card(
          bslib::card_body(
            numericInput(ns("eccload"), "Massa utilizzata (g):", value = 0, min = 0),

            DT::DTOutput(ns("eccentricity")),
            htmlOutput(ns("eccresult"))
          )
        ),

        bslib::card(
          bslib::card_body(

            shiny::tags$img(src = "www/scale_scheme.png",
                            style = "height:100%;
                                     width:auto;
                                     display:block;
                                     margin-left:auto;
                                     margin-right:auto;")
          )
        ),

        bslib::card(
          bslib::card_header(shiny::icon("circle-info"), "Cosa devi fare"),
          bslib::card_body(

            list(one = "Compila i campi sovrastanti;",
                 two = "carica la massa nelle posizioni indicate nello schema;",
                 three = "fai doppio click nella tabella per inserire le letture;",
                 four = "una volta completato l'inserimento, conferma con Ctrl + Invio.") |>
              list_to_li()
          )
        )

      )


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
    output$eccentricity <- DT::renderDT(
      DT::datatable(eccrv$data,
                    style = "bootstrap",
                    selection = "none",
                    colnames = c("Posizione", "Lettura (g)", "Differenza rispetto alla posizione 1 (g)"),
                    rownames = FALSE,
                    editable = list(
                      target = "column",
                      numeric = c(1, 2),
                      disable = list(columns = c(0, 2))
                      )
                    ) |>
        DT::formatRound(c(2, 3), digits = r$scale$signifdigits)
    )

    # when the table is edited the differences are computed
    observeEvent(input$eccentricity_cell_edit, {

      eccrv$data <- DT::editData(eccrv$data, input$eccentricity_cell_edit, "eccentricity", rownames = FALSE)
      numread <- eccrv$data$lettura |> as.numeric()
      eccrv$data$DI <- abs(numread - numread[1]) |> round(r$scale$signifdigits)

    })

    # non NA elements in the first eccentricity table
    ecclength <- reactive({
      eccrv$data$DI[!is.na(eccrv$data$DI)] |>
        length()
    })

    # max difference due to eccentricity
    eccresult <- reactive({

      maxdiff <- max(eccrv$data$DI)
      unc <- (maxdiff/(2 * sqrt(3))) |> mysigformat(r$scale$signifdigits)
      mycol <- ifelse(maxdiff > 3 * input$gsd, "text-warning", "text-success")
      myres <- ifelse(maxdiff > 3 * input$gsd, "non conforme", "conforme")
      mytext <- if(ecclength() < 5) {
        "Servono 5 misure." |> mywarning()
      } else if (input$eccload > input$maxcal) {
        "La massa impiegata non può eccedere il massimo dell'intervallo di taratura." |> mywarning()
      } else if (input$eccload < input$mincal) {
        "La massa impiegata non può essere inferiore al minimo dell'intervallo di taratura." |> mywarning()
      } else if (input$maxcal > input$maxfield) {
        "Il massimo dell'intervallo di taratura non può eccedere il massimo del campo di utilizzo." |> mywarning()
      } else if (input$mincal < input$minfield) {
        "Il minimo dell'intervallo di taratura non può essere inferiore al minimo del campo di utilizzo." |> mywarning()
      } else {
        glue::glue(
          "<ul>
              <li class = {mycol}> esito: {myres} </li>
              <li> differenza massima = {maxdiff} g </li>
              <li> contributo all'incertezza u(e) = {unc} g </li>
          </ul>")
      }
      myplaintext <- if(ecclength() < 5) {
        "Servono 5 misure."
      } else if (input$eccload > input$maxcal) {
        "La massa impiegata non può eccedere il massimo dell'intervallo di taratura."
      } else if (input$eccload < input$mincal) {
        "La massa impiegata non può essere inferiore al minimo dell'intervallo di taratura."
      } else if (input$maxcal > input$maxfield) {
        "Il massimo dell'intervallo di taratura non può eccedere il massimo del campo di utilizzo."
      } else if (input$mincal < input$minfield) {
        "Il minimo dell'intervallo di taratura non può essere inferiore al minimo del campo di utilizzo."
      } else {
        glue::glue(
               "* esito: {myres}
                * differenza massima = {maxdiff} g
                * contributo all'incertezza u(e) = {unc} g"
               )
      }

      list(maxdiff = maxdiff,
           uncecc = unc,
           result = mytext,
           plainresult = myplaintext
           )
    })


    output$eccresult <- renderText(eccresult()$result)


    #### saving all the inputs ----
    observeEvent(input$nextbtn, {

      inputnames <- names(input)
      savename <- inputnames[inputnames %not_in% "nextbtn"]

      lapply(savename, function (x) {
        r[[mynamespace]][[x]] <- input[[x]]
      })

      #### saving the output ----
      resultname <- names(eccresult())

      lapply(resultname, function (x) {
        r[[mynamespace]][[x]] <- eccresult()[[x]]
      })

      print(r[[mynamespace]])
      print(r[[mynamespace]]$result)
      print(output)

    })


  })
}

## To be copied in the UI
# mod_03_measure_ui("03_measure_1")

## To be copied in the server
# mod_03_measure_server("03_measure_1")
