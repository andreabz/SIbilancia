##' 04_method UI Function
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
        ns("minuse"),
        "Minimo del campo di utilizzo (g):",
        value = 0,
        min = 0
      ),
      numericInput(
        ns("maxuse"),
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
      # height = 700,

      ##### eccentricity ui ----
      bslib::nav_panel(
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
              )
            ),
            bslib::card_body(
              max_height = "300px",
              DT::DTOutput(ns("eccentricity"), width = "500px")
            ),
            bslib::card_body(
              min_height = "120px",
              htmlOutput(ns("eccresult"))
            )
          ),
          bslib::card(
            bslib::card_body(
              add_www_img("scale_scheme.png")
            )
          ),
          bslib::card(
            bslib::card_header(shiny::icon("circle-info"), "Cosa devi fare"),
            bslib::card_body(
              list(
                a = "Compila i campi sovrastanti;",
                b = "seleziona una massa pari ad almeno un terzo del massimo dell'intervallo di taratura;",
                c = "carica la massa nelle posizioni indicate nello schema e annota le letture;",
                d = "fai doppio click nella tabella per inserire le letture;",
                e = "una volta completato l'inserimento, conferma con Ctrl + Invio."
              ) |>
                list_to_li() |>
                tags$ul()
            )
          )
        )
      ),

      ##### repeatability ui ----
      bslib::nav_panel(
        title = "Ripetibilità",
        bslib::layout_columns(
          col_widths = c(6, 3),
          bslib::card(
            bslib::card_body(
              numericInput(
                ns("repload"),
                "Massa utilizzata (g):",
                value = 0,
                min = 0
              )
            ),
            bslib::card_body(
              DT::DTOutput(ns("repeatability"), width = "500px"),
            ),
            bslib::card_body(
              padding = 15,
              htmlOutput(ns("represult"))
            )
          ),
          bslib::card(
            bslib::card_header(shiny::icon("circle-info"), "Cosa devi fare"),
            bslib::card_body(
              list(
                a = "Seleziona una massa posta circa a metà dell'intervallo di taratura;",
                b = "carica la massa al centro del piatto della bilancia ed esegui 10 misure ripetute,
                               annotando le letture;",
                c = "fai doppio click nella tabella per inserire le letture;",
                d = "una volta completato l'inserimento, conferma con Ctrl + Invio."
              ) |>
                list_to_li()|>
                tags$ul()
            )
          )
        )
      ),

      ##### linearity ui ----
      bslib::nav_panel(
        title = "Linearità",
        bslib::layout_columns(
          col_widths = c(9, 3),
          bslib::card(
            bslib::card_body(
              DT::DTOutput(ns("linearity"), width = "95%"),
            ),
            bslib::card_body(
              htmlOutput(ns("linresult"))
            )
          ),
          bslib::card(
            bslib::card_header(shiny::icon("circle-info"), "Cosa devi fare"),
            bslib::card_body(
              list(
                a = "Seleziona dieci masse circa equispaziate all'interno dell'intervallo di taratura;",
                b = "esegui una misura a carico nullo;",
                c = "carica pesi crescenti al centro del piatto delle bilancia e annota le letture;",
                d = "ripeti le letture caricando pesi decrescenti;",
                e = "fai doppio click nella tabella per inserire le letture;",
                f = "una volta completato l'inserimento, conferma con Ctrl + Invio."
              ) |>
                list_to_li() |>
                tags$ul()
            )
          )
        )
      ),
      #### uncertainty ui ----
      bslib::nav_panel(
        title = "Incertezza",
        bslib::layout_columns(
          col_widths = c(9, 3),
          bslib::card(
            bslib::card_body(
              DT::DTOutput(ns("uncertainty"), width = "550px"),
            ),
            bslib::card_body(
              htmlOutput(ns("uncresult"))
            )
          ),
          bslib::card(
            bslib::card_header(shiny::icon("circle-info"), "Cosa devi fare"),
            bslib::card_body(
              list(
                a = "Clicca sul menù Misure, posto nella barra blu in alto;",
                b = "nel caso sia presente più di un intervallo di taratura,
                spostati tra di essi cliccando sui
                relativi pulsanti;",
                c = "ripeti i test di eccentricità, ripetibilità e linearità
                per tutti gli intervalli di taratura previsti dalla bilancia."
              ) |>
                list_to_li() |>
                tags$ul()
            )
          )
        )
      )
    ),
    tags$div(
      style = "padding-bottom: 15px",
      actionButton(ns("nextbtn"), "Avanti", width = "10%")
    )
  )
}

#' 04_measure Server Functions
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

    # reactive values for eccentricity measurements
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
      eccentricity_result(differences = eccrv$data$DI,
                          mydigits = r$scale$signifdigits,
                          massload = input$eccload,
                          minuse = input$minuse,
                          maxuse = input$maxuse,
                          mincal = input$mincal,
                          maxcal = input$maxcal,
                          givensd = input$gsd)
    })

    output$eccresult <- renderText({eccresult()$eccentricity_html})

    #### repeatability server ----
    # empty dataframe for repeatability
    repeatability_df <- data.frame(posizione = 1:10,
                                   lettura = rep(NA, times = 10)
    )

    # reactive values for repeatability measurements
    reprv <- reactiveValues(data = repeatability_df)

    # repeatability table
    output$repeatability <- DT::renderDT({
      DTrepeatability(reprv$data, r$scale$signifdigits)
    })

    # table editing
    observeEvent(input$repeatability_cell_edit, {

      reprv$data <- DT::editData(reprv$data, input$repeatability_cell_edit, "repeatability", rownames = FALSE)
      reprv$data$lettura <- reprv$data$lettura |> as.numeric()
    })

    # repeatability results
    represult <- reactive({
      repeatability_result(measures = reprv$data$lettura,
                           mydigits = r$scale$signifdigits,
                           massload = input$repload,
                           minuse = input$minuse,
                           maxuse = input$maxuse,
                           mincal = input$mincal,
                           maxcal = input$maxcal,
                           givensd = input$gsd)
    })

    output$represult <- renderText({represult()$repeatability_html})


    #### linearity server ----
    #empty dataframe for linearity test
    load_df <- data.frame(pos_up = 1:11,
                          val_nom = rep(NA, times = 11) |> as.numeric(),
                          val_conv = rep(NA, times = 11) |> as.numeric(),
                          lettura_up = rep(NA, times = 11) |> as.numeric(),
                          error_up = rep(NA, times = 11) |> as.numeric(),
                          pos_down = 11:1,
                          lettura_down = rep(NA, times = 11) |> as.numeric(),
                          error_down = rep(NA, times = 11) |> as.numeric(),
                          avg_error = rep(NA, times = 11) |> as.numeric()
    )

    # reactive values for load measurements
    loadrv <- reactiveValues(data = load_df)

    output$linearity <- DT::renderDT({
      DTlinearity(loadrv$data, r$scale$signifdigits)
    })

    # when the table is edited the differences are computed
    observeEvent(input$linearity_cell_edit, {

      loadrv$data <- DT::editData(loadrv$data, input$linearity_cell_edit, "linearity", rownames = FALSE)

      loadrv$data$error_up <- (loadrv$data$lettura_up - loadrv$data$val_conv)
      loadrv$data$error_down <- (loadrv$data$lettura_down - loadrv$data$val_conv)
      loadrv$data$avg_error <- rowMeans(cbind(loadrv$data$error_up, loadrv$data$error_down), na.rm = TRUE)
    })

    #### uncertainty server ----
    # calibration uncertainty
    mycaluncertainty <- reactive({
      caluncertainty(mass = loadrv$data$val_nom,
                     readformat = r$scale$nfor,
                     ucert = 0,
                     uecc = eccresult()$eccentricity_uncertainty,
                     eccload = input$eccload,
                     urep = represult()$repeatability_uncertainty,
                     avglin = loadrv$data$avg_error)
    })

    uncert_df <- reactive({
      data.frame(val_nom = loadrv$data$val_nom,
                 adjustment = loadrv$data$avg_error,
                 uncertainty = mycaluncertainty())
    })

    output$uncertainty <- DT::renderDT({
      DTuncertainty(uncert_df(), r$scale$signifdigits)
    })

    # usage uncertainty
    scaleuncertainty <- reactive({
      req(uncert_df())

      usageuncertainty(adjustments = uncert_df()$adjustment,
                       uncertainties = uncert_df()$uncertainty,
                       mydigits = r$scale$signifdigits)
    })

    scaleuncertainty_result <- reactive({
      usageuncertainty_result(givensd = input$gsd,
                              uncusage = scaleuncertainty())
    })

    output$uncresult <- renderText({
      scaleuncertainty_result()$usage_html
    })

    #### saving inputs and outputs ----
    observeEvent(input$nextbtn, {

      inputnames <- names(input)
      savename <- inputnames[inputnames %not_in% "nextbtn"]

      #### saving inputs ----
      lapply(savename, function (x) {
        r[[mynamespace]][[x]] <- input[[x]]
      })

      #### saving the eccentricity output ----
      eccname <- names(eccresult())

      lapply(eccname, function (x) {
        r[[mynamespace]][[x]] <- eccresult()[[x]]
      })

      #### saving the repeatability output ----
      repname <- names(represult())

      lapply(repname, function (x) {
        r[[mynamespace]][[x]] <- represult()[[x]]
      })

      #### saving the linearity output ----
      r[[mynamespace]]$linearity <- loadrv$data

      #### saving the uncertainty output ----
      uncname <- names(scaleuncertainty_result())

      lapply(uncname, function (x) {
        r[[mynamespace]][[x]] <- scaleuncertainty_result()[[x]]
      })

    })

  })
}

## To be copied in the UI
# mod_03_measure_ui("03_measure_1")

## To be copied in the server
# mod_03_measure_server("03_measure_1")
