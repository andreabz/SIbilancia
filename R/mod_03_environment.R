##' 03_environment UI Function
#'
#' @description A shiny Module for the environmental conditions in
#'  the {SIbilancia} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} list.
#' @return a list text and numeric inputs, along with an action button
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib card card_header card_body layout_column layout_column_wrap accordion accordion_panel
#' @importFrom htmltools tags css
#' @importFrom glue glue
mod_03_environment_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(6, 3, 3),

      #### temperature ----
      bslib::card(
        fill = FALSE,
        height = "400px",
        bslib::card_header(
          shiny::icon("temperature-three-quarters"),
          "Condizioni ambientali"
        ),
        bslib::card_body(
          bslib::layout_column_wrap(
            width = 1/2,

            numericInput(
              ns("tinit"),
              "Temperatura iniziale (°C):",
              20,
              min = 10,
              max = 30
            ),
            numericInput(
              ns("uinit"),
              "Incertezza della temperatura iniziale (°C):",
              1,
              min = 0.1,
              max = 10
            ),
            numericInput(
              ns("tend"),
              "Temperatura finale (°C):",
              20,
              min = 10,
              max = 30
            ),
            numericInput(
              ns("uend"),
              "Incertezza della temperatura finale (°C):",
              1,
              min = 0.1,
              max = 10
            ),
            textInput(ns("termid"), "Identificativo del termometro:"),
            dateInput(ns("terminitdate"), "Data di taratura:", language = "it"),
            dateInput(ns("termenddate"), "Data di scadenza della taratura:", language = "it"),

            htmlOutput(ns("tempcheck"), style = "padding-top: 5px;")
          )

        )

      ),


      #### checks ----
      bslib::card(
        fill = FALSE,
        height = "400px",
        bslib::card_header(
          shiny::icon("square-check"),
          "Controlli preliminari"
        ),

        checkboxGroupInput(
          ns("check"),
          "",
          choices = c(
            "Planarità" = "plan",
            "Pulizia piatto" = "clean",
            "Assenza instabilità meccaniche" = "mech"
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

#' 03_environment Server Functions
#'
#' @noRd
mod_03_environment_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mynamespace <- environment(ns)[['namespace']]
    r[[mynamespace]] <- shiny::reactiveValues()

    #### checking temperature difference ----
    tempdiff <- reactive({
      abs(input$tend - input$tinit)
    })

    tempreq <- reactive({
      ifelse(r$scale$type == "tech", 5, 2)
    })

    output$tempcheck <- renderText({

      mycol <- ifelse(tempdiff() <= tempreq(), "text-success", "text-warning")
      mysign <- ifelse(tempdiff() <= tempreq(), "≤", ">")

      glue::glue("</br> <p class = {mycol}> La differenza tra la temperatura iniziale e finale è {mysign} {tempreq()} °C</p>")

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
# mod_03_environment_ui("03_environment_1")

## To be copied in the server
# mod_03_environment_server("03_environment_1")
