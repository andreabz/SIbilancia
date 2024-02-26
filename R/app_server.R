#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib nav_remove nav_insert
#' @noRd
app_server <- function(input, output, session) {
  options(scipen = 999,
          DT.options = list(pageLength = 11,
                            dom = 't',
                            ordering = FALSE))

  r <- reactiveValues()

  mod_01_device_server("scale", r)
  mod_02_method_server("calibration", r)
  mod_03_environment_server("environment", r)
  mod_04_measure_server("measure", r)
  mod_05_report_server("report", r)


  ##### dinamically remove and add measurement panels ----
  observeEvent(r$scale$nint, {

    # remove the measurement panels
    lapply(1:2, function(x) {

      valuepanel <- paste0("range", x)

      bslib::nav_remove(
        id = "mainmenu",
        target = valuepanel,
    )
    })

    npanel <- r$scale$nint |> as.numeric()

    lapply(npanel:1, function(x) {

      namepanel <- paste0("Intervallo ", x)
      valuepanel <- paste0("range", x)

      # adding the measurement panels
      bslib::nav_insert(
        id = "mainmenu",
        target = "environment",
        select = FALSE,
        bslib::nav_panel(namepanel, value = valuepanel, mod_04_measure_ui(valuepanel))
      )

      # adding the server modules
      mod_04_measure_server(valuepanel, r)
    })

  })
}
