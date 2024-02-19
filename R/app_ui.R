#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel nav_spacer
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "SI bilancia",
      window_title = "SI precisa",
      theme = bslib::bs_theme(bootswatch = "cosmo",
                              version = 5,
                              "navbar-bg" = "#2780E3",
                              "navbar-brand-font-size" = "2rem"),
      inverse = TRUE,
      fluid = TRUE,
      collapsible = TRUE,
      lang = "it",

      bslib::nav_panel("Bilancia", value = "scale", mod_01_device_ui("scale")),
      bslib::nav_panel("Taratura", value = "calibration", mod_02_method_ui("calibration")),
      bslib::nav_panel("Misure", value = "measurement", mod_03_measure_ui("measure")),
      bslib::nav_panel("Report", value = "report", ""),

      bslib::nav_spacer(),

      bslib::nav_menu("Leggimi",
                      align = "right",

                      bslib::nav_panel("Per iniziare", value = "readme", ""),
                      bslib::nav_panel("Esempi", value = "readme", ""),
                      bslib::nav_panel("Validazione", value = "readme", ""),
                      bslib::nav_panel("Versioni", value = "readme", ""),
                      bslib::nav_panel("Struttura", value = "readme", "")
                      )

      )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SI bilancia"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
