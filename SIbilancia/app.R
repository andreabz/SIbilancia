#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SI bilancia"),

    #### certificate data ----
    textOutput("idcert"),
    dateInput("date", "Data di emissione:", language = "it"),
    textInput("client", "Destinatario:"),

    #### scale data ----
    #textInput("inst", "Strumento:"),
    #textInput("place", "Collocazione:"),
    #radioButtons("type", "Tipo di bilancia:",
    #             choices = c("tecnica" = "tech",
    #                         "analitica" = "anal"),
    #             inline = TRUE),
    #textInput("manu", "Produttore:"),
    #textInput("model", "Modello:"),
    #textInput("idn", "Matricola:"),
    #numericInput("nfor", "Unità di formato (g):", 0.0001, min = 0, max = 0.01),

    #### calibration information ----
    #textInput("doced", "Edizione:"),
    #textInput("docver", "Versione:"),
    #dateInput("docdate", "Data di emissione:", language = "it"),
    # textInput("refsampleid", "Sigla dei campioni:"),
    # textInput("refcertid", "Certificato di taratura:"),
    # textInput("reflatid", "Identificativo dei centri LAT:"),
    # dateInput("certval", "Data di scadenza dei certificati:", language = "it"),

    #### condition data ----
    numericInput("tinit", "Temperatura iniziale (°C):", 20, min = 10, max = 30),
    numericInput("uinit", "Incertezza della temperatura iniziale (°C):", 1, min = 0.1, max = 10),
    numericInput("tend", "Temperatura finale (°C):", 20, min = 10, max = 30),
    numericInput("uend", "Incertezza della temperatura finale (°C):", 1, min = 0.1, max = 10),
    textInput("termid", "Identificativo del termometro:"),
    dateInput("terminitdate", "Data di taratura:", language = "it"),
    dateInput("termenddate", "Data di scadenza della taratura:", language = "it"),

    #### test data ----
    numericInput("lowuse", "Minimo del campo di utilzzo (g):", 1, min = 0, max = 100),
    numericInput("highuse", "Massimo del campo di utilzzo (g):", 1, min = 0, max = 100),
    numericInput("lowcal", "Minimo del campo di taratura (g):", 1, min = 0, max = 100),
    numericInput("highcal", "Massimo del campo di taratura (g):", 1, min = 0, max = 100),
    numericInput("sd", "Scarto tipo di ripetibilità (g):", 0.01, min = 0, max = 10),

    ##### eccentricity data ----
    numericInput("eccweight", "Massa usata per la prova di eccentricità (g):", 0.01, min = 0, max = 10),

    DT::DTOutput("eccentricity1"),
    textOutput("eccmaxdiff"),
    textOutput("eccunc"),
    textOutput("eccresult"),

    ##### repeatability data ----
    numericInput("repweight", "Massa usata per la prova di ripetibilità (g):", 0.01, min = 0, max = 10),

    DT::DTOutput("repeatability1"),
    textOutput("repsd"),
    textOutput("represult"),

    ##### load data ----

    DT::DTOutput("load1")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  options(DT.options = list(pageLength = 11,
                            dom = 't',
                            ordering = FALSE))

  # certificate title
  output$idcert <- renderText({
    paste0("Certificato: ",
           input$inst,
           "-",
           input$date)
  })

  # get the decimal places of a number
  # from https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
  # @nisetama
  decimalplaces <- function(x) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      sapply(x, function(y){
        nchar(
          sub("^-?\\d*\\.?","",
                  format(y, scientific = F)
              )
          )
      })
    } else {
      return(0)
    }
  }

  # significant digits
  mysignifdigits <- reactive({ decimalplaces(input$nfor) })

  # rounding
  mysigformat <- function(x, digits) {
    stopifnot(
      is.numeric(x),
      is.numeric(digits),
      digits > 0
    )

    round(x, digits) |>
      format(scientific = FALSE)

  }

#### calibration temperature ----
  # checking temperature difference
  tempdiff <- reactive({
    abs(input$tend - input$tinit)
  })

  tempreq <- reactive({
    ifelse(input$type == "tech", 5, 2)
  })

#### eccentricity ----
  # empty dataframe for eccentricity
  eccentricity_df <- data.frame(posizione = 1:5,
                                lettura = rep(NA, times = 5),
                                DI = rep(NA, times = 5)
  )

  # for the first set of eccentricity measurements
  ecc1 <- reactiveValues(data = eccentricity_df)



  # eccentricity table
  output$eccentricity1 <- DT::renderDT(ecc1$data,
                                       style = "bootstrap",
                                       selection = "none",
                                       colnames = c("Posizione", "Lettura (g)", "Differenza rispetto alla posizione 1 (g)"),
                                       rownames = FALSE,
                                       editable = list(target = "column",
                                                       numeric = 2,
                                                       disable = list(columns = c(0, 2)))
  )

  # when the table is edited the differences are computed
  observeEvent(input$eccentricity1_cell_edit, {
    req(mysignifdigits())

    ecc1$data <- DT::editData(ecc1$data, input$eccentricity1_cell_edit, "eccentricity1", rownames = FALSE)
    ecc1$data$DI <- abs(ecc1$data$lettura - ecc1$data$lettura[1]) |> round(mysignifdigits())

  })

# non NA elements in the first eccentricity table
ecc1length <- reactive({
  ecc1$data$DI[!is.na(ecc1$data$DI)] |>
    length()
  })

# max difference due to eccentricity
ecc1result <- reactive({
  req(ecc1length() == 5)
  req(mysignifdigits())

  maxdiff <- max(ecc1$data$DI)
  unc <- (maxdiff/(2 * sqrt(3))) |>
    mysigformat(mysignifdigits())

  list(maxdiff = maxdiff,
       uncecc = unc,
       result = ifelse(maxdiff > 3 * input$sd,
                       "non è possibile proseguire con la taratura",
                       "conforme"
                       )
       )

})

output$eccmaxdiff <- renderText({

  validate(
    need(tempdiff() <= tempreq(), "La variazione di temperatura è eccessiva.")
    )

  ecc1result()$maxdiff

  })

output$eccunc <- renderText({
  validate(
    need(tempdiff() <= tempreq(), "")
    )

  ecc1result()$uncecc
  })

output$eccresult <- renderText({
  validate(
    need(tempdiff() <= tempreq(), "")
  )

  ecc1result()$result
  })

#### repeatability ----
# empty dataframe for repeatability
repeatability_df <- data.frame(posizione = 1:10,
                               lettura = rep(NA, times = 10)
)

# for the first set of repeatability measurements
rep1 <- reactiveValues(data = repeatability_df)



# eccentricity table
output$repeatability1 <- DT::renderDT(rep1$data,
                                     style = "bootstrap",
                                     selection = "none",
                                     colnames = c("Ripetizione", "Lettura (g)"),
                                     rownames = FALSE,
                                     editable = list(target = "column",
                                                     numeric = 1,
                                                     disable = list(columns = 0))
)

# when the table is edited the differences are computed
observeEvent(input$repeatability1_cell_edit, {
  req(mysignifdigits())

  rep1$data <- DT::editData(rep1$data, input$repeatability1_cell_edit, "repeatability1", rownames = FALSE)

})

# non NA elements in the first repeatability table
rep1result <- reactive({

  n <- rep1$data$lettura[!is.na(rep1$data$lettura)] |> length()
  mysd <- rep1$data$lettura |> sd(na.rm = TRUE)

  list(
    n = n,
    sd = mysd |> round(mysignifdigits()),
    result = ifelse(mysd > input$sd,
           "non conforme",
           "conforme"
    )

  )
})

output$repsd <- renderText({
  validate(
    need(tempdiff() <= tempreq(), "La variazione di temperatura è eccessiva."),
    need(rep1result()$n >= 5, "")
  )

  rep1result()$sd

})

output$represult <- renderText({
  validate(
    need(tempdiff() <= tempreq(), ""),
    need(rep1result()$n >= 5, "Servono almeno 5 valori per il calcolo della ripetibilità.")
  )

  rep1result()$result
})

##### increasing and decreasing load ----
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

# for the first set of repeatability measurements
load1 <- reactiveValues(data = load_df)


# load table
output$load1 <- DT::renderDT(
  DT::datatable(load1$data,
                style = "bootstrap",
                selection = "none",
                colnames = c("Prova",
                             "Valore nominale (g)",
                             "Valore convenzionale (g)",
                             "Lettura (g)",
                             "Errore (g)",
                             "Prova",
                             "Lettura (g)",
                             "Errore (g)",
                             "Errore medio (g)"
                             ),
                rownames = FALSE,
                editable = list(target = "column",
                                numeric = 0:8,
                                disable = list(columns = c(0, 4, 5, 7, 8))
                                )
  ) |> DT::formatRound(c(5, 8, 9), digits = mysignifdigits() + 1)
)

# when the table is edited the differences are computed
observeEvent(input$load1_cell_edit, {
  req(mysignifdigits())

  load1$data <- DT::editData(load1$data, input$load1_cell_edit, "load1", rownames = FALSE)

  load1$data$error_up <- (load1$data$lettura_up - load1$data$val_conv)
  load1$data$error_down <- (load1$data$lettura_down - load1$data$val_conv)
  load1$data$avg_error <- rowMeans(cbind(load1$data$error_up, load1$data$error_down), na.rm = TRUE)
})

}


# Run the application
shinyApp(ui = ui, server = server)
