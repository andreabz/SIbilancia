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

    # Sidebar with a slider input for number of bins
    textOutput("idcert"),
    dateInput("date", "Data di emissione:", language = "it"),
    textInput("client", "Destinatario:"),

    textInput("inst", "Strumento:"),
    textInput("place", "Collocazione:"),
    radioButtons("type", "Tipo di bilancia:",
                 choices = c("tecnica" = "tech",
                             "analitica" = "anal"),
                 inline = TRUE),
    textInput("manu", "Produttore:"),
    textInput("model", "Modello:"),
    textInput("idn", "Matricola:"),
    numericInput("nfor", "Unità di formato (g):", 0.0001, min = 0, max = 0.01),

    textInput("doced", "Edizione:"),
    textInput("docver", "Versione:"),
    dateInput("docdate", "Data di emissione:", language = "it"),
    textInput("refsampleid", "Sigla dei campioni:"),
    textInput("refcertid", "Certificato di taratura:"),
    textInput("reflatid", "Identificativo dei centri LAT:"),
    dateInput("certval", "Data di scadenza dei certificati:", language = "it"),

    numericInput("tinit", "Temperatura iniziale (°C):", 20, min = 10, max = 30),
    numericInput("uinit", "Incertezza della temperatura iniziale (°C):", 1, min = 0.1, max = 10),
    numericInput("tend", "Temperatura finale (°C):", 20, min = 10, max = 30),
    numericInput("uend", "Incertezza della temperatura finale (°C):", 1, min = 0.1, max = 10),
    textInput("termid", "Identificativo del termometro:"),
    dateInput("terminitdate", "Data di taratura:", language = "it"),
    dateInput("termenddate", "Data di scadenza della taratura:", language = "it"),

    numericInput("lowuse", "Minimo del campo di utilzzo (g):", 1, min = 0, max = 100),
    numericInput("highuse", "Massimo del campo di utilzzo (g):", 1, min = 0, max = 100),
    numericInput("lowcal", "Minimo del campo di taratura (g):", 1, min = 0, max = 100),
    numericInput("highcal", "Massimo del campo di taratura (g):", 1, min = 0, max = 100),
    numericInput("sd", "Scarto tipo di ripetibilità (g):", 0.01, min = 0, max = 10),

    DT::DTOutput("eccentricity1"),
    textOutput("maxdiff"),
    textOutput("uncecc"),
    textOutput("result")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
                                       options = list(dom = 't', ordering = FALSE),
                                       colnames = c("Posizione", "Lettura", "Differenza rispetto alla posizione 1"),
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
    round(mysignifdigits()) |>
    format(scientific = FALSE)

  list(maxdiff = maxdiff,
       uncecc = unc,
       result = ifelse(maxdiff > 3 * input$sd,
                       "non è possibile proseguire con la taratura",
                       "conforme"
                       )
       )

})

output$maxdiff <- renderText(ecc1result()$maxdiff)
output$uncecc <- renderText(ecc1result()$uncecc)
output$result <- renderText(ecc1result()$result)


}


# Run the application
shinyApp(ui = ui, server = server)
