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
    actionButton("ecc1calc", label = "Calcola")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  eccentricity_df <- data.frame(posizione = 1:5,
                                lettura = rep(NA, times = 5),
                                DI = rep(NA, times = 5)
  )

  ecc1 <- reactiveValues(data = eccentricity_df)

  output$idcert <- renderText({
    paste0("Certificato: ",
           input$inst,
           "-",
           input$date)
  })


  output$eccentricity1 <- DT::renderDT(ecc1$data,
                                       style = "bootstrap",
                                       options = list(dom = 't', ordering = FALSE),
                                       colnames = c("Posizione", "Lettura", "Variazione rispetto alla posizione 1"),
                                       rownames = FALSE,
                                       editable = list(target = "column",
                                                       numeric = 2,
                                                       disable = list(columns = c(0, 2)))
  )

  observeEvent(input$ecc1calc, {
    req(input$eccentricity1_cell_edit)

    ecc1$data <- DT::editData(ecc1$data, input$eccentricity1_cell_edit, "eccentricity1", rownames = FALSE)

    ecc1$data$DI <- ecc1$data$lettura - ecc1$data$lettura[1]

  })
}

# Run the application
shinyApp(ui = ui, server = server)
