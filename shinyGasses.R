
# Fist try at R shiny script

library(shiny)
# library(gassyPants)

ui <- fluidPage(

  titlePanel("Coupled O2 and CO2 diel metabolism 'toy' model"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs and outputs----
    sidebarPanel(
      # Input: Sliders
      p("Slope of P-I curve (alpha PI; umol/m2/hr/uE)", style='font-size:75%'),
      sliderInput(inputId = "alphaPI", label = NULL,
                  min = 0.1, max = 10, value = 1),
      p("Ecosystem respiration to a standard temperature (Rstd; mol/m2/day)",
          style='font-size:75%'),
      sliderInput(inputId = "Rstd", label = NULL,
                  min = 0.1, max = 10, value = 1),
      p("Gas transfer velocity for CO2 at a standard temperature  (kstd; m/hr)",
          style='font-size:75%'),
      sliderInput(inputId = "kstd", label = NULL,
                  min = 0.01, max = 10, value = 0.15),
      p("Average stream depth (m)", style='font-size:75%'),
      numericInput(inputId = "depth", label = NULL,
                   min = 0, max = 100, value = 0.5),
      p("Total alkalinity (umol/L)", style='font-size:75%'),
      numericInput(inputId = "TAlk", label = NULL,
                   min = 0, max = 10000, value = 1000),
      p("Inital dissolved inorganic carbon (DIC; umol/L)",
          style='font-size:75%'),
      numericInput(inputId = "TCO2_init", label = NULL,
                   min = 0, max = 10000, value = 1200),
      p("Inital partial pressure of dissolved CO2 (pCO2; uatm)",
          style='font-size:75%'),
      numericInput(inputId = "pCO2_init", label = NULL,
                   min = 0, max = 10000, value = 2000),
      p("Inital partial pressure of dissolved CO2 in air (pCO2; uatm)",
          style='font-size:75%'),
      numericInput(inputId = "pCO2_air", label = NULL,
                   min = 0, max = 10000, value = 410),
      p("Inital concentration of dissolved O2 (umol)", style='font-size:75%'),
      numericInput(inputId = "O2_init", label = NULL,
                   min = 0, max = 10000, value = 280),
      h3(HTML("24 hr GPP:  "),textOutput(outputId = "GPP")),
      h3(HTML("24 hr ER:  "),textOutput(outputId = "ER")),
      h3(HTML("24 hr G:  "),textOutput(outputId = "G"))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Time series plot of dissolved O2 and pCO2 ----
      plotOutput(outputId = "timeSeriesPlot_O2_CO2",
          height='200px', width='auto'),
      plotOutput(outputId = "timeSeriesPlot_pH_DIC",
          height='250px', width='auto')

    )
  )
)

# Define server logic required to time seroes plot of model output ----
server <- function(input, output) {

  # Background physical data from Fishtrap Cr.
  # Not editable by users
  x.lt <- as.POSIXlt(FishtrapCr$dateTime)
  temperature <- FishtrapCr$Temp_C
  PAR <- FishtrapCr$PAR_uE
  localHour <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600
  DOY <- x.lt$yday
  lat <- 48.93861111
  long <- -122.47861111
  tz <- -8
  masl <- 1
  salinity <- 0

  getModRes = reactive({

    # Static variable inputs (single value) from user interface
    depth <- input$depth
    TAlk <- input$TAlk
    TCO2_init <- input$TCO2_init      # Units of umol/L
    pCO2_init <- input$pCO2_init     # Units of uatm
    O2_init <- input$O2_init     # Units of umol
    pCO2_air <- input$pCO2_air     # Units of uatm

    # Dynamic variable inputs (sliders) from user interface
    alphaPI_hr <- input$alphaPI    # Units are umol/m2/hr/uE
    Rstd_day <- input$Rstd        # Units are mol/m2/day
    kstd_hr <- input$kstd      # Units are m/hr

    modelResults <- main(temperature = temperature, localHour = localHour, DOY = DOY,
       PAR = PAR, TAlk = TAlk, pCO2_init = pCO2_init,
       pCO2_air = pCO2_air, alphaPI_hr = alphaPI_hr, Rstd_day = Rstd_day,
       kstd_hr = kstd_hr, referenceT = 20, lat = lat, long = long,
       depth = depth, salinity = 0, masl=1, O2_init=O2_init)

    return(modelResults)
  })

  # 2. Output is two plots and numberic values for 24 hr GPP, ER, and G
  output$timeSeriesPlot_O2_CO2 <- renderPlot({
    modres = getModRes()
    # mm <<- modres
    plotO2_CO2(modres)
  })
  output$timeSeriesPlot_pH_DIC <- renderPlot({
    modres = getModRes()
    plotpH_DIC(modres)
  })
  #Add numeric outputs
  output$GPP <- renderText({
    modres = getModRes()
    modres$GPP
  })
  output$ER <- renderText({
    modres = getModRes()
    modres$ER
  })
  output$G <- renderText({
    modres = getModRes()
    modres$G
  })
}

shinyApp(ui = ui, server = server)
