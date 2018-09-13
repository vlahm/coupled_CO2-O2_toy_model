
library(shiny)
library(devtools)
install_github('gholtgrieve/gassyPants')
library(gassyPants)
source('helpers.R')

ui = fluidPage(
    # titlePanel("Coupled O2 and CO2 diel metabolism 'toy' model"),
    titlePanel(HTML(paste("<h4 style='color:#663399'>Coupled O2 and CO2 diel",
        "metabolism 'toy' model</h3>")), windowTitle='CO2-O2 Toy Model'),

    sidebarLayout(
        sidebarPanel(

            # Input: Sliders
            fluidRow(
                column(4,
                    p(strong('Slope of P-I curve'), '(alpha; umol/m2/hr/uE)',
                        style='font-size:75%')
                    # HTML(paste0("<p style='font-size:75%'>Slope of P-I ",
                    #     "curve<br>(alpha PI; umol/m2/hr/uE)</p>"))
                ),
                column(8,
                    sliderInput(inputId='alphaPI', label=NULL,
                        min=0.1, max=10, value=1)
                )
            ),
            fluidRow(
                column(4,
                    p(strong('ER to std temp'),
                        '(Rstd; mol/m2/day)', style='font-size:75%')
                ),
                column(8,
                    sliderInput(inputId='Rstd', label=NULL,
                        min=0.1, max=10, value=1)
                )
            ),
            fluidRow(
                column(4,
                    p(strong('k of CO2 at std temp'),
                        '(kstd; m/hr)', style='font-size:75%')
                ),
                column(8,
                    sliderInput(inputId='kstd', label=NULL,
                        min=0.01, max=10, value=0.15)
                )
            ),
            fluidRow(
                column(6,
                    p(strong('Avg stream depth'), '(m)', style='font-size:75%'),
                    numericInput(inputId='depth', label=NULL,
                        min=0, max=100, value=0.5),
                    p(strong('Tot alkalinity'), '(umol/L)',
                        style='font-size:75%'),
                    numericInput(inputId='TAlk', label=NULL,
                        min=0, max=10000, value=1000),
                    p(strong('Initial DIC'), '(umol/L)',
                        style='font-size:75%'),
                    numericInput(inputId='TCO2_init', label=NULL,
                        min=0, max=10000, value=1200)
                ),
                column(6,
                    p(strong('Init pCO2 aq'), '(uatm)',
                        style='font-size:75%'),
                    numericInput(inputId='pCO2_init', label=NULL,
                        min=0, max=10000, value=2000),
                    p(strong('Init pCO2 air'), '(uatm)', style='font-size:75%'),
                    numericInput(inputId='pCO2_air', label=NULL,
                        min=0, max=10000, value=410),
                    p(strong('Init [O2]'), '(umol)', style='font-size:75%'),
                    numericInput(inputId='O2_init', label=NULL,
                        min=0, max=10000, value=280)
                )
            ),
            fluidRow(
                column(4,
                    div(style='outline:solid #663399',
                        p(strong('24 hr GPP:'), style='font-size:75%'),
                        htmlOutput(outputId='GPP')
                    )
                ),
                column(4,
                    div(style='outline:solid #663399',
                        p(strong('24 hr ER:'), style='font-size:75%'),
                        htmlOutput(outputId='ER')
                    )
                ),
                column(4,
                    div(style='outline:solid #663399',
                        p(strong('24 hr G:'),style='font-size:75%'),
                        htmlOutput(outputId='G')
                    )
                )
            )
        ),
        mainPanel(

            # Output: Time series plot of dissolved O2 and pCO2 ----
            plotOutput(outputId='plotLegend', height='30px', width='auto'),
            plotOutput(outputId='timeSeriesPlot_O2_CO2',
                height='200px', width='auto'),
            plotOutput(outputId='timeSeriesPlot_pH_DIC',
                height='250px', width='auto')
        )
    )
)

# Define server logic required to time seroes plot of model output ----
server = function(input, output) {

    # Background physical data from Fishtrap Cr.
    # Not editable by users
    x.lt = as.POSIXlt(FishtrapCr$dateTime)
    temperature = FishtrapCr$Temp_C
    PAR = FishtrapCr$PAR_uE
    localHour = x.lt$hour + x.lt$min/60 + x.lt$sec/3600
    DOY = x.lt$yday
    lat = 48.93861111
    long = -122.47861111
    tz = -8
    masl = 1
    salinity = 0

    getModRes = reactive({

        # Static variable inputs (single value) from user interface
        depth = input$depth
        TAlk = input$TAlk
        TCO2_init = input$TCO2_init      # Units of umol/L
        pCO2_init = input$pCO2_init     # Units of uatm
        O2_init = input$O2_init     # Units of umol
        pCO2_air = input$pCO2_air     # Units of uatm

        # Dynamic variable inputs (sliders) from user interface
        alphaPI_hr = input$alphaPI    # Units are umol/m2/hr/uE
        Rstd_day = input$Rstd        # Units are mol/m2/day
        kstd_hr = input$kstd      # Units are m/hr

        modelResults = main(temperature=temperature, localHour=localHour,
            DOY=DOY, PAR=PAR, TAlk=TAlk, pCO2_init=pCO2_init,
            pCO2_air=pCO2_air, alphaPI_hr=alphaPI_hr, Rstd_day=Rstd_day,
            kstd_hr=kstd_hr, referenceT=20, lat=lat, long=long,
            depth=depth, salinity=0, masl=1, O2_init=O2_init)

        return(modelResults)
    })

    # 2. Output is two plots and numberic values for 24 hr GPP, ER, and G
    output$plotLegend = renderPlot({
        plotLegend()
    })
    output$timeSeriesPlot_O2_CO2 = renderPlot({
        modres = getModRes()
        # mm <<- modres
        plotO2_CO2(modres)
    })
    output$timeSeriesPlot_pH_DIC = renderPlot({
        modres = getModRes()
        plotpH_DIC(modres)
    })
    #Add numeric outputs
    output$GPP = renderText({
        modres = getModRes()
        gppout = paste("<p style='color:#663399'><strong>", round(modres$GPP, 4),
            '</strong></p>')
        return(HTML(gppout))
    })
    output$ER = renderText({
        modres = getModRes()
        erout = paste("<p style='color:#663399'><strong>", round(modres$ER, 4),
            '</strong></p>')
        return(HTML(erout))
    })
    output$G = renderText({
        modres = getModRes()
        print(modres$G)
        gout = paste("<p style='color:#663399'><strong>", round(modres$G, 4),
            '</strong></p>')
        return(HTML(gout))
    })
}

shinyApp(ui=ui, server=server)
