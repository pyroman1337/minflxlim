###########################################################################
# Simulation of minimal detectable flux for static chamber measurements  ##
#                        a shiny web app                                  #
# by Roman Hüppi, August 2018, roman.hueppi@usys.ethz.ch                  #
###########################################################################

# load libraries ----------------------------------------------------------
library(shiny)
library(gasfluxes)
library(ggplot2)
library(plotly)

# smaller input boxes -----------------------------------------------------
numericInput3<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "number", value = value,...))
}

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Estimate the 'Minimal Detectable Flux' for static chamber measurements"),
  
  # Sidebar with a slider input for number of flux simulated
  sidebarLayout(
    sidebarPanel(
      sliderInput("sims",
                  "Number of Simulations:",
                  min = 10,
                  max = 5000,
                  value = 100),
      
      radioButtons("method", "Choose regression method",
                   choices = c("non-linear (HMR)" = "HMR",
                               "robust-linear" = "linear"),
                   selected = 'linear'),
      
      h4("Please provide the input about your GHG chamber system"),
      
      numericInput(inputId="GC.sd", label=HTML("GC.sd [ppb]"), value = 5, width = 100),         
      helpText("i.e. use the standard deviation of at least 50 standard concentration samples", 
               "close to the atmospheric concentration measured on your gas chromatograph"),
      textInput(inputId="timing",label="sample timing [h]", value = "0; 0.25; 0.5; 0.75"),
      helpText("Enter the time of your samples separated by semicolon",
               "in the unit of your choice but prefereably hours",
               "(number of samples supported from 4 to n)"),
      numericInput3(inputId="area", label=HTML("chamber area &emsp; [m<sup>2</sup>]"), value = 0.0491, width = 100),
      numericInput3(inputId="volume", label=HTML("chamber volume [m<sup>3</sup>]"), value = 0.00245, width = 100),
      
      radioButtons("gas.species", "GHG of your interest",
                   choices = c("Nitrous oxide" = "N2O",    # eval(c(expression("N"[2]*"O"))
                               "Carbon dioxide" = "CO2",
                               "Methane" = "CH4"),
                   selected = 'N2O'),
      actionButton("gasfluxes.go", "Simulate fluxes")
      
    ),
    
    # Show a plot of the generated fluxes
    mainPanel(
      tabsetPanel(
        tabPanel(p(icon("bar-chart"), "Simulation"),
                 
                 plotlyOutput("distPlot"),
                 br(),
                 br(),
                 br(),
                 verbatimTextOutput("value")
        ),
        tabPanel(p(icon("question-circle"), "Help"),
                 includeMarkdown("help.Rmd")
        ) 
      )  
    )  # end main panel
  ),
  img(src = "sae.png", height = 32, align = "left"),
  img(src = "ETH_logo.jpg", height = 70, align = "right")
)

# Define server logic
server <- function(input, output) {
  
  flxlim.sim <- reactive({
    
    input$gasfluxes.go # activation button
    
    C0.ppb <- switch(input$gas.species,
                     "N2O" = 330,     # ppb
                     "CO2" = 400000 , # ppb
                     "CH4" = 1800)    # ppb
    
    species.mol <- switch(input$gas.species,
                          "N2O" = 44,  # g/mol
                          "CO2" = 44,  # g/mol
                          "CH4" = 16)  # g/mol
    
    C0   <- C0.ppb / 1000      * species.mol * 273.15 / 22.4 / (273.15 + 15) #mg / m^3 at 15°C for N2O/CH4
    sdGC <- input$GC.sd / 1000 * species.mol * 273.15 / 22.4 / (273.15 + 15) #mg / m^3 
    set.seed(55)
    t <- as.numeric(unlist(strsplit(input$timing,";")))
    
    sim <- data.frame(t = t, C = abs(rnorm(length(t)*input$sims, mean = C0, sd = sdGC)), # simulate random fluxes with zero increase
                      id = rep(1:input$sims, each = length(t)), A = input$area, V = input$volume)  
    
    if(input$method == "HMR"){
      simflux <- gasfluxes(sim, .id = "id", .times = "t", verbose = FALSE,
                           methods = c("robust linear","HMR"), plot = FALSE) 
      simflux[, f0 := HMR.f0]  # use non-linear estimates
      simflux[is.na(f0), f0 := robust.linear.f0] # if HMR not available, use robust linear
      n.HMR <- simflux[!is.na(HMR.f0), .N] # count number of non-linear estimates
    } 
    
    if(input$method == "linear"){  #use robust linear estimates only
      simflux <- gasfluxes(sim, .id = "id", .times = "t", verbose = FALSE,
                           methods = c("robust linear"), plot = FALSE) 
      simflux[, f0 := robust.linear.f0]
      n.HMR <- 0
    } 
    
    f.detect <<- simflux[, quantile(f0, 0.975)] # detection limit is defined as 95 % inter quartile range
    # f.detect
    flxlim.out <- list(f.detect = f.detect, n.HMR = n.HMR, simflux = simflux)  
    # }, once = F)  # 
    
    return(flxlim.out)
    
  })
  
  
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    input$gasfluxes.go
    isolate({
      simflux <- flxlim.sim()$simflux  
      f.detect <- flxlim.sim()$f.detect
      flux.unit  <- switch(input$gas.species,
                           "N2O" = "[mg N<sub>2</sub>O m<sup>-2</sub> h<sup>-1</sub> ]", 
                           "CO2" = "[mg CO<sub>2</sub> m<sup>-2</sub> h<sup>-1</sub> ]", 
                           "CH4" = "[mg CH<sub>4</sub> m<sup>-2</sub> h<sup>-1</sub> ]")
      
      ggplotly(
        ggplot(simflux, aes_string(x = "id", y = "f0")) +
          geom_hline(yintercept = 0, col = "green") +  ylim(-3*f.detect,3*f.detect) + # sadly, the y limites dont work
          geom_point(size = 0.9) + geom_hline(yintercept = c(f.detect,-f.detect), col = "red") +
          ylab(paste("flux",flux.unit)) #+ xlab("ΔN<sub>2</sub>O [ppb]")  #expression(
        ,height = 450, dynamicTicks = T)  # )
      
    })
    
  })
  
  output$value <- renderPrint({
    input$gasfluxes.go
    isolate({
      
      flux.unit  <- switch(input$gas.species,
                           "N2O" = "[mg N2O/m^2/h ]", 
                           "CO2" = "[mg CO2/m^2/h ]", 
                           "CH4" = "[mg CH4/m^2/h ]")
      invisible(print("Estimated minimal detectable flux:"));
      print(flxlim.sim()$f.detect);print(flux.unit); # print(flux.unit);
      print("Number of HMR fluxes detected:");flxlim.sim()$n.HMR})
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

