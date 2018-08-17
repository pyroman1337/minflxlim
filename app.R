#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Minimal detectable flux from static chamber measurements"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("sims",
                     "Number of Simulations:",
                     min = 10,
                     max = 5000,
                     value = 100),
         h4("Please provide the input about your GHG chamber system"),
         
         numericInput(inputId="GC.sd", label=HTML("GC.sd [ppb]"), value = 5, width = 100),         
         helpText("Standard deviation of at least 50 close to atmospheric standard samples"),
         textInput(inputId="timing",label="sample timing [h]", value = "0, 0.25, 0.5, 0.75"),
         helpText("Enter the time of your samples comma separated",
                 "in the unit of your choice but prefereably hours",
                 "number of sample from 2 to n"),
         numericInput3(inputId="area", label=HTML("chamber area &emsp; [m<sup>2</sup>]"), value = 0.0491, width = 100),
         numericInput3(inputId="volume", label=HTML("chamber volume [m<sup>3</sup>]"), value = 0.00245, width = 100),
         
        radioButtons("gas.species", "GHG of your interest",
                     choices = c("Nitrous oxide" = "N2O",    # eval(c(expression("N"[2]*"O"))
                                 "Carbon dioxide" = "CO2",
                                 "Methane" = "CH4"),
                     selected = 'N2O')
         
      ),
      
      # Show a plot of the generated distribution
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
          )  ## maybe not "hahahah"
        )  
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   flxlim.sim <- reactive({
  
     # GC.sd = 3
     # t = seq(0, 0.5, length.out =  4)
     # V = 0.00245
     # A = 0.0491
     C0.ppb <- switch(input$gas.species,
                        "N2O" = 330,  # ppb
                        "CO2" = 400 , # ppm
                        "CH4" = 1800)# ppb
     
     species.mol <- switch(input$gas.species,
                      "N2O" = 44,  # g/mol
                      "CO2" = 44,  # g/mol
                      "CH4" = 14)  # g/mol
 
     C0   <- C0.ppb      * species.mol * 273.15 / 22.4 / (273.15 + 15) #mg / m^3 at 15°C for N2O/CH4
     sdGC <- input$GC.sd * species.mol * 273.15 / 22.4 / (273.15 + 15) #mg / m^3 
     # set.seed(42)
     t <- as.numeric(unlist(strsplit(input$timing,",")))
     

     
     ## hungary chambers: A = 0.0491, V =	0.00245
     
     sim <- data.frame(t = t, C = rnorm(length(t)*input$sims, mean = C0, sd = sdGC), 
                       # id = rep(1:1e3, each = 4), A = 0.0491, V = 0.00245)  # Hungary
                       id = rep(1:input$sims, each = length(t)), A = input$area, V = input$volume)  # Trier (mean)
     
     simflux <- gasfluxes(sim, .id = "id", .times = "t", methods = c("linear","HMR"), plot = FALSE) 
     simflux[, f0 := HMR.f0]
     simflux[is.na(f0), f0 := linear.f0]
     f.detect <<- simflux[, quantile(f0, 0.975)] #0
     # f.detect
     n.HMR <- simflux[!is.na(HMR.f0), .N] #
     flxlim.out <- list(f.detect = f.detect, n.HMR = n.HMR, simflux = simflux)   
     return(flxlim.out)
   })
  
   output$distPlot <- renderPlotly({
      # generate bins based on input$bins from ui.R
     simflux <- flxlim.sim()$simflux
     f.detect <- flxlim.sim()$f.detect
     flux.unit  <- switch(input$gas.species,
                          "N2O" = "[mg N<sub>2</sub>O m<sup>-2</sub> h<sup>-1</sub> ]", 
                          "CO2" = "[ g CO<sub>2</sub> m<sup>-2</sub> h<sup>-1</sub> ]", 
                          "CH4" = "[mg CH<sub>4</sub> m<sup>-2</sub> h<sup>-1</sub> ]")
    
     ggplotly(
       ggplot(simflux, aes_string(x = "id", y = "f0")) +
       geom_hline(yintercept = 0, col = "green") +   ylim(-2*f.detect,2*f.detect) +
       geom_point(size = 0.9) + geom_hline(yintercept = c(f.detect,-f.detect), col = "red") +
       ylab(paste("flux",flux.unit)) #+ xlab("ΔN<sub>2</sub>O [ppb]")  #expression(
       ,height = 450, dynamicTicks = T)  # )
     
   })
   
   output$value <- renderPrint({
     # flux.unit  <- switch(input$gas.species,
     #                      "N2O" = expression(paste("mg ",CO_2*m^2*h^2)), 
     #                      "CO2" = "[ g CO<sub>2</sub> m<sup>-2</sub> h<sup>-1</sub> ]", 
     #                      "CH4" = "[mg CH<sub>4</sub> m<sup>-2</sub> h<sup>-1</sub> ]")
     # 
     invisible(print("Estimated minimal detectable flux:"));
     print(flxlim.sim()$f.detect); # print(flux.unit);
     print("Number of HMR fluxes detected:");flxlim.sim()$n.HMR})
}

# Run the application 
shinyApp(ui = ui, server = server)

