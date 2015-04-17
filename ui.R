# ui.R

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Example of survival analysis with time interaction"),
    sidebarPanel(
      h2("Select parameters for simulation"),
      p("Parameters for Cox regression"),
      sliderInput("ns","Sample size:", min=100, max=1000, value=200, step=100, format="###", animate=FALSE),  
      sliderInput("beta1",
                  withMathJax(
                    helpText('\\( 
                             \\beta \\text{ coefficient for treatment } (\\beta_1)
                             \\)')
                  ),
                  min=0.1, max=3, value=0.5, step=0.5, format="#.#", animate=FALSE),
      sliderInput("beta1t", 
                  withMathJax(
                    helpText('\\( 
                             \\beta  \\text{ coefficient for time interaction } (\\beta_{1t})
                             \\)')
                  ),
                  min=0.1, max=3, value=0.5, step=0.1, format="#.#", animate=FALSE),
      sliderInput("lambda", 
                  withMathJax(
                    helpText('\\( 
                             \\text{ Rate of events: } (\\lambda)
                             \\)')
                  ),
                  min=0.1, max=1, value=0.2, step=0.2, format="#.#", animate=FALSE),
      br()
    ),
  
    mainPanel(
      tabsetPanel(selected="Background",
        withMathJax(),
        
        tabPanel("Background",
        h5("Simulation model without time interaction"),
         uiOutput("eqn1"),
         br(),
         
        h5("Simulation model with time interaction"),
        uiOutput("eqn2"),
         br(),
         
         h5("Link to simulation method"),
          wellPanel( 
            helpText(   a("DOI 10.1002/sim.5452",
                          href="http://www.ncbi.nlm.nih.gov.libproxy.lib.unc.edu/pmc/articles/PMC3546387/")),
            helpText(    a("PMID 15724232",
                          href="http://www.ncbi.nlm.nih.gov.libproxy.lib.unc.edu/pubmed/15724232"))
            ),
         br(),
        
        h5("Extra background info on hazard and relation with cumulative hazard"),
        uiOutput("eqn3"),
        br()
         ),
        
        tabPanel("Regression results",
         h5("Table of Cox regression coefficients, no time interaction included"),
          tableOutput("s.1"),
         br(),
          
          
         h5("Table of Cox regression coefficients, time interaction included"),
          tableOutput("s.2"),
          br(),
          
         h5("Test of time interaction."),
         tableOutput("test.1"),
         br(),
         
         h5("Likelihood ratio test of model with interaction vs without."),
         tableOutput("lrt.1"),
         br()
         ),
        
        tabPanel("Plots",
         
         h5("Histogram of time to event."),
         plotOutput("hist.1"),
         br(),
         
         h5("Plot of beta over time."),
          plotOutput("plot.1"),
          br(),
         
         h5("Kaplan-Meier plots with x.1 covariate groups divided by median"),
         plotOutput("plot.2"),
         br()
         )         
        )
      )
  )
  )
