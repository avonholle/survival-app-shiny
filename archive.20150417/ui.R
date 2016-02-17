# ui.R

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Example of survival analysis with time interaction"),
    sidebarPanel(
      h2("Select parameters for simulation"),
      p("Parameters for Cox regression"),
      sliderInput("ns","Sample size:", min=100, max=1000, value=200, step=50, format="###", animate=FALSE),  
      sliderInput("beta1",
                  withMathJax(
                    helpText('\\( 
                             \\beta \\text{ coefficient for treatment } (\\beta_1)
                             \\)')
                  ),
                  min=0, max=3, value=0.5, step=0.1, format="#.#", animate=FALSE),
      sliderInput("beta1t", 
                  withMathJax(
                    helpText('\\( 
                             \\beta  \\text{coefficient for time interaction } (\\beta_{1t})
                             \\)')
                  ),
                  min=0.1, max=3, value=0.5, step=0.1, format="#.#", animate=FALSE),
      sliderInput("lambda", 
                  withMathJax(
                    helpText('\\( 
                             \\text{: rate}: } (\\lambda)
                             \\)')
                  ),
                  min=0, max=1, value=0.2, step=0.2, format="#.#", animate=FALSE),
      br()
    ),
  
    mainPanel(
        withMathJax(),
        
       h3("Simulation model without time interaction"),
       h3(uiOutput("eqn1")),
       br(),
       
       h3("Simulation model with time interaction"),
       h3(uiOutput("eqn2")),
       br(),
       
       h3("Link to simulation method"),
        wellPanel( 
          helpText(   a("Paper",
                        href="http://www.ncbi.nlm.nih.gov.libproxy.lib.unc.edu/pmc/articles/PMC3546387/"))),
       br(),
        

        h3("Histogram of time."),
        plotOutput("hist.1"),
        br(),
       
       # Left off here -- for some reason this isn't working..
       
        h3("Table of Cox regression results with no time interaction included"),
        tableOutput("s.1"),
        
       h3("Test of time interaction."),
       tableOutput("test.1"),
       br(),
       
        h3("Plot of beta over time."),
        plotOutput("plot.1"),
        br(),
       

       
       h3("Table of Cox regression results with time interaction included"),
       tableOutput("s.2"),
       br()
       
       
    )
    
    ))
