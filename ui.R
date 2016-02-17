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
                             \\beta  \\text{ coefficient for time interaction } (\\beta_{t})
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
      br(),
      
      h4("Contrast parameters"),
      tags$hr(),
      sliderInput("contrast.x1.1", 
                  withMathJax(
                    helpText('\\( 
                             \\text{ First value for x.1 in contrast} 
                             \\)')
                  ),
                  min=0.01, max=1, value=0.6, step=0.2, format="#.#", animate=FALSE),
      br(),
      sliderInput("contrast.x1.2", 
                  withMathJax(
                    helpText('\\( 
                             \\text{ Second value for x.1 in contrast} 
                             \\)')
                  ),
                  min=0.01, max=1, value=0.2, step=0.2, format="#.#", animate=FALSE),
      br(),
      sliderInput("contrast.time", 
                  withMathJax(
                    helpText('\\( 
                             \\text{ Time for contrast} 
                             \\)')
                  ),
                  min=1, max=10, value=4, step=1, format="#.#", animate=FALSE),
      br()
    ),
  
    mainPanel(
      tabsetPanel(selected="Plots",
        withMathJax(),
        tabPanel("Background",
        h5("Simulation Cox regression model without time interaction"),
         uiOutput("eqn1"),
         br(),
         
        h5("Simulation Cox regression model with time interaction"),
        uiOutput("eqn2"),
         br(),
         
         h5("Link to simulation method"),
          wellPanel( 
            helpText(   a("DOI 10.1002/sim.5452",
                          href="http://www.ncbi.nlm.nih.gov.libproxy.lib.unc.edu/pmc/articles/PMC3546387/")),
            helpText(    a("PMID 15724232",
                          href="http://www.ncbi.nlm.nih.gov.libproxy.lib.unc.edu/pubmed/15724232")),
            helpText(   a("Link to programs to make the Shiny app",
                          href="https://github.com/avonholle/survival-app-shiny"))
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
         ),
        
        tabPanel("95 percent CI calcs for a contrast",
                   h5("Coefficients (c.1) from model with time interaction"),
                   tableOutput("coef.3"),
                   br(),
                   
                   h5("Covariance (cov.1) from model with time interaction"),
                   tableOutput("cov.3.table"),
                   br(),
                   
                   h5("The vector of estimated paramters, c.1"),
                   uiOutput("c.1"),
                   br(),
                   
                   # get hr
                   # .......................................
                   # for x.1 = 0.2 at time=2, h_1 = h_0 * exp( \\beta_1 * 0.2 + \\beta_{1t} * 0.2 * log(2))
                   # for x.1 = 0.6 at time=2, h_2 = h_0 * exp( \\beta_1 * 0.6 + \\beta_{1t} * 0.6 * log(2))
                   # (h_1/h_0) / (h_2/h_0) = exp( \\beta_1 * 0.2 + \\beta_{1t} * 0.2 * log(2) - \\beta_1 * 0.6 - \\beta_{1t} * 0.6 * log(2))
                   # h_1 / h_2 = exp( \\beta_1*(0.2-0.6) + \\beta+{1t}*log(2)*(0.2-0.6) )
            
                 h4("Contrast parameters"),
                 tags$hr(),
                 
                 
                 h5("Planned contrast, cp.1: Contrast hazard at two different levels of x.1 at time t"),
                 withMathJax(
                   helpText('\\( 
                             h_1 = h_0 \\cdot exp(\\beta_1 \\cdot x_{t1} + \\beta_{1t} \\cdot x_{t1} \\cdot log(time)) \\\\
                             h_2 = h_0 \\cdot exp(\\beta_1 \\cdot x_{t2} + \\beta_{1t} \\cdot x_{t2} \\cdot log(time)) \\\\
                              \\displaystyle\\frac{\\frac{h_1}{h_0} } { \\frac{h_2}{h_0} } = exp(\\beta_1 \\cdot x_{t1} + \\beta_{1t} \\cdot x_{t1} \\cdot log(time) - \\beta_1 \\cdot x_{t2} - \\beta_{1t} \\cdot x_{t2} \\cdot log(time)) \\\\
                              \\frac{h_1}{h_2} = exp(\\beta_1 \\cdot (x_{t1} - x_{t2}) + \\beta_{1t} \\cdot log(time) \\cdot (x_{t1}-x_{t2})) \\\\
                              \\rightarrow = (\\beta_1, \\beta_{1t})^{\\prime} \\cdot (x_{t1}-x_{t2}, log(time) \\cdot (x_{t1}-x_{t2})) \\\\
                              \\rightarrow = (\\beta_1 , \\beta_{1t})^{\\prime} \\cdot cp.1 \\\\
                              \\rightarrow cp.1 = ( (x_{t1}-x_{t2}), \\text{ }log(time) \\cdot (x_{t1}-x_{t2}))
                            \\)')
                 ),
                uiOutput("text1"),
                uiOutput("text2"),
                uiOutput("text3"),
                
                 HTML("cp.1="),
                uiOutput("cp.1"),
                 br(),
                 
                 h5("Estimated sd for contrast/HR = sqrt(var.1)"),
                 withMathJax(
                   helpText('$$C^{\\prime} \\cdot \\Sigma \\cdot C \\rightarrow \\\\
                             cp.1^{\\prime} \\cdot cov.1 \\cdot cp.1$$')
                 ),
                 uiOutput("var.3"),
                 br(),

                 h5("Multiply specified contrast times vector of parameters = estimated HR (ce.1)"),
                 withMathJax(
                   helpText('\\( 
                             ce.1 = cp.1^{\\prime} \\cdot c.1
                             \\)')
                 ),
                 uiOutput("ce.1"),
                 br(),
                 
                 h5("Get 95 percent ci from var.1 and ce.1 "),
                 withMathJax(
                   helpText('\\( 
                             exp(ce.1 \\pm 1.96 \\cdot sqrt(var.1))
                             \\)')
                 ),
                 uiOutput("ci.3"),
                 br()
        )
        )
      )
  )
  )
