library(ggplot2)
library(survival)


shinyServer(function(input, output) {
  
  withMathJax()
  
# Parameters for function below..................
# ns: number of people in sample
# lambda: rate
# beta1: coefficient for x term
# beta1t: coefficient for interaction with time for x term

sim.dat.1 = function(ns, lambda, beta1, beta1t){
  
  n.1 = ns
  u.1 = runif(n.1) # random uniform numbers based on n.1 above
  lambda.1 = lambda
  beta.1 = beta1
  x.1 = runif(ns)
  
  lp.1 = beta.1*x.1 
  
  time.1 = -log(u.1) / (lambda.1*exp(lp.1))
  
  # see Appendix C. Continuous time-varying covariate: exponential distribution of event times
  # page 3957 of DOI: 10.1002/sim.5452
  
  # based on the following formula:
  # h(t|x(t)) = h_0(t) exp(beta_t*z(t) + beta*x)
  
  # with a single continuous time-varying covariate, z(t) = kt, which is proportional to t, the time variable.
  # x in this case is a fixed covariate
  # no piece-wise definition of the cumulative hazard function
  
  #k.1 = sample(c(1,2,3), rep=T, n.1, prob=c(1/3,1/3,1/3)) # say there are two different values of a dose of medicine, 1 or 2. SAmple from these doses with even prob
  #k.1=2
  k.1 = x.1 # this makes it a violation of the cox ph model assumption because the beta should be constant over time.
  beta.t.1 = beta1t
  
  time.2 = (1/(beta.t.1*k.1)) * log(1 + beta.t.1*k.1*(-log(u.1))/(lambda.1*exp(lp.1)))
  
  # Make censoring times based on an exponential distribution with same rate
  C <- rexp(n=n.1, rate=lambda.1)
  
  # follow-up times. See http://stats.stackexchange.com/questions/135124/how-to-create-a-toy-survival-time-to-event-data-with-right-censoring
  time.f <- pmin(time.2, C) # pmin function: Returns the (parallel) maxima and minima of the input values.
  # For min or max, a length-one vector. For pmin or pmax, a vector of length the longest of the input vectors, or length zero if one of the inputs had zero length.
  
  # event indicators
  status <- as.numeric(time.2 <= C)
  
  # make data frame with value for cox regression
  df.1 = data.frame(x.1, k.1, time.f, status)
  return(df.1)
}

# take parameters entered with the slider bar and use to simulate data
df.1 <- reactive({
  dat.1 = sim.dat.1(input$ns, input$lambda, input$beta1, input$beta1t)
  dat.1$id = seq(1:nrow(dat.1))
  dat.1$med.x.1 = cut(dat.1$x.1, 
                       breaks=c(quantile(dat.1$x.1, 
                                         probs = c(seq(0, 1, by=0.50)))), 
                       labels=c("h1", "h2"))
  return(dat.1)
})

# Plot histogram of times
# Make plot
output$hist.1 <- renderPlot({
 p.1 =  ggplot(data=df.1(), aes(x=time.f)) + geom_histogram() + theme_bw() +
    xlab("Time") + ylab("Count")
 return(p.1)
})

fit.1 <- reactive({
  # Run coxph with data having  time dependence -- but no time dependence in this model.
  coxph(data=df.1(), Surv(time.f, status) ~ x.1)
  })

fit.1.zph <- reactive({
  # Run coxph with time dependence
  fit.1.t = cox.zph(fit.1(), transform="identity") 
})

summary.1 <- reactive({
  summary(fit.1())
})

output$s.1 <- renderTable({
  summary.1()$coefficients
})

#fit.1.t = cox.zph(fit.1, transform=log) 
output$plot.1 <- renderPlot({
  return(plot(fit.1.zph()[1])) # plot for the time variant variable -- continuous.
})

output$test.1 <- renderTable({
  print(fit.1.zph()$table)
})


# based on info from 
# Tutorial: Survival Estimation for Cox Regression
# Models with Time-Varying Coecients Using SAS
# and R
survdat = reactive({
  cut.points = unique(df.1()$time[df.1()$status==1])

  surv2 = survSplit(data=df.1(), cut=cut.points,
                    end="time.f", start="time0", event="status")
#  surv2 = surv2[order(surv2$id),]
  colnames(surv2)[c(3:5,7)] = c( "time1", "event", "id", "time0")
  
  surv2$t.x.1 = surv2$x.1*surv2$time1
  surv2$lt.x.1 = surv2$x.1*log(surv2$time1)
  return(surv2)
})

fit.1a = reactive({  
  model1 = coxph(Surv(time0, time1, event) ~ x.1 , 
               data=survdat(),
               ties="efron") # NO interaction
})

fit.3 = reactive({  
  model3 = coxph(Surv(time0, time1, event) ~ x.1 + lt.x.1, 
               data=survdat(),
               ties="efron") # with interaction, log transform
})

output$s.2 <- renderTable({
  summary(fit.3())$coefficients
})

lrt.1 = reactive({
  lrt1 = anova(fit.1a(), fit.3())  
  rownames(lrt1)=c("Model, no int", 
                   "Model, With int")
  return(lrt1)
})

output$lrt.1 = renderTable({
  lrt.1()
})

# Kaplan-Meier plot
fit.4 = reactive({
  model4 = with(df.1(), survfit(Surv(time.f, status) ~ strata(med.x.1), conf.type="none"))
  return(model4)
  })

#fit.1.t = cox.zph(fit.1, transform=log) 
output$plot.2 <- renderPlot({
  cloglog2=function(y){
    log(-log(y))
  }
  
  par(mfrow=c(2,2))
  
  plot(fit.4(), lty=c(1,3), xlab="Time", ylab="Survival Probability", col=c("red", "blue"))
  legend(max(df.1()$time.f)-3, 0.9, c("Upper half x.1","Lower half x.1") , col=c("red", "blue"), lty=c(1,3) )
  
  plot(fit.4(), lty=c(1,3), xlab="Time", ylab="log(-log(y))", fun=cloglog2, col=c("red", "blue"))
  plot(fit.4(), lty=c(1,3), xlab="Time", ylab="Log cumulative hazard rate", fun='cumhaz', 
       col=c("red", "blue")) # see http://www.math.wustl.edu/~jmding/math434/R_model_diag.R
})


# Get confidence intervals for hazard ratio of x.1 at 0.2 compared to 0.6
# ..................................................................

coef.3 = reactive({
  coef.1 = summary(fit.3())$coefficients
})

cov.3 = reactive({
  cov.1 = vcov(fit.3())
})

output$coef.3 <- renderTable({
  coef.3()
})

output$cov.3.table = renderTable({
  cov.3()
})

output$c.1 = renderPrint({
  coef.3 = round(coef.3()[,1],5)
  rownames(coef.3) = NULL
  return(coef.3)
  })

contrast.diff = reactive({
  input$contrast.x1.1 - input$contrast.x1.2
})

output$cp.1 <- renderPrint({ round(c( contrast.diff(),
                                      log(input$contrast.time)*contrast.diff()),
                                   5) })

output$ce.1 = renderTable({
  ce.1 = c(contrast.diff(), log(input$contrast.time)*contrast.diff()) %*% coef.3()[,1]
  rownames(ce.1) = NULL
  return(ce.1)
})

output$var.3 = renderTable({
  c.1 = c(contrast.diff(), log(input$contrast.time)*contrast.diff())
  var.1 = sqrt(t(c.1) %*% cov.3() %*% c.1)
  return(var.1)
})

output$ci.3 = renderPrint({
  c.1 = c(contrast.diff(), log(as.numeric(input$contrast.time))*contrast.diff())
  ce.1 = c.1 %*% coef.3()[,1]
  var.1 = sqrt(t(c.1) %*% cov.3() %*% c.1)
  ci.95.1 = paste(round(exp(ce.1),3),
                  " (", 
                  round(exp(ce.1-1.96*var.1),3),
                  ", ", 
                  round(exp(ce.1+1.96*var.1),3),
                  ")", sep="")
  return(ci.95.1)
})



# Equations.....................................................

# see http://shiny.rstudio.com/gallery/mathjax.html
# have to be careful with font sizes.
output$eqn1 <- renderUI({
  withMathJax(
    helpText('\\( \\text{h}(t \\mid x_i) = \\text{h}_0(t) \\cdot \\text{exp(}\\beta_1 x_i) \\\\
                  \\text{---------------} \\\\
                  H_0(t) = \\int_0^t h_0(u)du \\\\
                  S(t \\mid x) = exp(-H_{0}(t) \\cdot exp(\\beta^{\\prime}x)) \\\\
                  F(t \\mid x) = 1 - exp(-H_{0}(t) \\cdot exp(\\beta^{\\prime}x)) \\\\
                  \\text{Distribution of event times is } \\sim F(t \\mid x) \\\\
                  \\text{T is survival time and t is time. } x_i \\text{ is } \\sim U(0,1).
             \\)')
  )
})

output$eqn2 <- renderUI({
  withMathJax(
    helpText('\\( \\text{h}(t \\mid x_i(t)) = \\text{h}_0(t) \\cdot \\text{exp(} \\beta_t z(t) + \\beta_1 x_i)\\\\
             z(t) = kt, \\text{ with } k>0  \\\\
             \\text{In this simulation } k = x_i \\text{ and } t=log(t).
             \\)')
  )
})

# derivation of hazard and cumulative hazard
output$eqn3 <- renderUI({
  withMathJax(
    helpText('\\( 
            S(t) = Pr(T>t) = \\int_t^{\\infty} f(u)du = 1-F(t) \\\\
            \\lambda(t) = lim_{dt \\rightarrow 0} \\displaystyle\\frac{Pr(t \\leq T < t+dt)}{dt \\cdot S(t)} = \\frac{f(t)}{S(t)} = -\\frac{S^{\\prime}(t)}{S(t)}
             \\)')
  )
})

output$text1 <- renderText({ 
  HTML(paste("x", tags$sub("t1"), "=", input$contrast.x1.1))
})

output$text2 <- renderText({ 
  HTML(paste("x", tags$sub("t2"), "=", input$contrast.x1.2))
})

output$text3 <- renderText({ 
  HTML(paste("time = ", input$contrast.time))
})

})
