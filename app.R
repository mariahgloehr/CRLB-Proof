#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #allow latex
    withMathJax(),
    # Application title
    titlePanel("Proof of the Cramer-Rao Lower Bound"),
    
    tabsetPanel(
      #create four tabs, intro and then one for critieria
      tabPanel("Introduction"),
      
      tabPanel("Proof",
               uiOutput("proof")),

      tabPanel("Simulation",
               
               inputPanel(
                 #choose RV distribution
                 radioButtons("dist",
                             "Distribution",
                             choices = c("Normal", "Exponential", "Poisson")),
                #choose parameter value
                textInput(
                   "parameter",
                   "Parameter Value",
                   value = 1
                 )
                 ),
               #plot histogram
               plotOutput("histogram")
               ),

      tabPanel("Real Data")
      
    )
)

# Define server logic
server <- function(input, output) {
  
  
  output$proof <- renderUI({
    HTML('
      <p><strong>Theorem (CRLB):</strong> If \\(X_1, ..., X_n\\) are an iid sample from a distribution with pdf \\(f(X|\\theta)\\), then an unbiased estimator \\(\\hat{\\theta}\\) of \\(\\theta\\) satisfies:</p>
      $$V(\\hat{\\theta}) \\geq \\frac{1}{n \\cdot \\mathbb{E}\\left[\\left(\\frac{d}{d\\theta}\\ln{f(X|\\theta)}\\right)^2\\right]}$$

      <p><strong>Proof:</strong></p>
      <p>Let \\(Y = \\hat{\\theta}\\), \\(Z = \\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)}\\) be random variables.</p>

      <p>Then \\(\\mathbb{E}[Z] = \\mathbb{E}\\left[\\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)}\\right] = 0\\)</p>

      <p>Using the Cauchy–Schwarz inequality:</p>
      $$\\mathbb{V}[\\hat{\\theta}] \\geq \\frac{\\left(\\text{Cov}\\left[\\hat{\\theta}, \\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta} \\ln f(X_i; \\theta)\\right]\\right)^2}{\\mathbb{V}\\left[\\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta} \\ln f(X_i; \\theta)\\right]}$$

      <p>To evaluate the numerator:</p>
      <ul>
        <li>\\( \\text{Cov}\\left[\\hat{\\theta}, \\sum \\frac{\\partial}{\\partial\\theta}\\ln f(X_i;\\theta)\\right] = \\mathbb{E}\\left[\\hat{\\theta} \\cdot \\sum \\frac{\\partial}{\\partial\\theta}\\ln f(X_i;\\theta)\\right] - \\mathbb{E}[\\hat{\\theta}]\\cdot \\mathbb{E}\\left[\\sum \\frac{\\partial}{\\partial\\theta}\\ln f(X_i;\\theta)\\right] \\)</li>
        <li>Since \\(\\mathbb{E}[Z] = 0\\), the second term vanishes.</li>
      </ul>

      <p>So we get:</p>
      $$\\text{Cov}\\left[\\hat{\\theta}, Z\\right] = \\frac{\\partial}{\\partial\\theta} \\mathbb{E}[\\hat{\\theta}]$$

      <p>Because \\(\\hat{\\theta}\\) is unbiased, this equals 1. So the inequality becomes:</p>
      $$\\mathbb{V}[\\hat{\\theta}] \\geq \\frac{1}{\\mathbb{V}\\left[\\sum \\frac{\\partial}{\\partial\\theta}\\ln f(X_i; \\theta)\\right]}$$

      <p>We analyze the denominator. Since the \\(X_i\\) are iid:</p>
      $$\\mathbb{V}\\left[\\sum \\frac{\\partial}{\\partial\\theta}\\ln f(X_i; \\theta)\\right] = n \\cdot \\mathbb{V}\\left[\\frac{\\partial}{\\partial\\theta} \\ln f(X; \\theta)\\right]$$

      <p>And thus:</p>
      $$\\mathbb{V}[\\hat{\\theta}] \\geq \\frac{1}{n \\cdot \\mathbb{E}\\left[\\left(\\frac{\\partial}{\\partial\\theta} \\ln f(X; \\theta)\\right)^2\\right]}$$

      <p><strong>Q.E.D.</strong></p>
    ')
  })
}

shinyApp(ui = ui, server = server)
