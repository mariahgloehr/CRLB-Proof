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
      #create four tabs, intro and then one for criteria
      tabPanel("Introduction",
               fluidRow(column(
                 6,
                 p(
                 "The Cramér-Rao Lower Bound, also referred to as the the Cramér-Rao inequality,
                 is a fundamental result of statistics. Its derivation is credited to 
                 Calyampudi Radhakrishna Rao, who was born in Hadagali, Karnataka State, India
                 on September 10, 1920. He obtained masters in both mathematics and statisitics in India
                 before completeing his PhD at Cambridge University in 1948, where the renowned R.A. Fisher
                 was his advisor."
               ),
               p(
                 "The Cramér-Rao Lower Bound states that the reciprocal of the Fisher information bounds
                 the variance of an unbiased estimator from below. Thus, if the variance of a given
                 unbiased estimator achieves this bound, it is the most efficient estimator possible.
                 Rao derived the lower bound in 1945."
               )),
               column(
                 6, img(src = "title_image.jpg", width = "100%"), p("Calyampudi Radhakrishna Rao, Credit Tejaswini Rao")
               ))),
      
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
    withMathJax( HTML('
      <p><strong>Theorem (CRLB):</strong> If \\(X_1, ..., X_n\\) are an iid sample from a distribution with pdf \\(f(X|\\theta)\\), then an unbiased estimator \\(\\hat{\\theta}\\) of \\(\\theta\\) satisfies:</p>
      $$V(\\hat{\\theta}) \\geq \\frac{1}{n \\cdot \\mathbb{E}\\left[\\left(\\frac{d}{d\\theta}\\ln{f(X|\\theta)}\\right)^2\\right]}$$

      <p><strong>Proof:</strong></p>
      <p> Let \\(\\hat{\\theta} = \\hat{\\theta}(X_1, ..., X_n)\\) be an unbiased estimator of \\(\\theta\\) and let 
      $$Z = \\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta}\\ln{f(X_i|\\theta)}.$$ </p>
      <p> We claim that $$\\mathbb{E}[Z] = 0.$$ </p>
      <p>We observe that since \\(\\int f(X_i|\\theta)dx_i = 1,\\) 
      $$\\frac{\\partial}{\\partial\\theta}\\int f(X_i|\\theta)dx_i = 0.$$ </p>
      
      <p> Given the identity 
      $$\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad
      \\frac{\\partial}{\\partial\\theta} \\ln f(x|\\theta) = \\frac{1}{f(x|\\theta)} \\cdot \\frac{\\partial}{\\partial\\theta} f(x|\\theta) 
      \\quad \\quad \\quad \\quad\\quad\\quad\\quad\\quad (1) $$</p>
      
      <p> which can be rearranged as 
      $$\\frac{\\partial}{\\partial\\theta}f(X_i|\\theta) = \\bigg[\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i|\\theta)} \\bigg]f(X_i|\\theta)$$  </p>
     
      <p> we then have 
      $$0 =  \\frac{\\partial}{\\partial\\theta}\\int f(X_i|\\theta)dx_i = \\int \\bigg[\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i|\\theta)} \\bigg]f(X_i|\\theta)dx_i $$
      using our assumption of interchanging differentiation and integration.</p>
      
      <p> Then,
      $$\\mathbb{E}[Z] = \\mathbb{E}\\bigg[ \\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta}\\ln{f(X_i|\\theta)}\\bigg]$$ </p>
      <p>
      $$\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad
      = \\sum_{i=1}^n \\mathbb{E}\\bigg[\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i|\\theta)}\\bigg]
      \\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad (2)$$ </p>
      <p>
      $$\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad 
      = \\sum_{i=1}^n \\int \\frac{\\partial}{\\partial\\theta}\\ln{f(X_i|\\theta)} \\cdot f(X_i|\\theta) dx_i 
      \\quad\\quad\\quad\\quad\\quad\\quad (3)$$ </p>
      $$ = \\sum_{i=1}^n 0 $$
      $$ = 0 $$ 
      as desired. </p>
      
      <p> We now claim that, 
      $$Var(Z) = n \\cdot \\mathbb{E}\\bigg[\\big(\\frac{\\partial}{\\partial\\theta}\\ln{f(X|\\theta)}  \\big)^2\\bigg]. $$ </p>
      
      <p> Since the  \\(X_i \\)s are iid, we start with 
      $$Var\\bigg[\\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)}\\bigg] = n \\cdot  Var\\bigg[\\frac{\\partial}{\\partial\\theta}\\ln{f(X; \\theta)}\\bigg]$$
      $$ \\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad
      = n\\bigg( \\mathbb{E}\\bigg[\\bigg(\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)} \\bigg)^2 \\bigg] - \\mathbb{E}\\bigg[\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)} \\bigg]^2 \\bigg)$$
      </p>
      
      <p> From (2) and (3), we know that 
      $$ \\mathbb{E}\\bigg[\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)} \\bigg]^2 = (0)^2 = 0 $$ 
      so it follows that 
      $$Var\\bigg[\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)}\\bigg] 
      = n \\cdot \\mathbb{E}\\bigg[\\bigg(\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)} \\bigg)^2 \\bigg]$$ 
      as desired. </p>
      
      <p>Given identity (1), we can rewrite \\(Z\\) as follows:
      $$Z = \\sum_{i=1}^n \\frac{\\frac{\\partial}{\\partial\\theta}f(X_i|\\theta)}{f(X_i|\\theta)}$$
      </p>
      
      <p> Using the Cauchy-Swartz property of covariance, we know that
      $$Var[\\hat{\\theta}]Var[Z] \\geq \\frac{\\text{Cov}^2[\\hat{\\theta}, Z]}{Var[Z]}$$
      $$\\quad\\quad\\quad Var[\\hat{\\theta}] \\geq \\frac{\\text{Cov}^2[\\hat{\\theta}, Z]}{Var[Z]}$$
      $$\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad
      Var[\\hat{\\theta}] \\geq \\frac{\\text{Cov}^2[\\hat{\\theta}, Z]}{n \\cdot \\mathbb{E}\\bigg[\\bigg(\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)} \\bigg)^2 \\bigg]}$$
      
      <p>The proof will be completed by showing that
      $$\\text{Cov}^2[\\hat{\\theta}, Z] = \\text{Cov}^2\\bigg[\\hat{\\theta}, \\sum_{i=1}^n \\frac{\\frac{\\partial}{\\partial\\theta}f(X_i|\\theta)}{f(X_i|\\theta)}\\bigg] = 1.$$
      </p>
      
      <p>
      We first expand the covariance as follows:
      $$\\text{Cov}[\\hat{\\theta}, Z] = \\mathbb{E}[\\hat{\\theta} \\cdot Z] - \\mathbb{E}[\\hat{\\theta}]\\mathbb{E}[Z] $$
      </p>
      
      <p>
      We know that 
      $$\\mathbb{E}[\\hat{\\theta}]\\mathbb{E}[Z] = 0$$
      so
      $$\\text{Cov}[\\hat{\\theta}, Z] = \\mathbb{E}[\\hat{\\theta} \\cdot Z]
      \\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad$$
      $$= \\mathbb{E}\\left[\\hat{\\theta} \\cdot \\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta} \\ln f(X_i; \\theta)\\right] 
      \\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad$$
      $$ \\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad
      = \\int ... \\int \\hat{\\theta}(x_1, ..., x_n) \\cdot \\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta} \\ln f(x_i; \\theta) \\cdot \\prod_{j=1}^n f(x_j; \\theta)\\, dx_1 \\cdots dx_j 
      \\quad\\quad (4)$$
      </p>
      
      <p>
      From the product rule, we know that
      $$ \\quad\\quad\\quad\\quad\\quad\\quad\\frac{\\partial}{\\partial\\theta}\\prod_{i=1}^n f(x_i;\\theta) =
          \\sum_{i=i}^n\\bigg(\\frac{\\partial}{\\partial\\theta}f(x_i;\\theta) \\cdot \\prod_{j\\neq i}^n f(x_j; \\theta)\\bigg) 
      \\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad (5)$$
      and from identity (1) we know that 
      $$\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad \\frac{\\partial}{\\partial\\theta} \\ln f(x_i; \\theta) \\cdot  f(x_i;\\theta) = \\frac{\\partial}{\\partial\\theta} f(x_i;\\theta) 
      \\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad\\quad (6)$$
      </p>
      
      <p> 
      Combining (5) and (6), we can then simplify (4) as follows:
      $$\\text{Cov}\\left[\\hat{\\theta}, \\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta}\\ln f(X_i; \\theta)\\right] 
        = \\int \\cdots \\int \\hat{\\theta}(x_1, \\cdots, x_n) \\cdot \\sum_{i=1}^n \\frac{\\partial}{\\partial\\theta} f(x_i;\\theta) \\prod_{i=1}^n f(x_i; \\theta)\\, dx_i $$
      $$ \\quad\\quad\\quad \\quad\\quad\\quad
      = \\int \\cdots \\int \\hat{\\theta}(x_1, \\cdots, x_n) \\cdot \\frac{\\partial}{\\partial\\theta} \\prod_{i=1}^n f(x_i; \\theta)\\, dx_i $$
      </p>
      
      <p>
      Now using our assumption of interchanging integration and differentiation, we find 
      $$ \\quad\\quad\\quad
      \\text{Cov}[\\hat{\\theta}, Z] 
        = \\int \\cdots \\int \\hat{\\theta}(x_1, ..., x_n) \\cdot \\frac{\\partial}{\\partial\\theta} \\prod_{i=1}^n f(x_i; \\theta)\\, dx_i$$
      $$ \\quad\\quad\\quad\\quad\\quad\\quad\\quad
      = \\frac{\\partial}{\\partial\\theta} \\int \\cdots \\int \\hat{\\theta}(x_1, ..., x_n) \\cdot \\prod_{i=1}^n f(x_i; \\theta)\\, dx_i $$
      $$ = \\frac{\\partial}{\\partial\\theta} \\mathbb{E}[\\hat{\\theta}] 
      \\quad\\quad\\quad\\quad\\quad\\quad$$
      </p>
      
      <p>
      Since \\(\\hat{\\theta}\\) is an unbiased estimator of \\(\\theta\\),
      $$\\frac{\\partial}{\\partial\\theta} \\mathbb{E}[\\hat{\\theta}] = 1$$
      </p>
      
      <p>
      So we have found that
      $$Var[\\hat{\\theta}] \\geq \\frac{\\text{Cov}^2[\\hat{\\theta}, Z]}{n \\cdot \\mathbb{E}\\bigg[\\bigg(\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)} \\bigg)^2 \\bigg]} $$
      $$\\quad\\quad\\quad
      \\geq \\frac{1^2}{n \\cdot \\mathbb{E}\\bigg[\\bigg(\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)} \\bigg)^2 \\bigg]} $$
      $$\\quad\\quad\\quad
      \\geq \\frac{1}{n \\cdot \\mathbb{E}\\bigg[\\bigg(\\frac{\\partial}{\\partial\\theta}\\ln{f(X_i; \\theta)} \\bigg)^2 \\bigg]} $$
      </p>
      
      <p>
      This concludes the proof.
      </p>

    ')
    )
  })
}

shinyApp(ui = ui, server = server)
