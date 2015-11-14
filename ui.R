library(shiny)

total_priors = c('Negative Binomial'='nbinom', 'Poisson'='pois')
prop_priors = c('Beta'='beta', 'Truncated Normal'='tnorm')

shinyUI(
  fluidPage(
    titlePanel(
      "Broman's Socks: An Application of Approximate Bayesian Computation"
    ),
    sidebarPanel(
      numericInput('n_sims', h4('Simulations:'), value=100000, min=100, step=1),
      hr(),
      h4('Data:'),
      sliderInput('n_total', 'Total number of socks pulled:', min=0, max=30, value=11),
      sliderInput('n_paired', 'Number of sock pairs found:', min=0, max=15, value=0),
      hr(),
      h4('Priors:'),
      selectInput('total_prior', 'Prior for total number of socks:', total_priors),
      selectInput('prop_prior', 'Prior for paired proportion of socks:', prop_priors),
      hr(),
      h4('Hyperparameters:'),
      conditionalPanel(
        condition="input.total_prior == 'nbinom'",
        numericInput('total_mu', HTML('Total number (&mu;):'), value=30, min=1, max=120),
        numericInput('total_sd', HTML('Total number (&sigma;):'), value=15, min=0, max=1)
      ),
      conditionalPanel(
        condition="input.total_prior == 'pois'",
        sliderInput('total_lambda', HTML('Total number (&lambda;):'), value=44, min=11, max=120)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'beta'",
        numericInput('prop_alpha', HTML('Proportion prior (&alpha;):'), value=15 , min=0, max=NA),
        numericInput('prop_beta', HTML('Proportion prior (&beta;):'), value=2, min=0, max=NA)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'tnorm'",
        numericInput('prop_mu', HTML('Proportion prior (&mu;):'), value=0.9, min=0),
        numericInput('prop_sigma', HTML('Proportion prior (&sigma;):'), value=0.001, min=0)
      ),
      checkboxInput('showtruevalue', "Show me Broman's socks!")
    ),
    mainPanel(
      h4('Priors:'),
      plotOutput('all_prior'),
      br(),
      h4('Posteriors:'),
      plotOutput('all_posterior'),
      br(),
      h4('Posterior Statistics:'),
      tableOutput('postTable'),
      textOutput('text1'),
      textOutput('text2')
    )
  )
)
