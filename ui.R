library(shiny)

total_priors = c("Negative Binomial" = "nbinom", "Poisson" = "pois")
prop_priors = c("Beta" = "beta", "Truncated Normal"="tnorm")

shinyUI(
  fluidPage(
    titlePanel(
      "Socks"
    ),
    sidebarPanel(
      numericInput("n_sims", h4("Simulations:"),value = 100000, min = 100, step = 1),
      hr(),
      h4("Data:"),
      sliderInput("n_total", "Total Socks:", min=0, max=30, value=11),
      sliderInput("n_paired", "Paired Socks:", min=0, max=30, value=0),
      hr(),
      h4("Priors:"),
      selectInput("total_prior", "Prior for total", total_priors),
      selectInput("prop_prior", "Prior for proportion", prop_priors),
      hr(),
      h4("Hyperparameters:"),
      conditionalPanel(
        condition="input.total_prior == 'nbinom'",
        numericInput("total_mu",HTML("Total prior - mu"), value = 30, min=1, max=120),
        numericInput("total_sd",HTML("Total prior - sd"), value = 15, min=0, max=1)
      ),
      conditionalPanel(
        condition="input.total_prior == 'pois'",
        sliderInput("total_lambda",HTML("Total prior - &lambda;"), value = 5, min=1, max=120)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'beta'",
        numericInput("prop_alpha",HTML("Total prior - &alpha;"), value = 15 , min=0, max=NA),
        numericInput("prop_beta",HTML("Total prior - &beta;"), value = 2, min=0, max=NA)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'tnorm'",
        numericInput("prop_mu",HTML("Proportion prior - &mu;"), value = 0.5, min=0),
        numericInput("prop_sigma",HTML("Proportion prior - &sigma;"), value = 0.1, min=0)
      )
    ),
    mainPanel(
      h4("Priors:"),
      plotOutput("all_prior"),
      br(),
      h4("Posteriors:"),
      plotOutput("all_posterior"),
      br(),
      h4("Posterior Statistics:"),
      tableOutput("postTable"),
      textOutput("text1"),
      textOutput("text2"),
      textOutput("text3")
    )
  )
)
