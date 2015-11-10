library(truncnorm)

shinyServer(
  function(input, output, session) 
  {
    observe(
      {
        updateSliderInput(session,"n_paired", max = input$n_unique)
        
      }
    )
    
    priors = reactive(
      {
        d_total = numeric()
        if (input$total_prior == "pois")
        {
          d_total = rpois(input$n_sims, input$total_lambda)  
        } else {
          d_total = rnbinom(input$n_sims,size = input$total_r, prob = input$total_p)
        }
        
        d_prop = numeric()
        if (input$prop_prior == "beta")
        {
          d_prop = rbeta(input$n_sims, input$prop_alpha, input$prop_beta)  
        } else {
          d_prop = rtruncnorm(input$n_sims,0,1,input$prop_mu,input$prop_sigma)
        }
        
        data.frame(total = d_total, prop = d_prop)
      }
    )
    
    sims = reactive (
      {
        gen_model = function(prior_n_socks,prior_prop_pairs)
        {
            
            
            n_pairs <- round(floor(prior_n_socks / 2)*prior_prop_pairs)
            n_odd <- prior_n_socks - n_pairs *2
          # Simulating picking out n_picked socks
           socks <- rep(seq_len(n_pairs + n_odd), rep(c(2, 1), c(n_pairs, n_odd)))
          picked_socks <- sample(socks, size =  min(input$n_paired, input$n_unique))
          sock_counts <- table(picked_socks)
          result=c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
          n_socks = n_socks, n_pairs = n_pairs, n_odd = n_odd, prop_pairs = prop_pairs)
          return(result)
          
        }
    
        apply(priors(),1, function(x) gen_model(x[1],x[2]))  
          
      }
        
       
      
   )
    
    posterior = reactive(
      {
        priors()[sims()==input$n_paired,]    
      }
    )
    
    output$total_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,1], freq=FALSE, main="Total Socks")
        lines(density( priors()$unique ),col='green',lwd=3)
      }
    )
    
    output$prop_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,2], freq=FALSE, main="Proportion of Black Jelly Beans")
        lines(density( priors()$prop ),col='blue',lwd=3)
      }
    )
  }
)