library(truncnorm)

shinyServer(
  function(input, output, session)
  {
     observe(
      {
        updateSliderInput(session,"n_paired", max = round(floor(input$n_total/ 2)))

      }
    )

    priors = reactive(
      {
        d_n_sock = integer()
        if (input$total_prior == "pois")
        {
          d_n_sock = rpois(input$n_sims, input$total_lambda)
        } else {
          d_n_sock = rnbinom(input$n_sims,size = input$total_r, prob = input$total_p)
        }

        d_prop_pair = numeric()
        if (input$prop_prior == "beta")
        {
          d_prop_pair = rbeta(input$n_sims, input$prop_alpha, input$prop_beta)
        } else {
          d_prop_pair = rtruncnorm(input$n_sims,0,1,input$prop_mu,input$prop_sigma)
        }

        d_pair = integer()
        if (input$prop_prior == "beta")
        {
        d_pair = round(floor(d_n_sock / 2)*d_prop_pair)
        }else{
        d_pair = round(floor(d_n_sock / 2)*d_prop_pair)
        }

        d_odd = integer()
        if (input$prop_prior == "beta")
        {
        d_odd = d_n_sock - d_pair *2
        }else{
        d_odd = d_n_sock - d_pair *2
        }

        data.frame(total = d_n_sock, prop = d_prop_pair, n_pair = d_pair, n_odd = d_odd)
      }
    )

    sims = reactive (
      {
        gen_model = function(prior_n_socks,prior_prop_pairs, prior_pair, prior_odd)
        {
          #n_picked <- input$n_unique+2*input$n_paired

          # Simulating picking out n_picked socks
           socks <- rep(seq_len(prior_pair +  prior_odd), rep(c(2, 1),
                                          c(prior_pair, prior_odd)))

          picked_socks <- sample(socks, size =  min(input$n_total, prior_n_socks))

          sock_counts <- table(picked_socks)

          result=sum(sock_counts == 2)

#             c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
#           n_socks = prior_n_socks, n_pairs = prior_pair, n_odd = prior_odd,
#           prop_pairs = prior_prop_pairs)

          return(result)
        }
        apply(priors(),1, function(x) gen_model(x[1],x[2],x[3],x[4]))
      }
   )

    #pick out the rows from the dataframe result from prior() where "pairs" "variable"
    #equals the number of pairs user input, aka, n_paired
    #Later on the data frame drawn like this (with the same second column values)
    #is used to get the posterior distrbution for the four variables
    posterior = reactive(
      {
        #sock_sim=t(sims())
        post_sample = priors()[sims()==input$n_paired, ]
        post_sample
      }
    )

    output$all_prior = renderPlot(
      {
        par(mfrow=c(1,4),mar=c(15,3,4,1))
        #Prior for n_sock
        hist(priors()[,1],  main="Prior on n_socks",col="green",xlab="")
        #Prior for prop_pairs
        hist(priors()[,2],  main="Prior on prop_pairs",col="green",xlab="")
        #Prior for n_pairs
        hist(priors()[,3],  main="Resulting prior on n_pairs",col="green",xlab="")
        #Prior for n_odd
        hist(priors()[,4], main="Resulting prior on n_odd",col="green",xlab="")
      }
    )


    output$all_posterior = renderPlot(
      {
        cat("Post: ",dim(posterior()),"\n")

        par(mfrow=c(1,4),mar=c(15,3,4,1))
        #posterior for n_sock
        hist(posterior()[,1],  main="Posteior on n_socks",col="blue",xlab="")
        #posterior for prop_pairs
        hist(posterior()[,2], main="Posterior on prop_pairs",col="blue",xlab="")
        #posterior for n_pairs
        hist(posterior()[,3], main="Posterior on n_pairs",col="blue",xlab="")
        #posterior for n_odd
        hist(posterior()[,4], main="Posterior on n_odd",col="blue",xlab="")
      }
    )
  }
)