library(truncnorm)

shinyServer(
  function(input, output, session)
  {
#     observe(
#       {
#         updateSliderInput(session,"n_paired", max = input$n_unique)
#
#       }
#     )

    priors = reactive(
      {
        d_n_sock = numeric()
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

        data.frame(total = d_n_sock, prop = d_prop_pair)
      }
    )

    sims = reactive (
      {
        gen_model = function(prior_n_socks,prior_prop_pairs)
        {
          n_picked <- 11

            n_pairs <- round(floor(prior_n_socks / 2)*prior_prop_pairs)
            n_odd <- prior_n_socks - prior_prop_pairs *2
          # Simulating picking out n_picked socks
           socks <- rep(seq_len(n_pairs + n_odd), rep(c(2, 1), c(n_pairs, n_odd)))
          picked_socks <- sample(socks, size =  min(n_picked, prior_n_socks))
          sock_counts <- table(picked_socks)
          result=c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
          n_socks = prior_n_socks, n_pairs = n_pairs, n_odd = n_odd,
          prop_pairs = prior_prop_pairs)
          return(result)

        }

        apply(priors(),1, function(x) gen_model(x[1],x[2]))

      }



   )

    #pick out the rows from the dataframe result from prior() where "pairs" "variable"
    #equals the number of pairs user input, aka, n_paired
    #Later on the data frame drawn like this (with the same second column values)
    #is used to get the posterior distrbution for the four variables
    posterior = reactive(
      {
        sock_sim=t(sims())
        post_sample = sock_sim[sock_sim[,2]==input$n_paired &
                               sock_sim[,1]==input$n_unique, ]
        post_sample
      }
    )

    output$all_prior = renderPlot(
      {
        par(mfrow=c(1,4))
        #Prior for n_sock
        hist(t(sims())[,3], freq=FALSE, main="Prior on n_socks",col="green")
        #Prior for prop_pairs
        hist(t(sims())[,6], freq=FALSE, main="Prior on prop_pairs",col="green")
        #Prior for n_pairs
        hist(t(sims())[,4], freq=FALSE, main="Resulting prior on n_pairs",col="green")
        #Prior for n_odd
        hist(t(sims())[,5], freq=FALSE, main="Resulting prior on n_odd",col="green")
      }
    )


    output$all_posterior = renderPlot(
      {
        par(mfrow=c(1,4))
        #posterior for n_sock
        hist(posterior()[,3], freq=FALSE, main="Posteior on n_socks",col="blue")
        #posterior for prop_pairs
        hist(posterior()[,6], freq=FALSE, main="Posterior on prop_pairs",col="blue")
        #posterior for n_pairs
        hist(posterior()[,4], freq=FALSE, main="Posterior on n_pairs",col="blue")
        #posterior for n_odd
        hist(posterior()[,5], freq=FALSE, main="Posterior on n_odd",col="blue")
      }
    )
  }
)