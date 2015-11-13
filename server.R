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

        #About n_socks, the number of socks in Karl Broman’s laundry
        #must be positive and discrete.
        #It is reasonable to choose a Poisson distribution as a prior
        #since it is used widely for "count" data.
        #Furthermore, we can use the negative binomial prior.
        d_n_sock = integer()
        if (input$total_prior == "pois")
        {
          d_n_sock = rpois(input$n_sims, input$total_lambda)
        } else {
          size1 = -input$total_mu^2 / (input$total_mu - input$total_sd^2)
          d_n_sock = rnbinom(input$n_sims,mu = input$total_mu, size = size1)
        }

        #Instead of putting priors on n_paired and the resulting n_odd, we can put prior on
        #the proportion of paired socks.
        #Since proportion should be bigger than 0 and smaller than 1, we can use beta distribution
        #and truncated normal distribution (0 to 1)
        d_prop_pair = numeric()
        if (input$prop_prior == "beta")
        {
          d_prop_pair = rbeta(input$n_sims, input$prop_alpha, input$prop_beta)
        } else {
          d_prop_pair = rtruncnorm(input$n_sims,0,1,input$prop_mu,input$prop_sigma)
        }

        #With the prior on proportion, we can easily calculate the priors for n_pair
        #and n_odd. Thus we can get four prior distributions where these two are what
        #we most care about. By simulation afterward and picking out the rows with same
        #number of pairs as the user input, we can pickout the rows with these four parameters
        #and get the posterior distribution based on the picked rows.
        d_pair = integer()
        d_pair = round(floor(d_n_sock / 2)*d_prop_pair)

        d_odd = integer()
        d_odd = d_n_sock - d_pair *2

        data.frame(total = d_n_sock, prop = d_prop_pair, n_pair = d_pair, n_odd = d_odd)
      }
    )

    #The purpose of simulation is to create a column of simulated number of pairs,
    #which can be used to be compared with user-input "n_paired" to get the rows
    #for getting the posterior distribution
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

        #Creating a column of simulated number of pairs so as to be used later to
        #pick out the rows for posterior distribution
        apply(priors(),1, function(x) gen_model(x[1],x[2],x[3],x[4]))
      }
   )

    #pick out the rows from the dataframe result from prior() where "pairs" "variable"
    #equals the number of pairs user input, aka, n_paired
    #Later on the data frame drawn like this (with the same "pair" values)
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
        abline(v = median(priors()[,1]), lty = 2, col = "red", lwd=3)
        #Prior for prop_pairs
        hist(priors()[,2],  main="Prior on prop_pairs",col="green",xlab="")
        abline(v = median(priors()[,2]), lty = 2, col = "red", lwd=3)
        #Prior for n_pairs
        hist(priors()[,3],  main="Resulting prior on n_pairs",col="green",xlab="")
        abline(v = median(priors()[,3]), lty = 2, col = "red", lwd=3)
        #Prior for n_odd
        hist(priors()[,4], main="Resulting prior on n_odd",col="green",xlab="")
        abline(v = median(priors()[,4]), lty = 2, col = "red", lwd=3)
      }
    )


    output$all_posterior = renderPlot(
      {
        #Check the number of selected rows that construct the posterior distribution
        cat("Post: ",dim(posterior()),"\n")

        par(mfrow=c(1,4),mar=c(15,3,4,1))
        #posterior for n_sock
        hist(posterior()[,1],  main="Posterior on n_socks",col="blue",xlab="")
        abline(v = median(posterior()[,1]), lty = 2, col = "red",lwd=3)
        #posterior for prop_pairs
        hist(posterior()[,2], main="Posterior on prop_pairs",col="blue",xlab="")
        abline(v = median(posterior()[,2]), lty = 2, col = "red",lwd=3)
        #posterior for n_pairs
        hist(posterior()[,3], main="Posterior on n_pairs",col="blue",xlab="")
        abline(v = median(posterior()[,3]), lty = 2, col = "red",lwd=3)
        #posterior for n_odd
        hist(posterior()[,4], main="Posterior on n_odd",col="blue",xlab="")
        abline(v = median(posterior()[,4]), lty = 2, col = "red",lwd=3)
      }
    )


    output$postTable = renderTable(
      {
          dataFrame = data.frame("Medium" = c(round(median(posterior()[,1])),
                                              median(posterior()[,2]),
                                              round(median(posterior()[,3])),
                                              round(median(posterior()[,4]))),
                                 "95 Credible Interval Lower"=c(quantile(posterior()[,1],0.025), 
                                                                       quantile(posterior()[,2],0.025),
                                                                       quantile(posterior()[,3],0.025),
                                                                       quantile(posterior()[,4],0.025)),
                                 "95 Credible Interval Upper"=c(quantile(posterior()[,1],0.975), 
                                                                       quantile(posterior()[,2],0.975),
                                                                       quantile(posterior()[,3],0.975),
                                                                       quantile(posterior()[,4],0.975)))
          colnames(dataFrame)=c("Median", "95% Credible Interval Lower Bound", "95% Credible Interval Upper Bound")
          rownames(dataFrame) = c("Number of Socks", "Proportion of Pairs", "Number of Pairs",
                                  "Number of Odd")
          dataFrame
     }
)
    output$text1 <- renderText({paste("You have got", input$n_total,"socks.")})
    output$text2 <- renderText({paste(input$n_paired,"of them are paired.")})
    output$text3 <- renderText({paste("You can predict that there are",
                                round(median(posterior()[,1])), "socks in all.")})
    output$text4 <- renderText("The true number of total socks is 46 with 21 pairs and 3 sigletons.")
  }
)