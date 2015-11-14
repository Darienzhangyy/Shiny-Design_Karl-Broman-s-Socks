require(truncnorm)
require(parallel)
require(ggplot2)
require(gridExtra)

shinyServer(
  function(input, output, session) {
    observe(
      {
        updateSliderInput(session, 'n_paired', max=round(floor(input$n_total/2)))
      }
    )

    priors = reactive(
      {
        # Generate sock totals given user-specified priors constrained to discrete, positive values,
        # with speed enhanced by parallel processing in 10 batches.
        d_n_sock = integer()
        if(input$total_prior=='pois') {
          d_n_sock = unlist(mclapply(1:10, function(x) {
              rpois(input$n_sims/10, input$total_lambda)
            }, mc.cores=24))
        } else {
          size1 = -input$total_mu^2 / (input$total_mu - input$total_sd^2)
          d_n_sock = unlist(mclapply(1:10, function(x) {
              rnbinom(input$n_sims/10, mu=input$total_mu, size=size1)
            }, mc.cores=24))
        }

        # Generate pair proportions given user-specified priors constrained to fall on [0,1], with
        # speed enhanced by parallel processing in 10 batches.
        d_prop_pair = numeric()
        if(input$prop_prior=='beta') {
          d_prop_pair = unlist(mclapply(1:10, function(x) {
              rbeta(input$n_sims/10, input$prop_alpha, input$prop_beta) 
            }, mc.cores=24))
        } else {
          d_prop_pair = unlist(mclapply(1:10, function(x) {
              rtruncnorm(input$n_sims/10, 0, 1, input$prop_mu, input$prop_sigma)
            }, mc.cores=24))
        }

        # Given priors on the proportion of paired socks, `d_prop_pair`, and the total number of 
        # socks, `d_n_sock`, we can easily derive priors for the number of pairs, `n_pair`, and 
        # the number of singletons, `n_odd`.
        d_pair = integer()
        d_pair = round(floor(d_n_sock / 2) * d_prop_pair)
        d_odd = integer()
        d_odd = d_n_sock - d_pair * 2

        # Combine these priors into a data frame.
        data.frame(total=d_n_sock, prop=d_prop_pair, n_pair=d_pair, n_odd=d_odd)
      }
    )

    # Simulate the sock-picking process under user-specified priors for later comparison.
    sims = reactive (
      {
        # Simulate the sock-picking process and return the number of pairs.
        gen_model = function(prior_n_socks, prior_prop_pairs, prior_pair, prior_odd) {
          socks = rep(seq_len(prior_pair + prior_odd), 
                      rep(c(2, 1), c(prior_pair, prior_odd)))
          picked_socks = sample(socks, size=min(input$n_total, prior_n_socks))
          sock_counts = table(picked_socks)
          return(sum(sock_counts==2))
        }

        # Separate the data into 8 subsets for parallel processing via mclapply.
        n_chunks = 8
        steps = floor(seq(1, input$n_sims, len=n_chunks+1))
        l = list()
        l[[1]] = priors()[steps[1]:steps[2],]
        for(i in 2:n_chunks) {
          l[[i]] = priors()[(steps[i]+1):steps[i+1],]
        }

        # Run the data, broken into subsets, through the generative model in parallel.
        unlist(mclapply(l, function(x) apply(x,1, function(x) gen_model(x[1],x[2],x[3],x[4])), mc.cores = 8))
      }
    )

    # Match the simulated sock data to the user-specified number of found pairs.
    posterior = reactive(
      {
        priors()[sims()==input$n_paired,]
      }
    )

    # Create a data frame containing the 95% CI and median for all priors.
    pp = reactive(
      {
        p = as.data.frame(apply(priors(), 2, quantile, c(0.025, 0.5, 0.975)))
        colnames(p) = c('V1', 'V2', 'V3', 'V4')
        p
      }
    )
    
    # Plot all prior distributions, with lines marking the 95% CI and median values.
    output$all_prior = renderPlot(
      {
        p1 = ggplot(priors(), aes(x=total)) +
          geom_histogram(fill='#00BA38') + 
          labs(x=NULL, y=NULL, title='Total number of socks') +
          geom_vline(data=pp(), aes(xintercept=V1), linetype=c(3,5,3))
        p2 = ggplot(priors(), aes(x=prop)) +
          geom_histogram(fill='#00BA38') +
          labs(x=NULL, y=NULL, title='Paired proportion of socks') +
          geom_vline(data=pp(), aes(xintercept=V2), linetype=c(3,5,3))
        p3 = ggplot(priors(), aes(x=n_pair)) +
          geom_histogram(fill='#00BA38') +
          labs(x=NULL, y=NULL, title='Number of sock pairs') +
          geom_vline(data=pp(), aes(xintercept=V3), linetype=c(3,5,3))
        p4 = ggplot(priors(), aes(x=n_odd)) +
          geom_histogram(fill='#00BA38') +
          labs(x=NULL, y=NULL, title='Number of singletons') +
          geom_vline(data=pp(), aes(xintercept=V4), linetype=c(3,5,3))
        p_all = grid.arrange(p1, p2, p3, p4, ncol=2)
        print(p_all)
      }
    )
    
    # Create a data frame containing the 95% CI and median for all posteriors.
    qq = reactive(
      {
        q = as.data.frame(apply(posterior(), 2, quantile, c(0.025, 0.5, 0.975)))
        colnames(q) = c('V1', 'V2', 'V3', 'V4')
        q
      }
    )
    
    # Plot all posterior distributions, with lines marking the 95% CI and median values.
    output$all_posterior = renderPlot(
      {
        q1 = ggplot(posterior(), aes(x=total)) +
          geom_histogram(fill='#619CFF') + 
          labs(x=NULL, y=NULL, title='Total number of socks') +
          geom_vline(data=qq(), aes(xintercept=V1), linetype=c(3,5,3))
        q2 = ggplot(posterior(), aes(x=prop)) +
          geom_histogram(fill='#619CFF') + 
          labs(x=NULL, y=NULL, title='Paired proportion of socks') +
          geom_vline(data=qq(), aes(xintercept=V2), linetype=c(3,5,3))
        q3 = ggplot(posterior(), aes(x=n_pair)) +
          geom_histogram(fill='#619CFF') + 
          labs(x=NULL, y=NULL, title='Number of sock pairs') +
          geom_vline(data=qq(), aes(xintercept=V3), linetype=c(3,5,3))
        q4 = ggplot(posterior(), aes(x=n_odd)) +
          geom_histogram(fill='#619CFF') + 
          labs(x=NULL, y=NULL, title='Number of singletons') +
          geom_vline(data=qq(), aes(xintercept=V4), linetype=c(3,5,3))
        q_all = grid.arrange(q1, q2, q3, q4, ncol=2)
        print(q_all)
      }
    )
    
    # Summarize ABC output in tabular format.
    output$postTable = renderTable(
      {
        df = data.frame(low=apply(posterior(), 2, quantile, 0.025),
                        med=c(as.integer(median(posterior()[,1])),
                              median(posterior()[,2]),
                              as.integer(median(posterior()[,3])),
                              as.integer(median(posterior()[,4]))),
                        high=apply(posterior(), 2, quantile, 0.975))
        colnames(df) = c('95% Credible Interval Lower Bound', 'Median', 
                         '95% Credible Interval Upper Bound')
        rownames(df) = c('Total Number of Socks', 'Paired Proportion', 'Number of Pairs',
                         'Number of Singletons')
        df
      }, 
      align='cccc'
    )
    
    # Summarize ABC output in verbal form.
    output$text1 = renderText(
      {
        paste('After pulling', input$n_total, "socks out of the dryer, you've found", input$n_paired, 
              'pairs. Your best guess is', round(median(posterior()[,1])), 'socks in total.')
      }
    )
    
    # Reveal the outcome from our motivating example.
    output$text2 = renderText(
      {
        if(input$showtruevalue==T) {
          'There were 21 pairs and 3 singletons, for a total of 45 socks.'
        } else {
          "(Check the box on the side panel in order to reveal how many socks were in Broman's laundry.)"
        }
      }
    )
    
  }
)