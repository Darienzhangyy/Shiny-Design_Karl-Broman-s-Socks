#simple size
N <- 10000
#generate the sample 
sock_sim <- t(replicate(N, {
  #randomly generate n socks by negative binomial distribution
  n_socks <- rnbinom(1, mu = 30, size = -30^2/(30 - 15^2))
  #generate the probability fo paired socks by beta diatribution
  prop_pairs <- rbeta(1, shape1 = 15, shape2 = 2)
  #calculate number of paired socks
  n_pairs <- round(floor(n_socks/2)*prop_pairs)
  #number of odd socks
  n_odd <- n_socks - n_pairs*2
  #label paired an odd socks by number
  n_sock_types <- n_pairs + n_odd
  socks <- rep(seq_len(n_sock_types), rep( 2:1, c(n_pairs, n_odd)))
  #pick certain number of socks from sample
  picked_socks <- sample(socks, size = min(11, n_socks))
  sock_counts <- table(picked_socks)
  
  c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
    n_socks = n_socks, prop_pairs = prop_pairs)
  
}))

post_samples <- sock_sim[sock_sim[, "unique"] == 11&sock_sim[, "pairs"] == 0, ]

median(post_samples[,3])