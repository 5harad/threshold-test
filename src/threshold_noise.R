#!/usr/bin/env Rscript
library(boot)
source('run_mcmc.R')

beta_ccdf <- function(x, a, b) {
  return(pbeta(x, a, b, lower.tail=F))
}

beta_conditional_mean  <- function(x, a, b) {
  p <- beta_ccdf(x, a+1, b) 
  q <-beta_ccdf(x, a, b)
  return( p/q * a / (a+b) )
}



                     

simulate_dataset <- function(df, post, noise=0) {
  # Simulates a new dataset with the same number of stops as observed, where each race-department threshold with a random error ~ N(0,se=noise). 
  # Outputs threshold test results.
  #
  # input
  #    df: [data frame]  stop information
  #    post: [stan object] extracted posterior from Stan of main result
  #    noise: [float] noise parameter, where t'_rd = t_rd + N(mu=0, se=noise)
  # output
  #   df: [data frame] resulting aggregate information with numeber of stops, searches and hits as determined by the perturbed threshold.
  
  # extract police department, race, and number of stops
  df <- df[,1:3]
  nraces <- length(unique(df$race))
  n      <- integer(nrow(df))
  
  # initialize vectors
  searches_noisy    <- n
  hits_noisy        <- n
  thresh_noisy      <- n
  
  # extract posterior means for parameters
  phi_r    <- colMeans(post$phi_r)
  phi_d    <- colMeans(post$phi_d)
  lambda_r <- colMeans(post$lambda_r)
  lambda_d <- colMeans(post$lambda_d)
  t        <- colMeans(post$t_i)
  
  for (i in 1:nrow(df)) {
    r <- as.integer(df$race[i])
    d <- as.integer(df$police_department[i])
    nr_stops = df$num_stops[i]
    
    phi    = inv.logit(phi_d[d] + phi_r[r])
    lambda = exp(lambda_r[r] + lambda_d[d])
    
    a = phi * lambda
    b = (1-phi) * lambda
    
    # draw probability of posessing contraband for each stop in the race-department pair
    p_guilt    <- rbeta(nr_stops, a, b)
    
    # perturb threshold:  t'_rd = t_rd + N(mu=0, se=noise)
    t_noisy    <- rep(t[i], nr_stops) + rnorm(nr_stops, mean=0, sd=noise)  
    
    # deterministically conduct search only if the likelihood of posessing contraband is greater than the (perturbed) threshold
    s_dr <- subset(p_guilt, p_guilt > t_noisy)
    
    # for each search, find contraband with the probability of posessing contraband
    h_rd <- 0
    for (j in 1:length(s_dr)){
      h_rd <- h_rd + rbinom(1, 1, s_dr[j])
    }
    
    # update results
    searches_noisy[i] <- length(s_dr)
    hits_noisy[i]     <- h_rd
    thresh_noisy[i]   <- mean(t_noisy)
    
  }
  
  # return simulated dataset along with aggregate search & hit rates
  df <- df %>% 
    mutate(t_baseline   = t,
           t_noisy      = thresh_noisy,
           num_searches = searches_noisy,
           num_hits     = hits_noisy,
           search_rate  = num_searches/num_stops,
           hit_rate     = num_hits/num_searches)
  return(df)
}



perturb_thresholds <- function(df, noise, fname){
  # Perturbs each race-department threshold with a random error ~ N(0,se=noise). Outputs threshold test results.
  #
  # input
  #    df: [data frame]  stop information
  #    noise: [float] noise parameter, where t'_rd = t_rd + N(mu=0, se=noise)
  #    fname: [str] file name
  # output
  #   saves fit, posterior and observation dataframe to given directory
  stops = simulate_dataset(obs, post, noise = noise)
  output = run_mcmc(stops, paste0('../output/noise_', fname), iter=2000, simulation=TRUE)
}



#---------------------------------------------------------------------------------------
# run simulation tests
#---------------------------------------------------------------------------------------

load('../output/main_result.RData')

perturb_thresholds(obs, noise=0, fname='00')
perturb_thresholds(obs, noise=0.01, fname='01')
perturb_thresholds(obs, noise=0.02, fname='02')
perturb_thresholds(obs, noise=0.03, fname='03')
perturb_thresholds(obs, noise=0.04, fname='04')
perturb_thresholds(obs, noise=0.05, fname='05')




