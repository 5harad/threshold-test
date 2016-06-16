library(rstan)
library(dplyr)

run_mcmc <- function(stops, output_filename, iter = 5000, warmup = NULL, chains = 5, adapt_delta = 0.9, max_treedepth = 12, sample_from_prior = FALSE, verbose = FALSE, simulation=FALSE, model_file = 'model.stan') {

  if (is.null(warmup)) {
    if (sample_from_prior) {
      warmup = 0
    } else {
      warmup = ceiling(iter/2)
    }
  }

  if (!simulation){
    obs <- stops %>% 
      group_by(police_department, race) %>%
      summarise(num_stops    = n(),
                num_searches = sum(search_conducted),
                num_hits     = sum(contraband_found),
                search_rate  = num_searches/num_stops,
                hit_rate     = num_hits/num_searches) %>%
      filter(num_searches != 0) %>%
      as.data.frame()
  } else {
    obs <- stops
  }
  
  sorted_departments <- obs %>% 
    group_by(police_department) %>% 
    summarize(num_stops = sum(num_stops)) %>% 
    arrange(desc(num_stops)) 
  
  obs <- obs %>%
    mutate(race = factor(race, levels = unique(obs$race)),
           police_department = factor(police_department, levels=unique(sorted_departments$police_department))) %>%
    arrange(desc(num_stops)) %>%
    as.data.frame()
  
  # check model identifiability
  r <- length(unique(obs$race))
  d <- length(unique(obs$police_department))
  if (d <= 2*r/(r-2)) {
    stop('Not enough departments to constrain estimates')
  }

  
  # Package data for Stan
  stan_data = with(obs, list(
    N = nrow(obs),
    D = length(unique(police_department)),
    R = length(unique(race)),
    d = as.integer(police_department),
    r = as.integer(race),
    n = num_stops,
    s = num_searches,
    h = num_hits
  ))
  
  # set up parameter initialization
  initializer <- function(num_obs, num_races, num_depts) {	
    
    # force immediate evaluation of arguments
    force(num_obs); force(num_races); force(num_depts);
    
    function() {
      list(sigma_t        = runif(num_races, 0.1, 0.5),
           mu_phi_d       = runif(1, -5, -3),
           sigma_phi_d    = runif(1, 0.1, 0.5),
           mu_lambda_d    = runif(1, -0.5, 1.5),
           sigma_lambda_d = runif(1, 0.1, 0.5),
           t_r            = runif(num_races, -3, -1),
           t_i_raw        = runif(num_obs, -0.5, 0.5),
           phi_r          = runif(num_races, -5, -3),
           phi_d_raw      = runif(num_depts-1, -0.5, 0.5),
           lambda_r       = runif(num_races, -0.5, 1.5), 
           lambda_d_raw   = runif(num_depts-1, -0.5, 0.5)
      )
    }
  }

  # fit the model
  init_fn <- initializer(stan_data$N, stan_data$R, stan_data$D)
  fit <- stan(model_file, data=stan_data, iter=iter, init=init_fn, chains=chains, cores=chains, refresh = 50, warmup = warmup, control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth, adapt_engaged = !sample_from_prior), verbose = verbose, diagnostic_file = paste0(output_filename, '_diag.txt'))

  post = rstan::extract(fit)
  save(file=paste0(output_filename, '.RData'), obs, post, fit)
  list(obs=obs, post=post, fit=fit)
}



