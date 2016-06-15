source('run_mcmc.R')

load('../data/north_carolina.RData')


output = run_mcmc(north_carolina, '../output/main_result', iter = 5000, chains = 5)

