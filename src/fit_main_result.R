#!/usr/bin/env Rscript
source('run_mcmc.R')


# load data
north_carolina <- read_tsv(file = '../data/north_carolina.tsv.gz', sep = '\t', header = TRUE)

# run and save threshold results
output = run_mcmc(north_carolina, '../output/main_result', iter = 5000, chains = 5)

