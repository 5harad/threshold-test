#!/usr/bin/env Rscript
library(xtable)
source('analysis.R')

prefix='../output/'
  
load('../output/main_result.RData')

ggsave(paste0(prefix, 'benchmark_test.pdf'),
       plot_benchmark_test(obs, mx = 0.201),
       width =10, height =3.8)

ggsave(paste0(prefix, 'outcome_test.pdf'),
       plot_outcome_test(obs),
       width =10, height =3.8)

ggsave(paste0(prefix, 'signal.pdf'),
       plot_signal(obs, post, ymax = 3),
       width =5, height =5)

ggsave(paste0(prefix, 'department_thresholds.pdf'),
       plot_department_thresholds(obs, post),
       width =10, height =4)

tab_data = obs %>% group_by(race) %>%
  summarize('Stop count' = format(sum(num_stops), big.mark=",",scientific=FALSE), 
            'Search rate' = sprintf('%.1f%%', 100*sum(num_searches)/sum(num_stops)),
            'Hit rate' = sprintf('%.0f%%', 100*sum(num_hits)/sum(num_searches)))
colnames(tab_data)[1] = 'Driver race'
print.xtable(file = paste0(prefix, 'tab1.tex'), 
             xtable(tab_data, digits = 3, align = 'llrrr',
                    label = 'tab:results',
                    caption = '\\emph{Summary of the traffic stops conducted by the 100 largest police departments North Carolina. Relative to white drivers, the benchmark test (comparing search rates), finds discrimination against blacks and Hispanics, while the outcome test (comparing hit rates) finds discrimination against blacks, Hispanics, and Asians.}'),
             include.rownames = FALSE,
             booktabs = TRUE,
             sanitize.text.function = function(x) gsub('%', '\\\\%', x))
system(paste0("sed -i -e '5 i\\\\\\makebox[\\\\textwidth][c]{' -e 's/\\\\end{tabular}$/\\\\end{tabular}}/g' -e 's/\\[ht\\]/\\[t\\]/g' ", prefix, 'tab1.tex'))

tab_data[['Stop count']] = NULL
tab_data[['Search rate']] = NULL
tab_data[['Hit rate']] = NULL
tab_data[['Search threshold']] = sprintf('%.0f%%', 100*colMeans(post$thresholds))
tab_data[['95% credible interval']] = apply(colQuantiles(post$thresholds, probs = c(0.025, 0.975)), 1,
                                            function(x) sprintf('(%.0f%%, %.0f%%)', 100*x[1], 100*x[2]))
print.xtable(file = paste0(prefix, 'tab2.tex'), 
             xtable(tab_data, digits = 3, align = 'llrr',
                    label = 'tab:threshold_results',
                    caption = '\\emph{Inferred search thresholds for stops conducted by the 100 largest police departments in North Carolina. For each race group, we report the average threshold across departments, weighting by the number of stops conducted by the department. We find black and Hispanic drivers face significantly lower search thresholds than white and Asian drivers.}'),
             include.rownames = FALSE,
             booktabs = TRUE,
             sanitize.text.function = function(x) gsub('%', '\\\\%', x))
system(paste0("sed -i -e '5 i\\\\\\makebox[\\\\textwidth][c]{' -e 's/\\\\end{tabular}$/\\\\end{tabular}}/g' -e 's/\\[ht\\]/\\[t\\]/g' ", prefix, 'tab2.tex'))

black_tab = with(obs %>% filter(race == 'White') %>% left_join(obs %>% filter(race == 'Black'), by = c('police_department')),
                 table(search_rate.y>search_rate.x, hit_rate.x>hit_rate.y))
hispanic_tab = with(obs %>% filter(race == 'White') %>% left_join(obs %>% filter(race == 'Hispanic'), by = c('police_department')),
                 table(search_rate.y>search_rate.x, hit_rate.x>hit_rate.y))
asian_tab = with(obs %>% filter(race == 'White') %>% left_join(obs %>% filter(race == 'Asian'), by = c('police_department')),
                 table(search_rate.y>search_rate.x, hit_rate.x>hit_rate.y))

f = file(paste0(prefix,'tab3_raw.tex'), 'r')
tab = paste(readLines(f), collapse = '\n')
close(f)
f = file(paste0(prefix,'tab3.tex'), 'w')
writeChar(sprintf(tab,
                  black_tab[1], black_tab[3], black_tab[2], black_tab[4],
                  hispanic_tab[1], hispanic_tab[3], hispanic_tab[2], hispanic_tab[4],
                  asian_tab[1], asian_tab[3], asian_tab[2], asian_tab[4]), f)
close(f)

ggsave(paste0(prefix, 'search_rate_ppc.pdf'),
       search_rate_ppc(obs, post),
       width =5, height =4)

ggsave(paste0(prefix, 'hit_rate_ppc.pdf'),
       hit_rate_ppc(obs, post),
       width =5, height =4)

ggsave(paste0(prefix, 'signal_raleigh.pdf'),
       plot_signal(obs, post,
                   obs$race %in% c('Black', 'White') & obs$police_department == 'Raleigh Police Department',
                   ymax = 1),
       width =5, height =5)
