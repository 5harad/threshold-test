#!/usr/bin/env Rscript
library(dplyr)
library(pracma)

load('../output/main_result.RData')

print('Weighted thresholds')
print(format(data.frame(race = levels(obs$race), threshold = 100*colMeans(post$thresholds)), digits=4))

print('Extra searches')
obs$phi = colMeans(post$phi)
obs$lambda = colMeans(post$lambda)
obs$t = colMeans(post$t_i)
obs$i = 1:nrow(obs)
df = obs %>% filter(race == 'White') %>%
  inner_join(obs, by = 'police_department') %>%
  rowwise() %>%
  mutate(searches_under_white_threshold = (1-pbeta(t.x, phi.y*lambda.y, (1-phi.y)*lambda.y))*num_stops.y) %>%
  ungroup() %>% as.data.frame()
a = post$phi*post$lambda
b = (1-post$phi)*post$lambda
a = a[,df$i.y]
b = b[,df$i.y]
t = post$t_i[,df$i.x]
searches = apply(t(t(1-pbeta(t,a,b))*df$num_stops.y), 1, function(x) accumarray(as.integer(df$race.y), x))
total_searches = accumarray(as.integer(obs$race), obs$num_searches)
total_stops = accumarray(as.integer(obs$race), obs$num_stops)
print(format(data.frame(race = levels(obs$race),
                  extra_searches_under_white_threshold = total_searches-rowMeans(searches),
                  lower_95_CI = total_searches-rowQuantiles(searches, probs = c(0.025)),
                  upper_95_CI = total_searches-rowQuantiles(searches, probs = c(0.975)),
                  as_percent_of_total_searches = 100*(total_searches-rowMeans(searches))/total_searches,
                  search_rate_under_white_threshold = 100*rowMeans(searches)/total_stops,
                  actual_search_rate = 100*total_searches/total_stops),
  digits = 4))


pd = 'Raleigh Police Department'

print(paste(pd, 'stats'))
print(format(obs %>% filter(police_department == pd) %>%
               mutate(search_rate = 100*search_rate,
                  hit_rate = 100*hit_rate,
                  threshold = 100*t) %>%
         dplyr::select(race, search_rate, hit_rate, threshold), digits = 4))

df = obs %>% filter(police_department == pd) %>%
  mutate(mass_above_20_pct = (1-pbeta(0.2, phi*lambda, (1-phi)*lambda)))
print(sprintf('%.2f times more mass above 20%% for blacks than whites in %s',
        (df$mass_above_20_pct[df$race=='Black'])/(df$mass_above_20_pct[df$race=='White']), pd))


print(sprintf('%.2f%% of stops coded at 11pm', 100*mean(nc$stop_time == "23:00:00")))
print(sprintf('%.2f%% of stops coded at midnight', 100*mean(nc$stop_time == "00:00:00")))
print(sprintf('%.2f%% of stops coded at 1am', 100*mean(nc$stop_time == "01:00:00")))

print(sprintf('%.2f%% of drivers with age < 16 or > 105', 100*mean(nc$age < 16 | nc$age > 105)))


print('Racial breakdown of contraband found')
print(nc %>% filter(contraband_found) %>% group_by(race) %>% summarise_each(funs(mean), c(money, weapon, alcohol,drugs)))
