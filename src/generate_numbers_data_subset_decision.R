#!/usr/bin/env Rscript
library(dplyr)

load('../data/north_carolina_clean.RData')
nc = north_carolina

print(sprintf('Number of stops in raw dataset: %s',
        format(nrow(nc), digits = 0, big.mark = ',')))

print(with(nc, sprintf('%% Native American in raw dataset: %s%%',
        format(mean(race == 'Native American')*100, digits=4))))

print(with(nc, sprintf('%% race equals Other, NA or FALSE, in raw dataset: %s%%',
        format(mean(race == 'Other' | is.na(race) | race == 'FALSE')*100, digits=4))))

print(with(nc, sprintf('%% consent searches that recover contraband: %s%%',
        format(sum(contraband_found & search_type == 'Consent', na.rm=T)/
                 sum(search_type == 'Consent', na.rm=T)*100, digits=4))))

print(with(nc, sprintf('%% drivers arrested and subsequently searched: %s%%',
        format(sum(action == 'Arrest' & search_type == 'Incident To Arrest', na.rm=T)/
                 (sum(action == 'Arrest' & search_type == 'Incident To Arrest', na.rm=T) + sum(action == 'Arrest' & !search_conducted, na.rm=T))*100, digits=4))))

print(with(nc, sprintf('%% searches justified by search warrant: %s%%',
                 format(sum(search_type == 'Search Warrant', na.rm=T)/
                          sum(search_conducted)*100, digits=4))))

print(with(nc, sprintf('Number of police departments: %s',
                 format(length(unique(police_department)), digits=4))))

print(with(nc, sprintf('%% of stops conducted by state patrol: %s%%',
                 format(mean(police_department == 'NC State Highway Patrol')*100, digits=4))))

print(with(nc, sprintf('%% of searches conducted by state patrol: %s%%',
                 format(sum(search_conducted & police_department == 'NC State Highway Patrol')/sum(search_conducted)*100, digits=4))))

print(with(nc, sprintf('%% of successful searches conducted by state patrol: %s%%',
                 format(sum(contraband_found & police_department == 'NC State Highway Patrol', na.rm=T)/sum(contraband_found, na.rm=T)*100, digits=4))))

print(sprintf('%% of non-state patrol stops in largest 100 depts: %s%%',
                 format(
                   sum((nc %>% filter(police_department != 'NC State Highway Patrol') %>%
                         group_by(police_department) %>% tally() %>% arrange(desc(n)) %>% top_n(100))$n)/
                     sum(nc$police_department != 'NC State Highway Patrol')*100, digits=4)))


print(sprintf('%.2f%% of stops coded at 11pm', 100*mean(nc$stop_time == "23:00:00")))
print(sprintf('%.2f%% of stops coded at midnight', 100*mean(nc$stop_time == "00:00:00")))
print(sprintf('%.2f%% of stops coded at 1am', 100*mean(nc$stop_time == "01:00:00")))

print(sprintf('%.2f%% of drivers with age < 16 or > 105', 100*sum(nc$age < 16 | nc$age > 105, na.rm=T)/nrow(nc)))

print('Racial breakdown of contraband found')
print(nc %>% filter(contraband_found) %>% group_by(race) %>% summarise_each(funs(mean), c(money, weapon, alcohol, drugs)))

