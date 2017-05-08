#!/usr/bin/env Rscript
library(stringr)
library(dplyr)


# ----------------------------------------------------
# person
# ----------------------------------------------------
person <- read.table('../data/orig_data/PERSON.tsv', sep='\t', header=TRUE, stringsAsFactors=FALSE)
names(person) <- c('person_id', 'stop_id', 'person_type', 'age', 'gender', 'ethnicity', 'race')

# remove last row (incomplete)
person <- person[-nrow(person),]


person <- person %>%
  mutate(person_type = ifelse(person_type=="", NA, ifelse(person_type=="D", 'Driver', 'Passenger')),
         gender = ifelse(gender=='M', 'Male', gender),
         gender = ifelse(gender=='F', 'Female', gender),
         race   = ifelse(race=="", NA, race),
         race   = ifelse(race=="A", "Asian", race),
         race   = ifelse(race=="B", "Black", race),
         race   = ifelse(race=="I", "Native American", race),
         race   = ifelse(race=="U", "Other", race),
         race   = ifelse(race=="W", "White", race),
         ethnicity = ifelse(ethnicity=="H", "Hispanic", ethnicity),
         ethnicity = ifelse(ethnicity=="N", "Non-Hispanic", ethnicity),
         ethnicity = ifelse(ethnicity=="", NA, ethnicity),
         race   = ifelse(ethnicity=="Hispanic", "Hispanic", race)) %>%
  filter(person_type != 'Passenger')



# ----------------------------------------------------
# search
# ----------------------------------------------------
search <- read.table('../data/orig_data/SEARCH.tsv', sep='\t', header=TRUE, stringsAsFactors=FALSE)
names(search) <- c('search_id','stop_id','person_id','search_type','vehicle_search','driver_search','passenger_search','property_search','vehicle_seized','personal_property_seized','other_property_seized')

search <- search %>%
  mutate(search_conducted = TRUE,
         search_type = ifelse(search_type==1, "Consent", search_type),
         search_type = ifelse(search_type==2, "Search Warrant", search_type),
         search_type = ifelse(search_type==3, "Probable Cause", search_type),
         search_type = ifelse(search_type==4, "Incident To Arrest", search_type),
         search_type = ifelse(search_type==5, "Protective Frisk", search_type),
         vehicle_search    = ifelse(vehicle_search==1,TRUE,FALSE),
         driver_search     = ifelse(driver_search==1,TRUE,FALSE),
         passenger_search  = ifelse(passenger_search==1,TRUE,FALSE),
         property_search   = ifelse(property_search==1,TRUE,FALSE),
         vehicle_seized    = ifelse(vehicle_seized==1,TRUE,FALSE),
         personal_property_seized = ifelse(personal_property_seized==1,TRUE,FALSE),
         other_property_seized    = ifelse(other_property_seized==1,TRUE,FALSE)) 



# ----------------------------------------------------
# search basis
# one row per stop if a search was conducted
# one search_type per search
# ----------------------------------------------------
search_basis <- read.table('../data/orig_data/SEARCHBASIS.tsv', sep='\t', header=TRUE, stringsAsFactors=FALSE)
names(search_basis) <- c('search_basis_id','search_id','person_id','stop_id','search_basis')

search_basis <- search_basis %>%
  mutate(id = paste0(stop_id, search_id),
         search_basis = ifelse(search_basis=="ER", "Erratic Suspicious Behaviour",search_basis),
         search_basis = ifelse(search_basis=="OB", "Observation Suspected Contraband",search_basis),
         search_basis = ifelse(search_basis=="OI", "Other Official Info",search_basis),
         search_basis = ifelse(search_basis=="SM", "Suspicious Movement",search_basis),
         search_basis = ifelse(search_basis=="TIP", "Informant Tip",search_basis),
         search_basis = ifelse(search_basis=="WTNS", "Witness Observation",search_basis))

search_basis <- search_basis %>%
  group_by(id) %>%
  summarise(search_id = mean(search_id),
            stop_id = mean(stop_id), 
            all_basis = paste(search_basis, collapse=','))

# ----------------------------------------------------
# contraband
# ----------------------------------------------------
contraband <- read.table('../data/orig_data/CONTRABAND.tsv', sep='\t', header=TRUE, stringsAsFactors=FALSE)
names(contraband) <- c('contraband_id','search_id','person_id','stop_id',
                       'ounces_drugs','pounds_drugs','pints_alcohol','gallons_alcohol','dosages_drugs','grams_drugs',
                       'kilos_drugs', 'amount_money','number_weapons','other_contraband_dollar_amount')

contraband <- contraband %>%
  mutate(contraband_found = TRUE,
         money = ifelse(amount_money>0 | other_contraband_dollar_amount>0, TRUE, FALSE),
         weapon  = ifelse(number_weapons>0, TRUE, FALSE),
         alcohol = ifelse(pints_alcohol>0 | gallons_alcohol>0, TRUE, FALSE),
         drugs   = ifelse(ounces_drugs>0 | pounds_drugs>0 | grams_drugs>0 | kilos_drugs>0 | dosages_drugs>0, TRUE, FALSE)) %>%
  dplyr::select(-contraband_id)


# ----------------------------------------------------
# stops
# ----------------------------------------------------
stops <- read.table('../data/orig_data/STOP.tsv', sep='\t', header=TRUE, stringsAsFactors=FALSE, quote = "", fill=TRUE)
names(stops) <- c('stop_id', 'police_department', 'stop_date', 'stop_purpose', 'action', 'driver_arrest', 'passenger_arrest', 
                  'encounter_force','engage_force', 'officer_injury', 'driver_injury', 'passenger_injury', 'officer_id',
                  'county_id', 'stop_city')

stops <- stops %>%
  mutate(stop_time  = substr(stop_date,12,19),
         stop_date  = substr(stop_date,1,10),
         stop_date  = as.Date(stop_date, format='%Y-%m-%d'),
         stop_city  = str_to_title(stop_city),
         stop_purpose = ifelse(stop_purpose==1,'Speed Limit Violation',stop_purpose),
         stop_purpose = ifelse(stop_purpose==2,'Stop Light/Sign Violation',stop_purpose),
         stop_purpose = ifelse(stop_purpose==3,'Impaired Driving',stop_purpose),
         stop_purpose = ifelse(stop_purpose==4,'Safe Movement Violation',stop_purpose),
         stop_purpose = ifelse(stop_purpose==5,'Vehicle Equipment Violation',stop_purpose),
         stop_purpose = ifelse(stop_purpose==6,'Vehicle Regulatory Violation',stop_purpose),
         stop_purpose = ifelse(stop_purpose==7,'Seat Belt Violation',stop_purpose),
         stop_purpose = ifelse(stop_purpose==8,'Investigation',stop_purpose),
         stop_purpose = ifelse(stop_purpose==9,'Other Motor Vehicle Violation',stop_purpose),
         stop_purpose = ifelse(stop_purpose==10,'Checkpoint',stop_purpose),
         action = ifelse(action==1,'Verbal Warning',action),
         action = ifelse(action==2,'Written Warning',action),
         action = ifelse(action==3,'Citation',action),
         action = ifelse(action==4,'Arrest',action),
         action = ifelse(action==5,'No Action',action),
         verbal_warning  = ifelse(action=='Verbal Warning',TRUE,FALSE),
         written_warning = ifelse(action=='Written Warning',TRUE,FALSE),
         citation = ifelse(action=='Citation',TRUE,FALSE),
         arrest   = ifelse(action=='Arrest',TRUE,FALSE),        
         driver_arrest    = ifelse(driver_arrest==1,TRUE,FALSE),
         passenger_arrest = ifelse(passenger_arrest==1,TRUE,FALSE),
         encounter_force  = ifelse(encounter_force==1,TRUE,FALSE),
         engage_force     = ifelse(engage_force==1,TRUE,FALSE),
         officer_injury   = ifelse(officer_injury ==1,TRUE,FALSE),
         driver_injury    = ifelse(driver_injury ==1,TRUE,FALSE),
         passenger_injury = ifelse(passenger_injury ==1,TRUE,FALSE))



# ----------------------------------------------------
# add county name
# ----------------------------------------------------
county_codes <- read.csv('../data/orig_data/county_codes.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
county_codes <- county_codes[,1:2]
stops$county_name <- county_codes$county_name[match(stops$county_id, county_codes$county_id)]


# ----------------------------------------------------
# merge data frames and save cleaned data
# ----------------------------------------------------
north_carolina <- left_join(search, search_basis, by=c('stop_id', 'search_id'))
north_carolina <- left_join(north_carolina, contraband, by=c('search_id')) %>% rename(stop_id = stop_id.x)
north_carolina <- left_join(stops, north_carolina, by='stop_id')
north_carolina <- left_join(north_carolina, person, by='stop_id')

north_carolina <- north_carolina %>%
  select(-c(person_id,person_id.x, person_id.y, stop_id, stop_id.y, id ))

# convert NA to FALSE
north_carolina[is.na(north_carolina)] <- FALSE

# save clean data frame 
save(north_carolina, file='../data/north_carolina_clean.RData')


# ------------------------------------------------------------------------------------------------
# subset data for analysis
# - exclude NC state patrol
# - remove race: other, missing, Native American
# - include only 100 largest departments
# ------------------------------------------------------------------------------------------------
top_100_depts <- north_carolina %>% 
  filter(police_department != 'NC State Highway Patrol') %>%
  filter(race %in% c('White', 'Black', 'Hispanic', 'Asian')) %>%
  filter(gender %in% c('Female', 'Male')) %>%
  group_by(police_department) %>% 
  tally() %>% 
  top_n(100) %>% 
  arrange(desc(n)) %>% 
  dplyr::select(police_department) %>%
  as.data.frame()


north_carolina <- north_carolina %>%
  filter(police_department != 'NC State Highway Patrol') %>%
  filter(race %in% c('White', 'Black', 'Hispanic', 'Asian')) %>%
  filter(gender %in% c('Female', 'Male')) %>%
  filter(police_department %in% top_100_depts$police_department) %>%
  mutate(race         = factor(as.character(race), levels = c('White', 'Black', 'Hispanic', 'Asian')),
         gender       = factor(as.character(gender), levels = c('Female', 'Male')),
         search_basis = all_basis) %>%
  dplyr::select(c(police_department, gender, age, race, stop_date, stop_time, search_conducted, search_basis, contraband_found, money, weapon, alcohol, drugs))


# save clean data as tsv file
write.table(north_carolina, file='../data/north_carolina.tsv', quote=FALSE, sep='\t', row.names = FALSE)


