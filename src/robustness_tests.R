#!/usr/bin/env Rscript
source('run_mcmc.R')

#---------------------------------------------------------------------------------------
# placebo tests
#  - season
#  - day of week
#---------------------------------------------------------------------------------------
placebo_season <- function(df){
  # Placebo test replacing 'race' by season. Outputs threshold test results.
  #
  # input
  #    df: [data frame]  stop information
  # output
  #   saves fit, posterior and observation dataframe to given directory
  stops <- df %>%
    mutate(month = as.integer(substr(stop_date, 6, 7))) %>%
    mutate(race = factor(ifelse(month == 12 | month <=2, 'Winter',
                         ifelse(month > 2 & month <= 5, 'Spring',
                         ifelse(month > 5 & month <= 8, 'Summer', 'Fall'))),
                         levels = c('Fall', 'Winter', 'Spring', 'Summer'))) 
  
  output = run_mcmc(stops, '../output/season', iter = 2000, chains = 5)
}


placebo_weekday <- function(df){
  # Placebo test replacing 'race' by day of week. Outputs threshold test results.
  #
  # input
  #    df: [data frame]  stop information
  # output
  #   saves fit, posterior and observation dataframe to given directory
  
  stops <- df %>%
    mutate(month = as.integer(substr(stop_date, 6, 7))) %>%
    mutate(race = factor(weekdays(as.Date(stop_date)),
                         levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) 

  output = run_mcmc(stops, 'output/weekday', iter = 2000, chains = 5)
}




#---------------------------------------------------------------------------------------
# disaggregation tests
#  - gender
#  - time of day
#  - year
#---------------------------------------------------------------------------------------

fit_shift <- function(df, start_hr, stop_hr, fname){
  # Filters stops between that occured between [start_hr, stop_hr) and outputs threshold test results.
  # Excludes stops occuring at "00:00:00" as some departments do not record the exact stop time and use this as default.
  #
  # inputs
  #    df: [data frame]  stop information
  #    start_hr: [int] hour 
  #    stop_hr: [int] hour 
  #    fname: [str] file name
  # output
  #   saves fit, posterior and observation dataframe to given directory
  
  stops <- df %>%
  filter(stop_time != "00:00:00") %>%
  mutate(hour = as.integer(substr(stop_time, 1, 2))) %>%
  filter(hour >= start_hr & hour < stop_hr)

  output = run_mcmc(stops, paste0('output/hour_', fname), iter = 2000, chains = 5)
}



fit_gender <- function(df, gender_class, fname){
  # Filters stops by gender and outputs threshold test results.
  # inputs
  #    df: [data frame]  stop information
  #    gender_class: [str] gender of driver ('Male' or 'Female')
  #    fname: [str] file name
  # output
  #   saves fit, posterior and observation dataframe to given directory
  
  stops <- df %>%
    filter(gender == gender_class)
  
  output = run_mcmc(stops, paste0('output/gender_', fname), iter = 2000, chains = 5)
}


fit_year <- function(df, yr, fname){
  # Filters stops by year of stop and outputs threshold test results.
  # inputs
  #    df: [data frame]  stop information
  #    yr: [int] year of stop (2009 - 2014)
  #    fname: [str] file name
  # output
  #   saves fit, posterior and observation dataframe to given directory
  
  stops <- df %>%
    mutate(year = as.integer(substr(stop_date, 1, 4))) %>%
    filter(year == yr)
  
  output = run_mcmc(stops, paste0('output/year_', fname), iter = 2000, chains = 5)
}


fit_age <- function(df, start_age, stop_age, fname){
  # Filters stops >= start_age and <= stop_age and outputs threshold test results.
  # inputs
  #    df: [data frame]  stop information
  #    start_age: [int] minimum age of driver
  #    stop_age: [int] maximum age of driver
  #    fname: [str] file name
  # output
  #   saves fit, posterior and observation dataframe to given directory
  stops <- df %>%
    filter(age >= start_age & age <= stop_age)
  
  output = run_mcmc(stops, paste0('output/age_', fname), iter = 2000, chains = 5)
}



fit_search_basis <- function(df, fname){
  # NC allows for multiple search basis for each stop. 
  # This function excludes stops that include 'Other Official Info' as a search basis, and outputs threshold test results.
  # inputs
  #    df: [data frame]  stop information
  #    fname: [str] file name
  # output
  #   saves fit, posterior and observation dataframe to given directory
  
  stops <- df %>% 
    mutate(search_conducted = ifelse(search_type == 'Probable Cause' & (! grepl('Other Official Info', search_basis)), TRUE, FALSE),
           contraband_found = ifelse(search_conducted==TRUE, contraband_found, FALSE))
  
  print('Running model for probable cause, no official info')
  
  output = run_mcmc(stops, paste0(path, fname), iter = 2000, chains = 5, model='model.stan')
}


#---------------------------------------------------------------------------------------
# run tests
#---------------------------------------------------------------------------------------

load('../data/north_carolina.RData')


placebo_season(df=north_carolina)
placebo_weekday(df=north_carolina)

fit_shift(df=north_carolina, start_hr=0, stop_hr=4,   fname='0_4')
fit_shift(df=north_carolina, start_hr=4, stop_hr=8,   fname='4_8')
fit_shift(df=north_carolina, start_hr=8, stop_hr=12,  fname='8_12')
fit_shift(df=north_carolina, start_hr=12, stop_hr=16, fname='12_16')
fit_shift(df=north_carolina, start_hr=16, stop_hr=20, fname='16_20')
fit_shift(df=north_carolina, start_hr=20, stop_hr=24, fname='20_24')

fit_gender(df=north_carolina, gender_class = 'Male',   fname='male')
fit_gender(df=north_carolina, gender_class = 'Female', fname='female')

fit_year(df=north_carolina, yr = 2009, fname='2009')
fit_year(df=north_carolina, yr = 2010, fname='2010')
fit_year(df=north_carolina, yr = 2011, fname='2011')
fit_year(df=north_carolina, yr = 2012, fname='2012')
fit_year(df=north_carolina, yr = 2013, fname='2013')
fit_year(df=north_carolina, yr = 2014, fname='2014')

fit_age(df=north_carolina, start_age=16, stop_age=20, fname='16-20')
fit_age(df=north_carolina, start_age=21, stop_age=30, fname='21-20')
fit_age(df=north_carolina, start_age=31, stop_age=40, fname='31-40')
fit_age(df=north_carolina, start_age=41, stop_age=50, fname='41-50')
fit_age(df=north_carolina, start_age=51, stop_age=105, fname='51-105')

fit_search_basis(df=north_carolina, fname='search_basis_pc_excl_official_info')


