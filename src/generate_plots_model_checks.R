library(xtable)
source('analysis.R')

generate_plots = function(prefix = 'output/') {
  
  ggsave(paste0(prefix, 'example_red_discrimination.pdf'),
         plot_example(0.3, 0.352, 28.98, t2=0.351, p2=0.3878, l2=26.48, ylim = 5),
         width =5, height =5)
  
  ggsave(paste0(prefix, 'example_blue_discrimination.pdf'),
         plot_example(0.304, 0.354, 30.59, t2=0.254, p2=0.3385, l2=6.19, ylim = 5),
         width =5, height =5)
  
  load('../output/season.RData')
  df = data.frame(test = 'Season',
                  group = levels(obs$race), 
                  t = colMeans(post$thresholds),
                  tu = colQuantiles(post$thresholds, probs = c(0.975)),
                  tl = colQuantiles(post$thresholds, probs = c(0.025)))
  
  load('../output/weekday.RData')
  df = rbind(df,
             data.frame(test = 'Weekday', group = levels(obs$race),
                        t = colMeans(post$thresholds),
                        tu = colQuantiles(post$thresholds, probs = c(0.975)),
                        tl = colQuantiles(post$thresholds, probs = c(0.025))))
  df$group = factor(df$group, levels = df$group)
  
  ggsave(paste0(prefix, 'placebo.pdf'),
    ggplot(df, aes(x=group, y=t)) +
      ylab( 'Inferred threshold\n') +
      scale_y_continuous(labels=percent, expand = c(0,0), limits = c(0,0.15), breaks=c(0,0.05,0.10,0.15)) +
      geom_segment(aes(y=tl, yend=tu, xend = group),  stat="identity")+
      geom_point(aes(x=group, y=t)) +
      facet_grid( . ~test, scales="free", space="free") + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)),
    width =8, height =5)
  
  
  races <- factor(c('White', 'Black', 'Hispanic', 'Asian'), levels =c('White', 'Black', 'Hispanic', 'Asian'))
  
  load('../output/gender_male.RData')
  df <- data.frame(test = 'Gender', group= rep('Male', 4), race = races, 
                   t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                   stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                   t_upper = colQuantiles(post$thresholds, prob = 0.975),
                   t_lower = colQuantiles(post$thresholds, prob = 0.025))
  
  load('../output/gender_female.RData')
  df_female <- data.frame(test = 'Gender' , group = rep('Female',4), race = races, 
                          t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                          stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                          t_upper = colQuantiles(post$thresholds, prob = 0.975),
                          t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_female)
  
  
  
  load('../output/hour_8_12.RData')
  df_shift <- data.frame(test = 'Time', group= rep('8am - 12pm', 4), race = races, 
                          t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                          stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                          t_upper = colQuantiles(post$thresholds, prob = 0.975),
                          t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_shift)
  
  load('../output/hour_12_16.RData')
  df_shift <- data.frame(test = 'Time', group= rep('12pm - 4pm', 4), race = races, 
                          t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                          stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                          t_upper = colQuantiles(post$thresholds, prob = 0.975),
                          t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_shift)
  
  load('../output/hour_16_20.RData')
  df_shift <- data.frame(test = 'Time', group= rep('4pm - 8pm', 4), race = races, 
                          t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                          stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                          t_upper = colQuantiles(post$thresholds, prob = 0.975),
                          t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_shift)
  
  load('../output/hour_20_24.RData')
  df_shift <- data.frame(test = 'Time', group= rep('8pm - 12am', 4), race = races, 
                          t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                          stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                          t_upper = colQuantiles(post$thresholds, prob = 0.975),
                          t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_shift)
  
  load('../output/hour_0_4.RData')
  df_shift <- data.frame(test = 'Time', group= rep('12am - 4am', 4), race = races, 
                          t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                          stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                          t_upper = colQuantiles(post$thresholds, prob = 0.975),
                          t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_shift)
  
  load('../output/hour_4_8.RData')
  df_shift <- data.frame(test = 'Time', group= rep('4am - 8am', 4), race = races, 
                          t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                          stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                          t_upper = colQuantiles(post$thresholds, prob = 0.975),
                          t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_shift)
  
  
  load('../output/year_2009.RData')
  df_yr <- data.frame(test = 'Year', group= rep('2009', 4), race = races, 
                      t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                      stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                      t_upper = colQuantiles(post$thresholds, prob = 0.975),
                      t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_yr)
  
  load('../output/year_2010.RData')
  df_yr <- data.frame(test = 'Year', group= rep('2010', 4), race = races, 
                      t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                      stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                      t_upper = colQuantiles(post$thresholds, prob = 0.975),
                      t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_yr)
  
  load('../output/year_2011.RData')
  df_yr <- data.frame(test = 'Year', group= rep('2011', 4), race = races, 
                      t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                      stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                      t_upper = colQuantiles(post$thresholds, prob = 0.975),
                      t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_yr)
  
  load('../output/year_2012.RData')
  df_yr <- data.frame(test = 'Year', group= rep('2012', 4), race = races, 
                      t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                      stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                      t_upper = colQuantiles(post$thresholds, prob = 0.975),
                      t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_yr)
  
  load('../output/year_2013.RData')
  df_yr <- data.frame(test = 'Year', group= rep('2013', 4), race = races, 
                      t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                      stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                      t_upper = colQuantiles(post$thresholds, prob = 0.975),
                      t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_yr)
  
  load('../output/year_2014.RData')
  df_yr <- data.frame(test = 'Year', group= rep('2014', 4), race = races, 
                      t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                      stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                      t_upper = colQuantiles(post$thresholds, prob = 0.975),
                      t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_yr)
  
  
  load('../output/age_16_20.RData')
  df_age <- data.frame(test = 'Age', group= rep('16-20', 4), race = races, 
                      t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                      stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                      t_upper = colQuantiles(post$thresholds, prob = 0.975),
                      t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_age)
  
  load('../output/age_21_30.RData')
  df_age <- data.frame(test = 'Age', group= rep('21-30', 4), race = races, 
                       t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                       stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                       t_upper = colQuantiles(post$thresholds, prob = 0.975),
                       t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_age)
  
  load('../output/age_31_40.RData')
  df_age <- data.frame(test = 'Age', group= rep('31-40', 4), race = races, 
                       t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                       stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                       t_upper = colQuantiles(post$thresholds, prob = 0.975),
                       t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_age)
  
  load('../output/age_41_50.RData')
  df_age <- data.frame(test = 'Age', group= rep('41-50', 4), race = races, 
                       t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                       stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                       t_upper = colQuantiles(post$thresholds, prob = 0.975),
                       t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_age)
  
  load('../output/age_51_105.RData')
  df_age <- data.frame(test = 'Age', group= rep('51+', 4), race = races, 
                       t_hat = colMeans(post$thresholds), se = apply(post$thresholds, 2, sd),
                       stop_count = (obs %>% group_by(race) %>% summarize(num_stops = sum(num_stops)))$num_stops,
                       t_upper = colQuantiles(post$thresholds, prob = 0.975),
                       t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, df_age)
  
  ggsave(file=paste0(prefix, 'confounding_tests.pdf'),
         ggplot(df, aes(x=group, y=t_hat)) +
           ylab( 'Inferred threshold\n') +
           scale_y_continuous( labels=percent, limits=c(0,0.25), expand = c(0,0)) +
           geom_point(aes(y=t_hat, color=race, size=stop_count)) +
           geom_line(aes(y=t_hat, color=race, group=race)) +
           facet_grid( . ~test, scales="free", space="free") + 
           scale_color_manual(values=c('blue','black','red', 'green4'))  +
           scale_size_area(15) + 
           guides(size=FALSE) + 
           theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.title.x = element_blank(),
                 legend.title = element_blank(),
                 #legend.position=c(0.7,0.8),
                 legend.background = element_rect(fill = 'transparent'),
                 axis.text.x = element_text(angle = 45, hjust = 1)),
         width=10, height=5)
  
  
  load('../output/main_result.RData')
  post0 = post
  obs0 = obs
  t_hat0 = colMeans(post0$thresholds)
  
  load('../output/noise_00.RData')
  df <- data.frame(noise = rep(0.00, 4) , race = races, t_hat = colMeans(post$thresholds),
                   t_upper = colQuantiles(post$thresholds, prob = 0.975),
                   t_lower = colQuantiles(post$thresholds, prob = 0.025))
  
  load('../output/noise_01.RData')
  n1 <- data.frame(noise = rep(0.01, 4) , race = races, t_hat = colMeans(post$thresholds),
                   t_upper = colQuantiles(post$thresholds, prob = 0.975),
                   t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, n1)
  
  load('../output/noise_02.RData')
  n2 <- data.frame(noise =  rep(0.02, 4) , race = races, t_hat = colMeans(post$thresholds),
                   t_upper = colQuantiles(post$thresholds, prob = 0.975),
                   t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, n2)
  
  load('../output/noise_03.RData')
  n3 <- data.frame(noise = rep(0.03, 4), race = races, t_hat = colMeans(post$thresholds),
                   t_upper = colQuantiles(post$thresholds, prob = 0.975),
                   t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, n3)
  
  load('../output/noise_04.RData')
  n4 <- data.frame(noise = rep(0.04, 4), race = races, t_hat = colMeans(post$thresholds), 
                   t_upper = colQuantiles(post$thresholds, prob = 0.975),
                   t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, n4)
  
  load('../output/noise_05.RData')
  n5 <- data.frame(noise = rep(0.05, 4), race = races, t_hat = colMeans(post$thresholds),
                   t_upper = colQuantiles(post$thresholds, prob = 0.975),
                   t_lower = colQuantiles(post$thresholds, prob = 0.025))
  df <- rbind(df, n5)
  
  df$actual <- rep(t_hat0, 6)
  
  ggsave(file=paste0(prefix, 'noise.pdf'),
          ggplot(df, aes(x=noise, y=t_hat, color=race)) +
          geom_line() + 
          geom_point() + 
          geom_hline(aes(yintercept = actual, colour = race), linetype = 2) +
          xlab(expression('Threshold heterogeneity (' * sigma * ')')) +
          ylab('Posterior estimate of threshold\n') +
          scale_x_continuous( labels=percent, limits=c(0,0.05), expand = c(0, 0)) +
          scale_y_continuous( labels=percent, limits=c(0,0.2), expand = c(0, 0)) +
          scale_color_manual(values=c('blue','black','red', 'green4'))  +
          scale_fill_manual(values=c('blue','black','red', 'green4'))  +
          theme(legend.title = element_blank(),
                legend.background = element_rect(fill = 'transparent'),
                axis.title.x=element_text(margin=margin(20,0,0,0))),
         width=7, height = 5)
  
}
