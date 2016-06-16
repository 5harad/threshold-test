library(ggplot2)
library(rstan)
library(pracma)
library(matrixStats)
library(dplyr)
library(scales)
library(boot)
library(stats)
library(reshape2)

theme_set(theme_bw(base_size = 16) + theme(panel.grid.major = element_blank(), 
                                           panel.grid.minor = element_blank(),
                                           plot.margin = unit(c(0.5,1,0.2,0.2), "cm")))

beta_conditional_mean <- function(x, a, b) {
  return ((1-pbeta(x, a+1, b)) / (1-pbeta(x, a, b)) * a / (a+b))
}


summarize_results <- function(obs, fit)
{
  post = rstan::extract(fit)
  races = levels(obs$race)

  r = as.integer(obs$race)
  c = as.integer(obs$county)
  
  thresh = post$thresholds
  
  races = as.character(levels(obs$race))
  disc = array(dim = c(length(races),length(races)))
  rownames(disc) = races
  colnames(disc) = races
  output = as.data.frame(t(colMeans(thresh)))
  colnames(output) = races
  for (i in 1:length(races)) {
    cis = colQuantiles(thresh[,i]-thresh, probs = c(0.025, 0.975), drop = FALSE)
    print(cis)
    disc[i,-i] = ifelse(cis[-i,1] > 0, '+', ifelse(cis[-i,2] < 0, '-', ''))
    if (i==length(races)) break;
    out1 = array(dim = c(1,length(races[-(1:i)])))
    out1[1,] = ifelse(cis[-(1:i),1] > 0, races[-(1:i)], ifelse(cis[-(1:i),2] < 0, races[i], ''))
    colnames(out1) = paste(substr(races[i],1,2), 'vs', substr(races[-(1:i)],1,2))
    output = cbind(output, out1)
  }
  
  print(disc)
  
  params = get_sampler_params(fit, inc_warmup = FALSE)
  max_treedepth = slot(fit, 'stan_args')[[1]]$control$max_treedepth
  if (is.null(max_treedepth))
  {
    max_treedepth = 11
  } else {
    max_treedepth = max_treedepth + 1
  }
  diagnostics = array(dim = c(2, length(params)))
  diagnostics[1,] = sapply(params, function(x) sum(x[,'n_divergent__']))
  diagnostics[2,] = sapply(params, function(x) sum(x[,'treedepth__']>=max_treedepth))
  rownames(diagnostics) = c('sum_n_divergent', 'sum_max_treedepth')
  print(diagnostics)
  
  s = summary(fit)
  output$Rhat = max(s$summary[,'Rhat'], na.rm = TRUE)
  output$n_eff = min(s$summary[,'n_eff'])
  output$n_divergent = sum(diagnostics[1,])
  output$n_max_treedepth = sum(diagnostics[2,])
  print(paste('Max Rhat', format(round(max(s$summary[,'Rhat'], na.rm = TRUE),3), nsmall = 3)))
  print(paste('Min n_eff', format(round(min(s$summary[,'n_eff']),3), nsmall = 3)))
  
  extra_searches(obs, post)
  output
}

posterior_to_result <- function(obs, posterior, bgt1 = FALSE, unique_t = FALSE, t_i = FALSE) {
  result = obs;
  result$pred_hit_rate = colMeans(posterior$hit_rate);
  result$pred_search_rate = colMeans(posterior$search_rate);
  r = as.integer(obs$race);
  c = as.integer(obs$county);
  if (unique_t) {
    result$threshold = diag(colMeans(posterior$t_rc)[r, c]);
  }
  else 
  {
    if (!t_i)
    {
      result$threshold = colMeans(posterior$t)[r];
    }
    else
    {
      result$threshold = colMeans(posterior$t_i);
    }
  }
  result$phi = inv.logit(colMeans(posterior$phi_r)[r]+colMeans(posterior$phi_c)[c]);
  result$lambda = exp(colMeans(posterior$lambda_r)[r]+colMeans(posterior$lambda_c)[c]);
  if (bgt1) {result$lambda = 1/(1 - result$phi) + result$lambda}
  result$ushaped = (1-result$phi)*result$lambda < 1;
  result$pred_search_rate_min = colQuantiles(posterior$search_rate, probs = c(0.025));
  result$pred_search_rate_max = colQuantiles(posterior$search_rate, probs = c(0.975));
  result$pred_hit_rate_min = colQuantiles(posterior$hit_rate, probs = c(0.025));
  result$pred_hit_rate_max = colQuantiles(posterior$hit_rate, probs = c(0.975));
  result
}

search_rate_ppc <- function(obs, post, ylim = 0.03) {
  
  obs$pred_search_rate = colMeans(post$search_rate)
  
  print(with(obs,
             sprintf('Weighted RMS prediction error: %.2f%%',
                     100*sqrt(weighted.mean((search_rate-pred_search_rate)^2, num_stops)))))
  
  plt <- ggplot(data=obs, aes(x=pred_search_rate, y=pred_search_rate-search_rate)) +
    geom_point(aes(size=num_stops, color=race), alpha = 0.8) + scale_size_area(max_size=10) +
    scale_x_continuous('\nPredicted search rate', labels=percent)+#, limits=c(0,mx), labels=percent, expand = c(0, 0)) +
    scale_y_continuous('Search rate prediction error\n', labels=percent, limits=c(-ylim, ylim)) +#, labels=percent, expand = c(0, 0)) +
    geom_abline(slope=0, intercept=0, linetype='dashed') +
    theme(legend.position=c(1.0,0),
          legend.justification=c(1,0),
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent')) +
    scale_color_manual(values=c('blue','black','red', 'green4')) +
    guides(size=FALSE)
  plt
}

hit_rate_ppc <- function(obs, post, ylim = 0.2) {
  
  obs$pred_hit_rate = colMeans(post$hit_rate)
  
  print(with(obs,
       sprintf('Weighted RMS prediction error: %.2f%%',
               100*sqrt(weighted.mean((hit_rate-pred_hit_rate)^2, num_stops)))))
  
  plt <- ggplot(data=obs, aes(x=pred_hit_rate, y=hit_rate-pred_hit_rate)) +
    geom_point(aes(size=num_stops, color=race), alpha=0.8) + scale_size_area(max_size=10) +
    scale_x_continuous('\nPredicted hit rate', labels=percent) +# limits=c(0,valmax),  expand = c(0, 0)) +
    scale_y_continuous('Hit rate prediction error\n', labels=percent, limits = c(-ylim, ylim)) +#limits=c(0,valmax), labels=percent, expand = c(0, 0)) +
    geom_abline(slope=0, intercept=0, linetype='dashed') +
    theme(legend.position=c(1.0,0),
          legend.justification=c(1,0), 
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'))+
    scale_color_manual(values=c('blue','black','red', 'green4')) +
    guides(size=FALSE)

  plt
}

plot_signal <- function(obs, post, obs_mask = NULL, xlim = c(0,0.5), ymax = 5) {
  if (is.null(obs_mask)) {
    obs_mask = rep(TRUE, nrow(obs))
  }
  obs = obs[obs_mask,]
  races = levels(obs$race)  
  
  x = seq(0.0001, 0.9999, 0.0001)
  
  phi = colMeans(post$phi)[obs_mask]
  lambda = colMeans(post$lambda)[obs_mask]
  
  obs$thresholds = colMeans(post$t_i)[obs_mask]
  thresh = obs %>% group_by(police_department) %>%
    mutate(dept_stops = sum(num_stops)) %>% ungroup() %>%
    group_by(race) %>% 
    summarise(threshold = weighted.mean(thresholds, dept_stops))
  
  y = sapply(1:length(phi), function(i) obs$num_stops[i]*dbeta(x, phi[i]*lambda[i], (1-phi[i])*lambda[i])) 
  colnames(y) = obs$race
  
  df = cbind(melt(y),x)
  names(df) <- c('X1', 'race', 'value', 'x')
  
  df <- df %>%
    mutate(race = factor(race, levels = races)) %>%
    group_by(race, x) %>%
    summarise(value = sum(value)) %>%
    left_join(thresh) %>% left_join(obs %>% group_by(race) %>% summarise(num_stops = sum(num_stops))) %>%
    group_by(race) %>%
    mutate(value = value / num_stops) %>%
    as.data.frame()
  
  print(
    df %>% group_by(race) %>%
          summarise(avg = trapz(x, x*value),
                    search_rate = trapz(x[x>threshold], value[x>threshold]),
                    hit_rate = trapz(x[x>threshold], x[x>threshold]*value[x>threshold])/search_rate) %>%
    left_join(obs %>% group_by(race) %>% summarize(search_rate = sum(num_searches)/sum(num_stops),
                                              hit_rate = sum(num_hits)/sum(num_searches)), by = c('race'))
  )

  print(df %>% group_by(race) %>% summarise(at20 = mean(value[x>0.199 & x < 0.201]),
                                            over20 = trapz(x[x>0.2], value[x>0.2])))
  
  ylim=c(0,ymax)
  colors = c('blue', 'black', 'red','green4')
  names(colors) = races
  print(levels(df$race))
  
  plt <- ggplot(data=df, aes(x=x, y=value, color=race)) + geom_line() +
    geom_vline(aes(xintercept = threshold, color=race), linetype="dashed", show.legend=FALSE)+   
  scale_x_continuous('\nLikelihood of carrying contraband', limits=xlim, labels=percent, expand = c(0, 0)) +
  scale_y_continuous('Density\n', limits=c(0,ymax), expand = c(0, 0)) + 
  scale_color_manual(values = colors) +
  theme(legend.position=c(.95,.95),
        legend.justification=c(1,1),
        legend.title=element_blank(), 
        legend.background = element_rect(fill = 'transparent'))
  plt
}

threshold_differences <- function(obs, post, base_race = 1) {
  
  colors = c('blue', 'black', 'red','green4', 'orange', 'purple', 'yellow')
  races = levels(obs$race)
  
  thresh = post$thresholds
  
  thresh_diff = thresh[,base_race]-thresh[,-base_race, drop = FALSE]
  colnames(thresh_diff) = races[-base_race]
  thresh_diff = melt(thresh_diff)
  
  ggplot(thresh_diff) + geom_line(aes(x=value, color=Var2), stat = 'density') +
    scale_color_manual(values = colors[-base_race], labels=races[-base_race]) +
    geom_vline(xintercept = 0) +
    theme(legend.position=c(0.0,1.0),
          legend.justification=c(0,1), 
          legend.title=element_blank(), 
          legend.background = element_rect(fill = 'transparent')) +
    xlab('White threshold - minority threshold') + ylab('Posterior density')
    
}

threshold_differences_box <- function(obs, post, base_race = 1) {

  colors = c('blue', 'black', 'red','green4', 'orange', 'purple', 'yellow')
  races = as.character(levels(obs$race))
  
  thresh = post$thresholds
  
  r = length(levels(obs$race))
  colors = colors[1:r]
  bounds = colQuantiles(thresh[,base_race]-thresh[,-base_race, drop=FALSE], probs = c(0.025, 0.5, 0.975), drop = FALSE)
  colnames(bounds) = c('lower', 'median', 'upper')
  bounds = as.data.frame(bounds)
  bounds$race = factor(races[-base_race], levels = rev(races[-base_race]))
  
  ggplot(bounds, aes(x = race, color = race)) +
    geom_point(aes(y = median), size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    coord_flip() + 
    geom_hline(yintercept = 0, linetype = 2) + 
    #scale_x_discrete(breaks = as.character(races[-base_race])) + 
    scale_color_manual(values = rev(colors[-base_race]), labels=rev(races[-base_race])) +
    theme(legend.position="none") +
    ylab('Threshold Differences (White - Minority)') + xlab('') +
    guides(color=FALSE)
}

threshold_distributions <- function(obs, post) {
  
  colors = c('blue', 'black', 'red','green4', 'orange', 'purple', 'yellow')
  races = levels(obs$race)
  
  thresh = post$thresholds
  colnames(thresh) = races
  thresh = melt(thresh)
  
  ggplot(thresh) + geom_line(aes(x=value, color=Var2), stat = 'density') +
    scale_color_manual(values = colors[1:length(races)], labels=races) +
    theme(legend.position=c(0.0,1.0),
          legend.justification=c(0,1),
          legend.title=element_blank(), 
          legend.background = element_rect(fill = 'transparent')) +
    xlab('Threshold') + ylab('Posterior density')
  
}

plot_department_thresholds = function(obs, post, base_race = 1) {
  colors = c('blue', 'black', 'red','green4', 'orange', 'purple', 'yellow')
  races = as.character(levels(obs$race))
  obs$thresholds = colMeans(post$t_i)
  mx = max(obs$thresholds)
  df = obs %>% filter(race == races[base_race]) %>%
    right_join(obs %>% filter(race != races[base_race]), by = 'police_department')
  
  ggplot(df) + geom_point(aes(x=thresholds.x, y=thresholds.y, size = num_stops.y), alpha=0.8, shape = 1) +
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    scale_y_continuous('Minority threshold\n', limits=c(0,mx), labels=percent, expand = c(0, 0)) +
    scale_x_continuous('\nWhite threshold', limits=c(0,mx), labels=percent, expand = c(0, 0)) +
    scale_size_area(max_size=15) +
    theme(legend.position=c(0.0,1.0),
          legend.justification=c(0,1),
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          panel.margin.x=unit(1.5, "cm")) +
    scale_color_manual(values = colors[-base_race], labels=races[-base_race]) +
    guides(size=FALSE) + facet_grid(.~race.y)
}

plot_outcome_test = function(obs, base_race = 1) {
  colors = c('blue', 'black', 'red','green4', 'orange', 'purple', 'yellow')
  races = as.character(levels(obs$race))
  mx = max(obs$hit_rate)
  df = obs %>% filter(race == races[base_race]) %>% right_join(obs %>% filter(race != races[base_race]), by = 'police_department')
  
  ggplot(df) + geom_point(aes(x=hit_rate.x, y=hit_rate.y, size = num_stops.y), alpha=0.8, shape = 1) +
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    scale_y_continuous('Minority hit rate\n', limits=c(0,mx), labels=percent, expand = c(0, 0)) +
    scale_x_continuous('\nWhite hit rate', limits=c(0,mx), labels=percent, expand = c(0, 0)) +
    scale_size_area(max_size=15) +
    theme(legend.position=c(0.0,1.0),
          legend.justification=c(0,1),
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          panel.margin.x=unit(1.5, "cm")) +
    scale_color_manual(values = colors[-base_race], labels=races[-base_race]) +
    guides(size=FALSE, color = FALSE) + facet_grid( .~race.y)
}

plot_benchmark_test = function(obs, base_race = 1, mx = NULL) {
  colors = c('blue', 'black', 'red','green4', 'orange', 'purple', 'yellow')
  races = as.character(levels(obs$race))
  if (is.null(mx)) {
    mx = max(obs$search_rate)
  }
  df = obs %>% filter(race == races[base_race]) %>% right_join(obs %>% filter(race != races[base_race]), by = 'police_department')
  
  ggplot(df) + geom_point(aes(x=search_rate.x, y=search_rate.y, size = num_stops.y), alpha = 0.8, shape =1 ) +
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    scale_y_continuous('Minority search rate\n', limits=c(0,mx), labels=percent, expand = c(0, 0)) +
    scale_x_continuous('\nWhite search rate', limits=c(0,mx), labels=percent, expand = c(0, 0)) +
    scale_size_area(max_size=15) +
    theme(legend.position=c(0.0,1.0),
          legend.justification=c(0,1),
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          panel.margin.x=unit(1.5, "cm")) +
    scale_color_manual(values = colors[-base_race], labels=races[-base_race]) +
    guides(size=FALSE, color=FALSE) + facet_grid( .~race.y)
}

threshold_correlation = function(obs1, post1, obs2, post2, xlab, ylab, var = 't_i') {
  colors = c('blue', 'black', 'red','green4', 'orange', 'purple', 'yellow')
  races = as.character(levels(obs$race))
  obs1$thresholds = colMeans(post1[[var]])
  obs2$thresholds = colMeans(post2[[var]])
  
  obs_join = obs1 %>% inner_join(obs2, by = c('race', 'police_department'))
  
  print(cor(obs_join$thresholds.x, obs_join$thresholds.y))
  ggplot(obs_join) + geom_abline(intercept = 0, slope = 1 , linetype='dashed') + 
    geom_point(aes(x=thresholds.x, y=thresholds.y, color=race, size=num_stops.x), alpha = 0.8) + 
    scale_size_area(max_size=15) +
    theme(legend.position=c(0.0,1.0),
          legend.justification=c(0,1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'transparent')) +
    scale_color_manual(values = colors) + 
    scale_x_continuous(xlab, labels = percent, expand = c(0, 0)) +
    scale_y_continuous(ylab, labels = percent, expand = c(0, 0)) +
    guides(size=FALSE)
}

extra_searches = function(obs, post, base_race = 1) {
  races = levels(obs$race)
  obs$thresholds = colMeans(post$t_i)
  obs$phi = colMeans(post$phi)
  obs$lambda = colMeans(post$lambda)
  df = obs %>% filter(race == races[base_race]) %>% inner_join(obs , by = 'police_department')
  df$searches_under_white_threshold = (1-pbeta(df$thresholds.x, df$phi.y*df$lambda.y, (1-df$phi.y)*df$lambda.y))*df$num_stops.y
  df$hits_under_white_threshold = beta_conditional_mean(df$thresholds.x, df$phi.y*df$lambda.y, (1-df$phi.y)*df$lambda.y)*df$searches_under_white_threshold
  df %>% group_by(race.y) %>% summarise(
    extra_searches = sum(num_searches.y)-sum(searches_under_white_threshold),
    true_searches = sum(num_searches.y),
    pct_unjustifed = 100*extra_searches/true_searches,
    true_search_rate = sum(num_searches.y)/sum(num_stops.y),
    search_rate_under_white_threshold = (true_searches-extra_searches)/sum(num_stops.y),
    true_hit_rate = sum(num_hits.y)/sum(num_searches.y),
    hit_rate_under_white_threshold = sum(hits_under_white_threshold)/sum(searches_under_white_threshold)) %>%
    as.data.frame()
}

plot_example = function(t, p, l1, l2, t2 = t, p2=p, ylim = 3) {
  x = seq(0.0001, 0.9999, 0.0001)
  print(round(c(1-pbeta(t, l1*p, l1*(1-p)), beta_conditional_mean(t, l1*p, l1*(1-p))), digits = 2))
  print(round(c(1-pbeta(t2, l2*p2, l2*(1-p2)), beta_conditional_mean(t2, l2*p2, l2*(1-p2))), digits = 2))
  
  y1 = dbeta(x, l1*p, l1*(1-p))
  y2 = dbeta(x, l2*p2, l2*(1-p2))
  x = c(0,x,1)
  y1 = c(0,y1,0)
  y2 = c(0,y2,0)
  
  plt = ggplot() + geom_line(aes(x=x, y=y1, color = '1'), size = 0.8) + 
    geom_line(aes(x=x, y=y2, color = '2'), size = 0.8) +
    scale_y_continuous('Density\n', expand = c(0,0), limits = c(0, ylim)) +
    scale_x_continuous('\nLikelihood of possessing contraband', labels=percent, expand = c(0, 0)) +
    guides(color=FALSE)
  if (t == t2) {
    plt = plt + geom_vline(xintercept = t, linetype = 'dashed', size = 0.8)
  } else {
    plt = plt + geom_vline(aes(color = '1', xintercept = t), linetype = 'dashed', size = 0.8) +
      geom_vline(aes(color = '2', xintercept = t2), linetype = 'dashed', size = 0.8)
  }
  plt + scale_color_manual(values = c('red', 'blue'))
}

plot_thresholds_by_year = function(file_prefix, years) {
  colors = c('blue', 'black', 'red','green4', 'orange', 'purple', 'yellow')
  df = data.frame(year = NA, race = NA, mean = NA, lower = NA, upper = NA)
  for (y in years) {
    load(paste0(file_prefix, '_', y, '.RData'))

    cis = colQuantiles(post$thresholds, probs = c(0.025, 0.975))
    mn = colMeans(post$thresholds)
    df = rbind(df, data.frame(year = y, race = levels(obs$race), mean = mn, lower = cis[,1], upper = cis[,2]))
  }
  df = df[2:nrow(df),]
  print(df)
  df$race = factor(df$race, levels = levels(obs$race))

  ggplot(df, aes(x=year+0.05*(as.integer(race)-2.5), color = race, fill = race)) +
    geom_line(aes(y=mean)) +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    scale_color_manual(values=c('blue','black','red', 'green4'))
}

conflicting_departments = function(obs, post) {
  
  obs$num_no_searches = obs$num_stops - obs$num_searches
  obs$num_misses = obs$num_searches - obs$num_hits
  obs$i = 1:nrow(obs)
  
  df = obs %>% filter(race == 'White') %>%
    left_join(obs %>% filter(race %in% c('Black', 'Hispanic')), by = c('police_department')) %>%
    rowwise() %>%
    mutate(benchmark_test = (chisq.test(matrix(c(num_searches.x, num_no_searches.x, num_searches.y, num_no_searches.y), nrow = 2))[['p.value']] < 0.05)*sign(search_rate.y - search_rate.x),
      outcome_test = (chisq.test(matrix(c(num_hits.x, num_misses.x, num_hits.y, num_misses.y), nrow = 2))[['p.value']] < 0.05)*sign(hit_rate.x - hit_rate.y),
      threshold_test = ifelse(quantile(post$t_i[,i.x]-post$t_i[,i.y], 0.025)>0, 1,
                              ifelse(quantile(post$t_i[,i.x]-post$t_i[,i.y], 0.975)<0, -1, 0))) %>% as.data.frame()
}

load_mass_matrix = function(filename, post) {
  vec = read.table(filename, skip = dim(post$phi)[1]/5+26, nrows = 1,
                   comment.char = '', na.strings = '#',
                   sep = ',', stringsAsFactors = FALSE)
  vec$V1 = as.numeric(substring(vec$V1, 3))
  vec = as.numeric(vec)
  print(length(vec))
  i = 1
  out = list()
  for (n in names(post)) {
    print(n)
    print(i)
    d = dim(post[[n]])[2]

    if ((i+d-1) <= length(vec)) {
      out[[n]] = vec[i:(i+d-1)]
    } else {
      break
    }
    i = i+d
  }
  out
}

post_correlations = function(obs, post) {
  C = obs %>% mutate(r = as.integer(race), d = as.integer(police_department), i = 1:n()) %>% rowwise() %>% mutate(
    cor_phi_r = cor(post$phi_r[,r], post$t_i[,i], method='spearman'),
    cor_phi_d = cor(post$phi_d[,d], post$t_i[,i], method='spearman'),
    cor_lambda_r = cor(post$lambda_r[,r], post$t_i[,i], method='spearman'),
    cor_lambda_d = cor(post$lambda_d[,d], post$t_i[,i], method='spearman')) %>% 
    rowwise() %>% mutate(max_cor = max(abs(c(cor_phi_r, cor_phi_d, cor_lambda_r, cor_lambda_d)), na.rm = TRUE)) %>%
    as.data.frame()
  print(cor(C[,c('num_stops', 'num_searches', 'num_hits', 'search_rate', 'hit_rate')],
            C[,c('cor_phi_r', 'cor_phi_d', 'cor_lambda_r', 'cor_lambda_d', 'max_cor')], 'pair', 'spearman'))
  print(C %>% arrange(desc(max_cor)) %>% top_n(10))
  df = melt(C[,c('race','cor_phi_r', 'cor_phi_d', 'cor_lambda_r', 'cor_lambda_d')]) %>% mutate(
    var = factor(ifelse(grepl('lambda', variable), 'lambda', 'phi')),
    race_dept = factor(ifelse(grepl('_d', variable), 'department', 'race')))
  print(
    ggplot(df) + geom_line(aes(x=abs(value), color=race), stat = 'density') + facet_grid(var ~ race_dept, scales = 'free_y') + xlim(0.5,1)
  )
  C
}

noisy_thresholds = function(obs, post, noise, samples = 100, samples_per_obs = 2000) {
  obs$phi = colMeans(post$phi)
  obs$lambda = colMeans(post$lambda)
  obs$t = colMeans(post$t_i)
  obs$s_lower = numeric(nrow(obs))
  obs$s_upper = numeric(nrow(obs))
  obs$h_lower = numeric(nrow(obs))
  obs$h_upper = numeric(nrow(obs))
  
  for (i in 1:nrow(obs)) {
    print(i)
    x = matrix(rbeta(samples*samples_per_obs, obs$phi[i]*obs$lambda[i], (1-obs$phi[i])*obs$lambda[i]), nrow = samples_per_obs)
    t = pmax(0, obs$t[i] + rnorm(samples*samples_per_obs, sd = noise))
    s = colSums(x>t)
    x[x<t] = 0
    h = colSums(matrix(rbinom(samples*samples_per_obs, 1, x), nrow = samples_per_obs))
    CIs = quantile(s/samples_per_obs, probs = c(0.025, 0.975))
    obs$s_lower[i] = CIs[1]
    obs$s_upper[i] = CIs[2]
    CIh = quantile(h/s, probs = c(0.025, 0.975), na.rm = TRUE)
    obs$h_lower[i] = CIh[1]
    obs$h_upper[i] = CIh[2]
  }
  obs
}

plot_bootstrap = function(file_prefix = 'bootstrap', suffixes = 1:4) {
  
  load('../../../../data/north_carolina/jasa/main_result.RData')
  df = data.frame(race = factor(levels(obs$race), levels=levels(obs$race)), t = colMeans(post$thresholds), group = 0)
  
  for (s in suffixes) {
    load(paste0(file_prefix, s, '.RData'))
    df = rbind(df,
               data.frame(race = levels(obs$race),
                          t = colMeans(post$thresholds),
                          group = s))
  }
  
  print(
    ggplot(df) + geom_line(data=df[df$group!=0,],aes(x=group, y=t, color=race)) + 
      geom_hline(data=df[df$group==0,], aes(yintercept=t, color=race), linetype='dashed') +
      scale_color_manual(values=c('blue','black','red', 'green4')) +
      theme(legend.title = element_blank()) +
      xlab('\nBootstrap iteration') + ylab('Threshold\n')
  )
  
  df
}
