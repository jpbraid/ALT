library(tidyverse)
library(boot)

############################################################################################################
############################################################################################################

# set up initial params and lengths (in years) of the ranges over which to analyse mortality improvement
max_age <- 100
min_degree <- 2
max_degree <- 5
range_length_short <- 25
range_length_medium <- 100
range_length_long <- 125

############################################################################################################
############################################################################################################

# find the years for which life table data exists and create ranges based on above params
qx_data <- read_csv("qx_all.csv")
years_in_data <- sort(as.numeric(names(qx_data))) # this should drop the NA's
max_year <- max(years_in_data)
range_short <- years_in_data[max_year - years_in_data <= range_length_short]
range_medium <- years_in_data[max_year - years_in_data <= range_length_medium]
range_long <- years_in_data[max_year - years_in_data <= range_length_long]
ranges <- list(short = range_short, medium = range_medium, long = range_long)

# the entire analysis is identical for each gender, so we loop over genders
for (gender in c("M", "F")) {
  qx_data_gender <- qx_data %>% filter(gender == UQ(gender)) # data <- read_csv(sprintf("qx_all_%s.csv", gender))
  qx_data_gender <- qx_data_gender %>% mutate(`1890` = (`1885` + `1895`)/2) %>% 
    select(-c(`1885`, `1895`)) %>% select(age, `1890`, everything()) %>% 
    gather(key = "year", value = "q_x", -c(age, gender)) # data %>% pivot_longer(-age, names_to = "year", values_to = "q_x")
  qx_data_gender$year <- as.numeric(qx_data_gender$year)
  
  fit_data <- tibble(age = NA, range = NA, MI_fitted = NA, poly_degree = NA, adj_R_squared = NA) # CV_1, CV_2
  
  # now fit polynomial models for qx against year, for each age and each range of years;   
  # calculate MI factors based on predicted qx's from each fit        
  for (ages in 0:max_age) {
    for (range in names(ranges)) {
      for (deg in min_degree:max_degree) {
        if (length(ranges[[range]]) <= deg + 1) next # too few values for this fit
        else {
          fit <- qx_data_gender %>% filter(age == ages, year %in% ranges[[range]]) %>% 
            with(lm(q_x ~ poly(year, deg)))
          adj_R_squared <- summary(fit)$adj.r.squared
          #AIC <- fit$aic
          #set.seed(1)
          #CV <- boot::cv.glm(data %>% filter(age == ages & year %in% ranges[[range]]), fit, K = 5)$delta
          #OR do h <- lm.influence(fit)$h, then calculate CV = (yi - y-hati)^2/(1 - h_i)
          predicted_qx <- predict(fit)
          range_length <- max(ranges[[range]]) - min(ranges[[range]])
          MI_fitted <- (predicted_qx[length(ranges[[range]])]/predicted_qx[1])^(1/range_length) - 1 
          # need to do max(ranges[[range]]) - min
          #MI_crude <- qx[blah]/qx[bleh]^(1/length(range))-1
          if (is.na(MI_fitted)) next
          else {
            new_row <- c(ages, range, MI_fitted, deg, adj_R_squared)
            fit_data <- rbind(fit_data, new_row)
          }
        }
      }
    }
  }
  
  fit_data <- fit_data %>% filter(complete.cases(fit_data)) # i don't want NA's
  write.csv(fit_data, sprintf("MI_fits_%s.csv", gender), row.names = F)
  

  # what are our final smoothed MI's for each range?
  #for (raaange in names(ranges)) {
  #best_MI <- fit_data %>% filter(range == raaange) %>% group_by(age) %>% filter(adj_R_squared == max(adj_R_squared))
  #write.csv(best_MI, sprintf("output/best_MI_fit_%s_%s.csv", gender, raaange), row.names = F)
  # better: just find that polynomial degree which has a plurality of best adj R^2 for all ages, and use that degree for all of them
  # consistency is key
  #}
}

# i'll just use adjusted R^2, since they did, but it's worth raising the possibility of looking at CV error etc.
# Q: should we encode year as 0, 1, 2...?
