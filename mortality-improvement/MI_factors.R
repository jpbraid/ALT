library(tidyverse)
library(boot)

############################################################################################################
##years_short <- 26
##years_medium <- 100
##years_long <- 125
##year_ranges <- c(1885, 1895, 1905, 1921, 1933, 1947, 1954, 1961, 1966,
##                 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016, 2021)
##data <- read_csv("S:/Agencies/ALT/ALT/ALT2015-17/jesse/4_mortality_improvement/qx_all_M.csv")
##year_max <- max(year_ranges)
##range_short <- year_ranges[year_max - year_ranges <= 26]
##years_short_actual <- max(range_short) - min(range_short)
############################################################################################################

range_25 <- seq(from = 1991, to = 2016, by = 5)
range_110 <- c(1905, 1921, 1933, 1947, 1954, 1961, 1966, 1971,
          1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016)
range_125 <- c(1890, 1905, 1921, 1933, 1947, 1954, 1961, 1966,
           1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016)
ranges <- list(`25` = range_25, `110` = range_110, `125` = range_125)

for (gender in c("M", "F")) {
  file <- sprintf("qx_all_%s.csv", gender)
  data <- read_csv(file)
  data <- data %>% mutate(`1890` = (`1885` + `1895`)/2) %>% 
    select(-c(`1885`, `1895`)) %>% select(age, `1890`, everything()) %>% 
    gather(key = "year", value = "q_x", -age) # data %>% pivot_longer(-age, names_to = "year", values_to = "q_x")
  data$year <- as.numeric(data$year)

  fit_data <- tibble(age = NA, range = NA, MI = NA, poly_degree = NA, CV_1 = NA, CV_2 = NA) # adj_R_squared

  for (ages in 0:100) {
    for (range in names(ranges)) {
      for (deg in 2:5) {
         if (range == "25" & deg == 5) break
         else {
           fit <- data %>% filter(age == ages & year %in% ranges[[range]]) %>% 
           with(glm(q_x ~ poly(year, deg))) # encode year as like 0, 1, 2...?
           #adj_R_squared <- summary(fit)$adj.r.squared
           #AIC <- fit$aic
           set.seed(1)
           CV <- boot::cv.glm(data %>% filter(age == ages & year %in% ranges[[range]]), fit, K = 5)$delta
           predicted_qx <- predict(fit)
           MI <- (predicted_qx[length(ranges[[range]])]/predicted_qx[1])^(1/as.numeric(range)) - 1 # blah = range
           # get the crude MI's as well!!
           if (is.na(MI)) next
           else {
            new_row <- c(ages, range, MI, deg, CV[1], CV[2])
            fit_data <- rbind(fit_data, new_row)
           }
        }
      }
    }
  }

  fit_data <- fit_data %>% filter(complete.cases(fit_data)) # i don't want NA's
  write.csv(fit_data, sprintf("MI_fits_%s.csv", gender), row.names = F)
  
  # what are our final smoothed MI's for each range?
  for (rng in names(ranges)) {
    best_MI <- fit_data %>% filter(range == rng) %>% group_by(age) %>% filter(CV_2 == min(CV_2))
    write.csv(best_MI, sprintf("best_MI_fit_%s_%s.csv", gender, rng), row.names = F)
  }
}

# is adjusted R^2 the right thing to use here? maybe. is CV error better? not sure.

# here's a funny bug: you can't do for (age in 1:105) { ... data %>% filter(age == age) ... }
# i guess R gets confused when the iterate ("age") has the same name as the column ("age")





# 5 year plots
MI_qx <- tibble(age = NA, gender = NA, range = NA, MI = NA)
for (i in seq(from = 0, to = 50, by = 5)) {
  for (gender in c("M", "F")) {
    data <- read_csv(sprintf("qx_all_%s.csv", gender)) %>% gather(key = "year", value = "q_x", -age)
    df <- left_join(data %>% filter(year == 2016 - i), data %>% filter(year == 2016 - i -5), by = "age") %>% 
          mutate(MI = (q_x.x/q_x.y)^(1/5) - 1) %>% select(age, MI)
    # you could do pivot_wider() here i guess, or spread() in old tidyr
    df$gender <- gender
    df$range <- i/5 + 1
    MI_qx <- rbind(MI_qx, df)
  }
}
MI_qx <- MI_qx %>% filter(complete.cases(MI_qx))

for(ranges in 1:11) {
  MI_qx %>% filter(range == ranges) %>%
    ggplot(aes(x = age, y = MI, colour = as.factor(gender))) + geom_line() +
    labs(title = paste0(2016 - 5*ranges, " to ", 2016 - 5*(ranges - 1)), colour = "gender")
  ggsave(sprintf("MI_qx_%s.png", 2016 - 5*ranges))
}


MI_qx %>% filter(gender == "F", range <= 4) %>% 
  mutate(range = paste0(as.character(2016 - 5*range), " to ", as.character(2016 - 5*(range - 1)))) %>% 
  ggplot(aes(x = age, y = MI, colour = as.factor(range))) + geom_line() + labs(colour = "range")
ggsave("output/MI_qx_5_years_F.png")




