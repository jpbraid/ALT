# calculate forecasted qx values based on chosen scenario, up to max_year
forecast_qx <- function(gender, current_year, max_year, scenario = c(25, 125)) {
  # read in qx data for current year and the relevant MI factors
  qx_current <- read_csv(sprintf("qx_all_%s.csv", gender)) %>% 
    select(as.character(current_year)) %>% unlist()
  MI_factors <- read_csv(sprintf("output/MI_smoothed_%s_%g.csv", gender, scenario)) %>% 
    select(MI_SS) %>% unlist()
  
  # compel the two vectors to have the same length
  if (length(qx_current) > length(MI_factors)) {
    MI_factors <- c(MI_factors, rep(0, length(qx_current) - length(MI_factors)))
  } else {
    qx_current <- qx_current[1:length(MI_factors)]
  }
  max_age <- length(qx_current) - 1
  
  # now build up the projected qx's by multiplying by the relevant MI factor
  qx_projected <- matrix(nrow = max_age + 1, ncol = max_year - current_year + 1)
  qx_projected[, 1] <- qx_current
  for (year in (current_year + 1):max_year) {
    index <- year - current_year + 1
    qx_projected[, index] <- qx_projected[, 1]*(1 + MI_factors)^(index - 1)
  }
  qx_projected <- as.data.frame(qx_projected)
  names(qx_projected) <- current_year:max_year
  
  return(qx_projected)
  #write.csv(qx_projected, sprintf("qx_projected_%s_%s.csv", gender, scenario), row.names = F)
}

# calculate period e_x 
period_ex <- function(gender, current_age, current_year, period_year, scenario = c(25, 125)) {
  # we first project the qx's out to the year of interest 
  qx_projected <- forecast_qx(gender = gender, current_year = current_year, max_year = period_year, scenario = scenario)
  
  # the relevant qx's will be in the last column of the projected qx's
  period_qx <- qx_projected[, ncol(qx_projected)] %>% unlist()
  period_px <- 1 - period_qx
  
  # do the standard cumprod() on px's to find kpx, then sum to get e_x
  kpx <- cumprod(period_px[(current_age + 1):length(period_px)])
  e_x <- sum(kpx)
  print(e_x)
  return(e_x)
}

# calculate cohort e_x by: 
# 1. calculating projected qx's, 2. getting the "diagonal" , 3. etc.
cohort_ex <- function(gender, current_age, current_year, start_year, scenario = c(25, 125)) {
  # the key step is working out the max_year; it basically depends on the start_year, current_age, and max_age
  qx_current <- read_csv(sprintf("qx_all_%s.csv", gender)) %>% 
    select(as.character(current_year)) %>% unlist()
  MI_factors <- read_csv(sprintf("output/MI_smoothed_%s_%g.csv", gender, scenario)) %>% 
    select(MI_SS) %>% unlist()
  
  max_age <- ifelse(length(qx_current) > length(MI_factors), length(qx_current) - 1, length(MI_factors) - 1)
  max_year <- start_year + max_age - current_age
  qx_projected <- forecast_qx(gender = gender, current_year = current_year, max_year = max_year, scenario = scenario)
  
  # we want to look at qx's starting from the start_year and going up to the max_year
  # also the rows of the qx submatrix need to start at the current age and go up to the max age
  row_start <- current_age + 1
  row_end <- max_age + 1
  col_start <- start_year - current_year + 1
  col_end <- max_year - current_year + 1
  qx_submatrix <- as.matrix(qx_projected[row_start:row_end, col_start:col_end])
  diagonal_qx <- diag(qx_submatrix)
  diagonal_px <- 1 - diagonal_qx
  diagonal_kpx <- cumprod(diagonal_px)
  
  e_x <- sum(diagonal_kpx)
  #done
}

# set up initial params
gender <- "M"
current_age <- 65
current_year <- 2016
period_year <- 2060
improvement_range <- 125

# crank out the corresponding period e_x and cohort e_x
period_ex <- period_ex (gender = gender, current_age = current_age, current_year = current_year, period_year = period_year, scenario = improvement_range)
cohort_ex <- cohort_ex(gender = gender, current_age = current_age, current_year = current_year, start_year = period_year, scenario = improvement_range)

max_year_analysis <- 2020
max_age_analysis <- 4

# TABLES

analysis_years <- c(2016, seq(from = 2020, to = 2060, by = 10))
analysis_ages <- c(0, 30, 65)

df <- data.frame(gender = NA, improvement_range = NA, current_age = NA, start_year = NA, current_year = NA, e_x = NA, assumption = NA)

for(gender in (c("M", "F"))) {
  for (scenario in c(25, 125)) {
    for (year in analysis_years) {
      for (age in analysis_ages) {
          for (assumption in c("cohort", "period")) {
            if (assumption == "cohort") {
              ex <- cohort_ex(gender = gender, current_age = age, current_year = current_year, start_year = year, scenario = scenario)
            } else {
              ex <- period_ex(gender = gender, current_age = age, current_year = current_year, period_year = year, scenario = scenario)
            }
            
            new_row <- data.frame(gender = gender, improvement_range = scenario, current_age = age, start_year = year, 
                                  current_year = current_year, e_x = ex, assumption = assumption)
            df <- rbind(df, new_row)
          }
      }
    }
  }
} 

df %>% filter(complete.cases(df)) %>% 
       mutate(life_exp = e_x + current_age) %>% 
       write.csv("output/life_expectancy_tables.csv", row.names = F)


# FIGURE 12

analysis_years <- seq(from = 2016, to = 2066, by = 5)

df <- data.frame(gender = NA, scenario = NA, current_year = NA, start_year = NA, current_age = NA, e_x = NA)

for (gender in c("M", "F")) {
  for (year in analysis_years) {
    if (year > current_year) {
      for (scenario in c(25, 125)) {
        ex <- period_ex(gender = gender, current_age = 0, current_year = current_year, period_year = year, scenario = scenario)
        new_row <- data.frame(gender = gender, scenario = scenario, current_year = current_year, start_year = year, current_age = 0, e_x = ex)
        df <- rbind(df, new_row)
      }
    } else {
      ex <- period_ex(gender = gender, current_age = 0, current_year = year, period_year = year, scenario = 25)
      new_row <- data.frame(gender = gender, scenario = 0, current_year = current_year, start_year = year, current_age = 0, e_x = ex)
      df <- rbind(df, new_row)
    }
  }
}

df <- df %>% mutate(life_exp = current_age + e_x) %>% filter(complete.cases(df))
write.csv(df, "output/figure12.csv", row.names = F)


# FIGURE 13

df <- data.frame(gender = NA, scenario = NA, current_year = NA, current_age = NA, e_x = NA)

for (gender in c("M", "F")) {
  for (scenario in c(0, 25, 125)) {
    for (age in 0:100) {
      if (scenario == 0) {
        ex <- period_ex(gender = gender, current_age = age, current_year = current_year, period_year = current_year, scenario = 25)
      } else {
        ex <- cohort_ex(gender = gender, current_age = age, current_year = current_year, start_year = current_year, scenario = scenario)
      }
      new_row <- data.frame(gender = gender, scenario = scenario, current_year = current_year, current_age = age, e_x = ex)
      df <- rbind(df, new_row)
    }
  }
}

df <- df %>% mutate(life_exp = current_age + e_x) 
df %>% filter(complete.cases(df)) %>% write.csv("output/figure13.csv", row.names = F)


## FIGURE 14?


## this is all good, just need to think about:
## 1. naming convention for MI files: assumed MI_smoothed_M_125.csv
## 2. how to work out max_age within cohort_ex function (WITHOUT just reading in the qx_all and MI_factors data again)
## oh there's also a subtlety in that the MI factors (line 6) should really be linked to the current_year 
## (since they change from LT to LT)
## *sigh* rename column name from MI_SS to.. MI_smoothed or something
