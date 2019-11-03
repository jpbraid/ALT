### NOTE: the functions below assume that qx_data and MI_data exist in the global environment
library(tidyverse)

current_year <- 2016 # we start the projections from this year
max_analysis_year <- 2100
scenarios <- c(short = 25, long = 125)

qx_data <- read_csv("qx_all.csv") # qx_all_interpolated
MI_data <- read_csv("MI_all.csv")
max_age <- max(qx_data$age) # the maximum possible age i could consider; this well tell me how far out to project beyond the max_analysis_year

forecast_qx <- function(gender, from_year, to_year, MI_year, scenario = c(25, 125)) {
  MI_factors <- MI_data %>% filter(gender == UQ(gender), range == scenario, year == MI_year) %>% select(MI) %>% unlist()
  qx_start <- qx_data %>% filter(gender == UQ(gender)) %>% select(as.character(from_year)) %>% unlist()
  
  # make sure MI and qx are the same length or whatever
  # max_age <- blah
  if (length(qx_start) > length(MI_factors)) {
    MI_factors <- c(MI_factors, rep(0, length(qx_start) - length(MI_factors)))
  } else {
    qx_start <- qx_start[1:length(MI_factors)] # uhh should this be the other way around?
  }
  max_age <- length(qx_start) - 1
  
  # build up projected qx's
  qx_projected <- matrix(nrow = max_age + 1, ncol = to_year - from_year + 2)
  qx_projected[, 1] <- qx_start
  for (year in (from_year + 1):to_year) {
    index <- year - from_year + 1
    qx_projected[, index] <- qx_projected[, 1]*(1 + MI_factors)^(index - 1)
  }
  qx_projected[, to_year - from_year + 2] <- scenario
  qx_projected <- as.data.frame(qx_projected)
  names(qx_projected) <- c(from_year:to_year, "scenario")
  
  return(qx_projected)
}

# now calculate the projected qx's
qx_projections <- as.data.frame(matrix(nrow = 0, ncol = max_analysis_year + max_age - current_year + 1))
names(qx_projections) <- as.character(current_year:(max_analysis_year + max_age))
for (scenario in c(25, 125)) {
  for (gender in c("M", "F")) {
    qx <- forecast_qx(gender = gender, from_year = current_year, 
                      to_year = max_analysis_year + max_age, MI_year = current_year, 
                      scenario = scenario) 
    write.csv(qx, sprintf("qx_projections_%s_%g.csv", gender, scenario), row.names = F)
    qx_projections <- rbind(qx_projections, qx)
  }
}
qx_data_doubled <- rbind(qx_data, qx_data) %>% select(-"2016")
qx_projections <- cbind(qx_data_doubled, qx_projections) %>% select(age, gender, scenario, everything())
# BETTER: add gender to the forecast_qx() output; double the data according to scenario; do a full join on (gender, scenario)

get_ex <- function(age, gender, year, scenario = c(25, 125), assumption = c("period", "cohort")) {
  if (assumption == "period") {
    qx <- qx_projections %>% filter(gender == UQ(gender), scenario == UQ(scenario)) %>% select(as.character(year)) %>% as.matrix()
    px <- 1 - qx
    kpx <- px[(age + 1):nrow(px), ] %>% as.matrix() %>% apply(2, cumprod)
    ex <- kpx %>% apply(2, sum) + 0.5
    return(ex)
  } else {
    # basically we just need to extract the relevant submatrix from qx_projections and take the diagonal of that
    qx_submatrix <- qx_projections %>% filter(gender == UQ(gender), scenario == UQ(scenario)) %>% as.matrix()
    max_age <- nrow(qx_submatrix) - 1
    years <- year:(year + max_age - age) %>% as.character()
    qx_submatrix <- qx_submatrix %>% as.data.frame() %>% select(years) %>% as.matrix()
    qx_submatrix <- qx_submatrix[(age + 1):(max_age + 1), ]
    qx <- diag(qx_submatrix) %>% as.numeric() %>% as.matrix()
    px <- 1 - qx
    kpx <- px %>% apply(2, cumprod)  # hnnn the cumprod is unnecessary here (unless year actually is longer than 1...)
    ex <- kpx %>% apply(2, sum) + 0.5
    return(ex)
  }
}

# set up initial params (single query)
gender <- "F"
age <- 0
analysis_year <- 2060
improvement_range <- 25
assumption <- "cohort"

get_ex(age = age, gender = gender, year = analysis_year, scenario = improvement_range, assumption = assumption)

# or for larger loops set up vectors of params
# TABLES
analysis_ages <- c(0, 30, 65)
analysis_years <- c(2016, seq(from = 2020, to = 2060, by = 10))

table_data <- data.frame(gender = NA, analysis_year = NA, current_age = NA, scenario = NA, assumption = NA, e_x = NA)
for (gender in c("M", "F")) {
  for (scenario in c(25, 125)) {
    for (age in analysis_ages) {
      for (year in analysis_years) {
        for (assumption in c("period", "cohort")) {
          e_x <- get_ex(age = age, gender = gender, year = year, scenario = scenario, assumption = assumption)
          new_row <- data.frame(gender = gender, analysis_year = year, current_age = age, 
                                scenario = scenario, assumption = assumption, e_x = e_x)
          table_data <- rbind(table_data, new_row)
        }
      }
    }
  }
}
table_data <- table_data %>% filter(complete.cases(table_data)) %>% mutate(life_exp = current_age + e_x)

# figure 12 is pretty straightforward
analysis_years <- seq(from = 2016, to = 2066, by = 5)

projected_life_exp <- data.frame(gender = NA, analysis_year = NA, scenario = NA, e_x = NA)
for (gender in c("M", "F")) {
  for (scenario in c(25, 125)) {
    for (year in analysis_years) {
      e_x <- get_ex(age = 0, gender = gender, year = year, scenario = scenario, assumption = "period")
      new_row <- data.frame(gender = gender, analysis_year = year, scenario = scenario, e_x = e_x)
      projected_life_exp <- rbind(projected_life_exp, new_row)
    }
  }
}

# get the "past" ones as well

# figure 13
analysis_ages <- 0:100
analysis_year <- current_year

cohort_life_expectancy <- data.frame(gender = NA, age = NA, scenario = NA, assumption = NA, e_x = NA)
for (gender in c("M", "F")) {
  for (age in analysis_ages) {
    for (scenario in c(25, 125)) {
      for (assumption in c("period", "cohort")) {
        e_x <- get_ex(age = age, gender = gender, year = analysis_year, scenario = scenario, assumption = assumption)
        new_row <- data.frame(gender = gender, age = age, scenario = scenario, assumption = assumption, e_x = e_x)
        cohort_life_expectancy <- rbind(cohort_life_expectancy, new_row)
      }
    }
  }
}


# THEN do a kbarqx function
# we get the relevant qx values (for every age); then for each k find kbarqx...

get_kbar_qx <- function(age, gender, year, scenario = c(25, 125), assumption = c("period", "cohort")) {
  if (assumption == "period") {
    qx <- qx_projections %>% filter(gender == UQ(gender), scenario == UQ(scenario)) %>% select(as.character(year)) %>% unlist()
    px <- 1 - qx
    kpx <- cumprod(px)
    kbar_qx <- lag(kpx, default = 1) * qx
    print(kbar_qx)
    # then calculate as normal
  } else {
    # extract the relevant submatrix
    qx_submatrix <- qx_projections %>% filter(gender == UQ(gender), scenario == UQ(scenario)) %>% as.matrix()
    max_age <- nrow(qx_submatrix) - 1
    years <- year:(year + max_age - age) %>% as.character()
    qx_submatrix <- qx_submatrix %>% as.data.frame() %>% select(years) %>% as.matrix()
    qx_submatrix <- qx_submatrix[(age + 1):(max_age + 1), ]
    print(qx_submatrix)
    qx <- diag(qx_submatrix) %>% as.numeric() %>% unlist()
    px <- 1 - qx
    kpx <- cumprod(px)
    kbar_qx <- lag(kpx, default = 1) * qx
    print(kbar_qx)
  }
}

get_kbar_qx(age = 65, gender = "M", year = 2016, scenario = 25, assumption = "cohort")


## NOTE: assumption cohort is incompatible with analysis_year < current_year (at the moment)
## if you want to do a cohort calculation for an earlier year, you need to change the current_year at the top, and recalculate the qx_projections
## (because the projections are based on MI data **from the current year**)
## hmm this is a bit subtle. do you WANT to calculate cohort life expectancy from 2011 USING the 2011 MI factors? or... would you rather use the
## actual qx's from 2011 to 2016, and then use the 2016 MI factors to project qx's from there?? the latter seems to make more sense
## other thing is i've tried to do this idea where you can grab multiple ex's at once by passing the function a list of years
## but i don't think this works yet

## PLAN: paste the MI_factors (2016+2011) together into a single long dataframe
## run all the above (maybe write the qx_projections to csv)
## then change MI_year to 2011 in the qx_projections, and calculate kbarqx there
## then you're done
