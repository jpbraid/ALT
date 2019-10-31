forecast_qx <- function(gender, current_year, max_year, scenario = c(25, 125)) {
  # read in qx data for current year, and the relevant MI factors
  qx_current <- read_csv(sprintf("qx_all_interpolated_%s.csv", gender)) %>% 
    select(as.character(current_year)) %>% unlist()
  MI_factors <- read_csv(sprintf("MI_improvement_%s%_%s.csv", scenario, gender)) %>% 
    select(MI) %>% unlist()
  
  # we now have two simple vectors of the same length: qx for the current year,
  # and the MI_x for each age
  
  # if the max age is different in each data set, we have to use the smaller of the two
  max_age <- min(length(qx_current), length(MI_factors)) - 1
  qx_current <- qx_current[1:(max_age + 1)]
  MI_factors <- MI_factors[1:(max_age + 1)]
  
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

# set up initial params
gender <- "M"
current_year <- 2016
max_year <- 2065
improvement_range <- 125

# crank out the corresponding projected qx's
forecast_qx(gender = gender, current_year = current_year, max_year = max_year, scenario = improvement_range)

# calculate period ex using kpx.R

# calculate cohort ex by: 
# 1. calculating projected qx's, 
# 2. getting "diagonal" qx's, 
# 3. calculating diagonal kpx's, and
# 4. summing up the kpx's
cohort_ex <- function(gender, current_age, start_year, current_year, scenario = c(25, 125)) {
  max_year <- start_year + max_age - current_age - 1 # ??
  qx_projected <- forecast_qx(gender = gender, current_year = current_year, max_year = max_year, scenario = scenario)
  
  diagonal_qx <- diag(qx_projected) #sorta
  diagonal_px <- 1 - diagonal_qx
  diagonal_kpx <- cumprod(diagonal_px)
  
  e_x <- sum(diagonal_kpx)
  #done
}
