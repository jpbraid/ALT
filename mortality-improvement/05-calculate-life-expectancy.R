# calculate forecasted qx values based on chosen scenario, up to max_year
forecast_qx <- function(gender, current_year, max_year, scenario = c(25, 125)) {
  # read in qx data for current year and the relevant MI factors
  qx_current <- read_csv(sprintf("qx_all_interpolated_%s.csv", gender)) %>% 
    select(as.character(current_year)) %>% unlist()
  MI_factors <- read_csv(sprintf("MI_smoothed_%s_%g.csv", gender, scenario)) %>% 
    select(MI) %>% unlist()
  
  # if the max age is different in each data set, we have to use the smaller of the two
  # actually i need to do something a bit more sophisticated here i think
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
  return(e_x)
}

# calculate cohort e_x by: 
# 1. calculating projected qx's, 2. getting the "diagonal" , 3. etc.
cohort_ex <- function(gender, current_age, current_year, start_year, scenario = c(25, 125)) {
  max_year <- start_year + max_age - current_age - 1 # ??
  qx_projected <- forecast_qx(gender = gender, current_year = current_year, max_year = max_year, scenario = scenario)
  
  diagonal_qx <- diag(qx_projected) #sorta
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

# crank out the corresponding period e_x
period_ex <- period_ex (gender = gender, current_year = current_year, period_year = period_year, scenario = improvement_range)


## this is all good, just need to think about:
## 1. naming convention for MI files (prolly will have to change to MI_smoothed_M_125.csv etc)
## 2. how to handle the "max_age" when calculating projected qx / period ex
## 3. also the whole max_age thing for cohort ex
## oh there's also a subtlety in that the MI factors (line 6) should really be linked to the current_year 
## (since they change from LT to LT)
