### NOTE: the functions below assume that qx_data and MI_data exist in the global environment

current_year <- 2016
max_analysis_year <- 2100
# scenarios <- c()

qx_data <- read_csv("qx_all.csv")
MI_data <- read_csv("MI_all.csv")
max_age <- nrow(qx_data)/2 - 1  # divide by 2 since M and F are stacked on top of each other... also assumes each are same length!
# Ohhhhhhhhh! that reminds me i need to do that thing where i extrapolate qx's for old ages and years < 1996!!!

forecast_qx <- function(gender, from_year, to_year, scenario = c(25, 125)) {
  MI_factors <- MI_data %>% filter(gender == UQ(gender), range == scenario, year == from_year) %>% select(MI) %>% unlist()
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
# for (scenario in c(25, 125)) for (gender in c("M", "F")) ... qx_proj <- blah qx_projections <- rbind(qx_projections, qx_proj)
qx_projections_25_M <- forecast_qx(sex = "M", from_year = current_year, to_year = max_analysis_year + max_age, scenario = 25) 
qx_projections_125_M <- forecast_qx(sex = "M", from_year = current_year, to_year = max_analysis_year + max_age, scenario = 125) 
qx_projections_25_F <- forecast_qx(sex = "F", from_year = current_year, to_year = max_analysis_year + max_age, scenario = 25) 
qx_projections_125_F <- forecast_qx(sex = "F", from_year = current_year, to_year = max_analysis_year + max_age, scenario = 125) 
qx_projections <- rbind(qx_projections_25_M, qx_projections_25_F, qx_projections_125_M, qx_projections_125_F)
qx_data_doubled <- rbind(qx_data, qx_data) %>% select(-"2016")
qx_projections <- cbind(qx_data_doubled, qx_projections) %>% select(age, gender, scenario, everything())
# be very careful about the rbind order here...

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
    kpx <- px %>% apply(2, cumprod)  # hnnn the cumprod is unnecessary here
    ex <- kpx %>% apply(2, sum) + 0.5
    return(ex)
  }
}


# set up initial params (single query)
gender <- "M"
age <- 65
analysis_year <- 2060
improvement_range <- 25
assumption <- "cohort"

get_ex(age = age, gender = gender, year = analysis_year, scenario = improvement_range, assumption = assumption)

# or for larger loops set up vectors of params (we loop over gender and improvement_range in this case so no need to specify those)
analysis_ages <- c(0, 30, 65)
analysis_years <- c(2016, seq(from = 2020, to = 2060, by = 10))

# do some loops and that's it 


# THEN do a kbarqx function
