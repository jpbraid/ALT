### NOTE: the functions below assume that qx_data and MI_data exist in the global environment

max_analysis_year <- 2100
current_year <- 2016
# scenarios <- c()
qx_data <- read_csv("qx_all.csv") %>% select(-age)
MI_data <- read_csv("MI_all.csv")
max_age <- nrow(qx_data)/2

forecast_qx <- function(from_year, to_year, scenario = c(25, 125)) {
  MI_factors <- MI_data %>% filter(range == scenario, year == from_year) %>% select(MI) %>% unlist() # filter gender?
  qx <- qx_data %>% select(as.character(from_year)) %>% unlist()
  
  # make sure MI and qx are the same length or whatever
  # max_age <- blah
  if (length(qx_current) > length(MI_factors)) {
    MI_factors <- c(MI_factors, rep(0, length(qx_current) - length(MI_factors)))
  } else {
    qx_current <- qx_current[1:length(MI_factors)]
  }
  max_age <- length(qx_current) - 1
  
  qx_projected <- matrix(nrow = max_age + 1, ncol = to_year - from_year + 1)
  qx_projected[, 1] <- qx_current
  for (year in (current_year + 1):to_year) {
    index <- year - from_year + 1
    qx_projected[, index] <- qx_projected[, 1]*(1 + MI_factors)^(index - 1)
  }
  qx_projected <- as.data.frame(qx_projected)
  names(qx_projected) <- from_year:to_year
  
  return(qx_projected)
}

qx_projections_25 <- ## project up to max_analysis_year + max_age using MI_25 factors
qx_projections_125 <- ## as above using the MI_125 factors
qx_projections <- # just rbind these two
# if we had more than two scenarios, do a for loop
  
get_ex <- function(age, gender, year, scenario = c(25, 125), assumption = c("period", "cohort")) {
    if (assumption == "period") {
      qx <- qx_projections %>% filter(gender == gender, scenario == scenario) %>% select(as.character(year)) %>% as.matrix()
      px <- 1 - qx
      kpx <- px[(age + 1):nrow(px), ] %>% apply(2, cumprod)
      ex <- kpx %>% apply(2, sum) + 0.5
      return(ex)
    } else {
      print("not ready to handle cohort just yet")
      # basically we just need to extract the relevant submatrix from qx_projections and take the diagonal of that
    }
}

# set up initial params (single query)
gender <- "M"
age <- 65
analysis_year <- 2060
improvement_range <- 125

# or for larger loops set up vectors of params (we loop over gender and improvement_range in this case so no need to specify those)
analysis_ages <- c(0, 30, 65)
analysis_years <- c(2016, seq(from = 2020, to = 2060, by = 10))
