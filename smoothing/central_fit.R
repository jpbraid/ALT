library(tidyverse) 

# knot_iterate <- function(x, y, knots) etc.

# set up params
min_age <- 3
max_age <- 102
initial_knots_M <- c()
initial_knots_F <- c()

# load in raw mortality data
for (gender in c("M", "F")) {
  etr <- read_csv(sprintf("etr_ForR%s", gender)) %>% filter(age %in% min_age:max_age)
  # initial_knots <- 
}
