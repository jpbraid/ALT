library(tidyverse) 

# knot_iterate <- function(x, y, knots) etc.
# eval_knots

# set up params; initial knots are user-defined and may require some tinkering with
min_age <- 3
max_age <- 102
initial_knots_M <- c(10, 20, 30)
initial_knots_F <- c(30, 40, 50)

# load in "raw" mortality data
for (gender in c("M", "F")) {
  etr <- read_csv(sprintf("etr_ForR%s", gender)) %>% filter(age %in% min_age:max_age)
  final_knots <- knot_iterate(x = etr$age, y = etr$crude_mx, knots = initial_knots) # etc
  spline_mx <- eval_knots(final_knots)
  # do a full join and write to CSV
}
