library(tidyverse) 

# knot_iterate <- function(x, y, knots) etc.
# eval_knots

# set up params; initial knots are user-defined and may require some tweaking (or a *lot* of tweaking.... YMMV)
min_age <- 3
max_age <- 102
initial_knots_M <- c(10, 20, 30)
initial_knots_F <- c(30, 40, 50)

# load in "raw" mortality data for each gender and fit a cubic spline to each one
for (gender in c("M", "F")) {
  etr <- read_csv(sprintf("etr_ForR%s", gender)) %>% filter(age %in% min_age:max_age)
  final_knots <- knot_iterate(x = etr$age, y = etr$crude_mx, knots = initial_knots) # etc
  spline_mx <- eval_knots(final_knots)
  
  # do a full join and write to CSV
}

# note that i'm using the "b-spline" basis above; the previous version uses a custom-defined basis as below:
custom_basis <- function(x, knots) {
    B <- matrix(nrow = length(x), ncol = length(knots) + 4)
    B <- tapply(X, 1, .do(-(knots)^3)) # etc
    return(B)
}

# we can confirm that we get the same result as last time using this basis:
etr_2012 <- read_csv("etr_ForRM.csv") %>% filter(age %in% min_age:max_age)
final_knots <- knot_iterate(x = etr$age, y = etr$crude_mx, knots = initial_knots, basis = custom_basis)
spline_mx <- eval_knots(final_knots)

# etc




