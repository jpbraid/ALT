library(tidyverse)
library(pcaPP)

# pull in the raw mortality data from each year
# note: this analysis is only for a single population, so we select e.g. just the male mortality rates
mortality_data <- read_csv("qx_all.csv") %>% filter(gender == "M")
