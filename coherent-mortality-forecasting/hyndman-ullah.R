library(tidyverse)
library(pcaPP)

# pull in the raw mortality data from each year
# note: this analysis is only for a single population, so we select e.g. just the male mortality rates
mortality_data <- read_csv("mx_all.csv") %>% 
                  filter(gender == "M") %>% 
                  gather(key = "year", value = "m_x", -c(age, gender))

# look at the change in shape over the years
mortality_data %>% ggplot(aes(x = age, y = m_x, colour = as.factor(year))) + 
                    geom_line() + 
                    labs(colour = "year")

# so here rather than for-ing through years we can use broom / apply logic to fit all the models at once
for (year in unique(mortality_data$year)) {
  mortality_subset <- mortality_data %>% filter(year == !!year) %>% mutate(variance = CETR/(m_x*(1 - m_x)))
  # fit a weighted regression spline
 }

