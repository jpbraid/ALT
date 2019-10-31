library(tidyverse)

# the idea here is you choose the gender and range and try different values of dof until you find a fit that "looks" good
# can't say too much more than that really

gender <- "M"
range <- "short"
dof <- 30
max_spline_age <- 100
max_age <- 105

MI <- read_csv(sprintf("output/best_MI_fit_%s_%s.csv", gender, range))
fit_SS <- smooth.spline(x = MI$age[1:(max_spline_age + 1)], y = MI$MI[1:(max_spline_age + 1)], df = dof)
MI_predicted <- predict(fit_SS, x = (max_spline_age):max_age)$y
plot(MI$age, MI$MI)
points(fit_SS$x, fit_SS$y, col = "orange", lwd = 2, type = "l")
points((max_spline_age):max_age, MI_predicted, col = "orange", lwd = 2, type = "l")

df <- rbind(data.frame(age = fit_SS$x, MI_SS = fit_SS$y, dof = dof, spline_max = max_spline_age), 
            data.frame(age = max_spline_age:max_age, MI_SS = MI_predicted, df = df, spline_max = max_spline_age))
write.csv(df, sprintf("output/MI_SS_%s_%s.csv", gender, range), row.names = F) 
