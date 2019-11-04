library(tidyverse)

# the idea here is you choose the gender and range and try different values of df until you find a fit that "looks" good
# can't say too much more than that really
# the user has to eyeball the plots and choose df based on the general appearance, so it's hard to automate any of this

gender <- "M"
range <- 25
df <- 20
max_spline_age <- 100

MI <- read_csv(sprintf("output/best_MI_fit_%s_%s.csv", gender, range))
fit_SS <- smooth.spline(x = MI$age[1:(max_spline_age + 1)], y =MI$MI[1:(max_spline_age + 1)], df = df)
MI_predicted <- predict(fit_SS, x = (max_spline_age):105)$y

# set anything > 0  in the tail end to 0

plot(MI$age, MI$MI)
points(fit_SS$x, fit_SS$y, col = "orange", lwd = 2, type = "l")
points((max_spline_age):105, MI_predicted, col = "orange", lwd = 2, type = "l")
#plot(fit_SS$x, fit_SS$y*100, col = "orange", type = "l")

df <- rbind(data.frame(age = fit_SS$x[1:max_spline_age], MI_SS = fit_SS$y[1:max_spline_age], df = df, spline_max = max_spline_age), 
            data.frame(age = max_spline_age:105, MI_SS = MI_predicted, df = df, spline_max = max_spline_age))

write.csv(df, sprintf("MI_smoothed_%s_%s.csv", gender, range), row.names = F) 
