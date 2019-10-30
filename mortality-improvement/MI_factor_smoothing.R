library(tidyverse)

# the idea here is you choose the gender and range and try different values of df until you find a fit that "looks" good
# can't say too much more than that really
# the user has to eyeball the plots and choose df based on the general appearance, so it's hard to automate any of this

gender <- "M"
range <- "short"
dof <- 30

MI <- read_csv(sprintf("output/best_MI_fit_%s_%s.csv", gender, range)
fit_SS <- smooth.spline(x = MI$age, y = MI$MI, df = dof)
plot(MI$age, MI$MI*100)
points(fit_SS$x, fit_SS$y*100, col = "orange", lwd = 2, type = "l")
#plot(fit_SS$x, fit_SS$y*100, col = "orange", type = "l")

df <- data.frame(age = fit_SS$x, MI_SS = fit_SS$y)
write.csv(df, sprintf("MI_SS_%s_%s.csv", gender, range), row.names = F)
