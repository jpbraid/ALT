library(tidyverse)

# the idea here is you choose the gender and range and try different values of df until you find a fit that "looks" good
# can't say too much more than that really
# the user has to eyeball the plots and choose df based on the general appearance, so it's hard to automate any of this

MI <- read_csv("output/best_MI_fit_F_125.csv")
fit_SS <- smooth.spline(x = MI$age, y = MI$MI, df = 30)

plot(MI$age, MI$MI*100)
points(fit_SS$x, fit_SS$y*100, col = "orange", lwd = 2, type = "l")
#plot(fit_SS$x, fit_SS$y*100, col = "orange", type = "l")

df <- data.frame(age = fit_SS$x, MI_SS = fit_SS$y)

write.csv(df, "MI_SS_F_25.csv", row.names = F) #just be careful to change the gender and range here
