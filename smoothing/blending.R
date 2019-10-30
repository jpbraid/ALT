library(tidyverse)

get_chisq <- function(actual = etr$deathtotal, exposure = etr$Cetrtotal, mx) {
  numerator <- (actual - exposure*mx)^2
  denominator <- exposure*mx*(1 - mx)
  chisq <- sum(numerator/denominator)
  return(chisq)
}

# the below is all for a single gender; i should make it extendable (will do so when all the other work is done)
etr <- read_csv("etr_ForRM.csv")
spline_fit <- read_csv("infant_plus_central_blend_males.csv")  # double check the name of this csv file
colnames(spline_fit)[3] <- "spline_mx"    # this should *technically* say "infant+spline mx" or some such
makeham <- read_csv("makeham_males_weighted.csv")
ranges <- colnames(makeham)[-c(1,2)]

# plot each makeham
for(range in ranges) {
  makeham_fit <- makeham %>% select(age, range)
  colnames(makeham_fit)[2] <- "makeham_mx"
  full_data <- full_join(spline_fit, makeham_fit, by = "age")
  plot(full_data$age, log(full_data$crudemx), main = paste("Makeham + spline fit for age range", range),
       xlab = "Age", ylab = "m_x")
  points(full_data$age, log(full_data$spline_mx), col = "deepskyblue", lwd = 2, type = "l")
  points(full_data$age, log(full_data$makeham_mx), col = "orange", lwd = 2, type = "l")
  Sys.sleep(2.5)
}

# for each makeham fit, look at each possible starting point for the blending; evaluate metrics
for(range in ranges) {
  makeham_fit <- read_csv("makeham_males_weighted.csv") %>% select(age, range)
  colnames(makeham_fit)[2] <- "makeham_mx"
  #plot(full_data$age, log(full_data$crudemx))
  #points(makeham_fit$age, log(makeham_fit$makeham_mx), col = "orange", lwd = 2, type = "l")
  
  # now do a full join on the spline fit and the makeham fit; this will be used to construct the blended fit
  full_data <- full_join(spline_fit, makeham_fit, by = "age")
  
  # look at each different possiblity for ages at which to start the blending
  blend_min <- stringr::str_sub(range, start = 1L, end = 2L)
  for (blend_start in blend_min:102) {
    blend_fit <- tibble(age = full_data$age, blended_mx = full_data$makeham_mx)
    blend_fit$blended_mx[1:blend_start] <- full_data$spline_mx[1:blend_start]
    factor <- full_data$spline_mx[blend_start+1]/full_data$makeham_mx[blend_start+1]
    blend_fit$blended_mx[(blend_start+1):109] <- blend_fit$blended_mx[(blend_start+1):109]*factor
    plot(full_data$age, log(full_data$crudemx))
    points(blend_fit$age, log(blend_fit$blended_mx), col = "orange", lwd = 2, type = "l")
    plot(blend_fit$age[blend_min:109], log(blend_fit$blend[blend_min:109]))
    
    #calculate the metrics; basically check that chisq is OK, then do sum(actual - expected)
    print(get_chisq(actual = etr$deathtotal[3:106], exposure = etr$Cetrtotal[3:106], mx = blend_fit$blended_mx[3:106]))
    print(get_chisq(actual = etr$deathtotal[3:101], exposure = etr$Cetrtotal[3:101], mx = blend_fit$blended_mx[3:101]))
    print(sum(etr$deathtotal[3:106] - etr$Cetrtotal[3:106]*blend_fit$blended_mx[3:106]))
    print(sum(etr$deathtotal[3:101] - etr$Cetrtotal[3:101]*blend_fit$blended_mx[3:101]))
  }
  
}


#### TEST A SINGLE MAKEHAM FIT ####

# for males, the range 82-108 seems to work [starting the blending at 90]
makeham_fit <- read_csv("makeham_males_weighted.csv") %>% select(age, `82-108`)
colnames(makeham_fit)[2] <- "makeham_mx"
full_data <- full_join(spline_fit, makeham_fit, by = "age")
blend_start <- 90 # anything from min age [=85] to 102
blend_fit <- tibble(age = full_data$age, blended_mx = full_data$makeham_mx)
blend_fit$blended_mx[1:blend_start] <- full_data$spline_mx[1:blend_start]
factor <- full_data$spline_mx[blend_start+1]/full_data$makeham_mx[blend_start+1]
blend_fit$blended_mx[(blend_start+1):109] <- blend_fit$blended_mx[(blend_start+1):109]*factor
plot(full_data$age, log(full_data$crude_mx), main = "Blended curve (males)", xlab = "Age", ylab = "m_x")
points(blend_fit$age, log(blend_fit$blended_mx), col = "orange", lwd = 2, type = "l")
#plot(blend_fit$age[blend_min:109], log(blend_fit$blend[blend_min:109]))
#calculate the metrics; basically check that chisq is OK, then do sum(actual - expected)
print(get_chisq(actual = etr$deathtotal[3:106], exposure = etr$Cetrtotal[3:106], mx = blend_fit$blended_mx[3:106]))
print(get_chisq(actual = etr$deathtotal[3:101], exposure = etr$Cetrtotal[3:101], mx = blend_fit$blended_mx[3:101]))
print(sum(etr$deathtotal[3:106] - etr$Cetrtotal[3:106]*blend_fit$blended_mx[3:106]))
print(sum(etr$deathtotal[3:101] - etr$Cetrtotal[3:101]*blend_fit$blended_mx[3:101]))

# whichever blended curve we think is best we write as CSV:
write.csv(blend_fit, "blended_curve_males.csv", row.names = F)
