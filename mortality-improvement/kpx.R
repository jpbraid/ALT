library(tidyverse)

# load in qx data from all life tables and calculate corresponding px's
qx_data <- read_csv("qx_all_interpolated_M.csv") # maybe sort this by age
max_age <- max(qx_data$age)
qx_data <- qx_data %>% select(-age)
px_data <- 1 - qx_data

# calculate the survival probabilities kpx for each life table
# note that kpx[[i]] tells us {k_p_(i-1)} for k = 0, 1, ... (row i corresponds to age i - 1)
kpx <- vector(mode = "list", length = nrow(px_data))
names(kpx) <- 1:nrow(px_data)
for(i in 1:nrow(px_data)) {
  kpx[[i]] <- rbind(1, cumprod(px_data[i:nrow(px_data), ]))
}

# now calculate k|q_x = kpx*q_{x+k}
k_bar_qx <- vector(mode = "list", length = nrow(px_data))
k_bar_qx[[1]] <- kpx[[1]][-nrow(kpx[[1]]), ]*qx_data
for (i in 2:nrow(px_data)) {
  k_bar_qx[[i]] <- kpx[[i]][-nrow(kpx[[i]]), ]*qx_data[-(1:(i - 1)), ]
}

# knowing k|q_x we can calculate the curtate future lifetime at various ages
#(actually we can also use the kpx's for that, BUT...)
e_0 <- apply(k_bar_qx[[1]] * (0:max_age), 2, sum) + 0.5
e_65 <- apply(k_bar_qx[[66]] * (65:max_age), 2, sum) + 0.5 # max_age + 1 ??? i think so
# more correct would be e_x <- apply(k_bar_qx[[x]] * (0:blah), 2, sum) + 0.5
# or just do e_x <- apply(kpx[[1]][-1], 2, sum) + 0.5


### WARNING: the above only works for years < 1996 or ages < 100 (inclusive or)
### basically, that's because sum kpx q_{x+k} does NOT equal 1 for old x
### (technically, it *never* equals 1, even for young x, because the recent life tables don't have a final age with q_x = 1;
### nonetheless, it gets pretty damn close for x < 100)


# plot the distribution of K_0 and K_65 for various calendar years
# (to be continued)
