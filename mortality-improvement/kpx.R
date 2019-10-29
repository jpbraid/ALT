library(tidyverse)

# load in qx data from all life tables and calculate corresponding px's
qx_data <- read_csv("qx_all_interpolated_M.csv") 
# maybe sort this by age
max_age <- max(qx_data$age)
qx_data <- qx_data %>% select(-age)
px_data <- 1 - qx_data

# calculate the survival probabilities kpx for each life table
kpx <- vector(mode = "list", length = nrow(px_data))
names(kpx) <- 1:nrow(px_data)
for(i in 1:nrow(px_data)) {
  kpx[[i]] <- rbind(1, cumprod(px_data[i:nrow(px_data), ]))
}
# note that kpx[[i]] tells us {k_p_(i-1)} for k = 0, 1, ... (row i corresponds to age i - 1)

# now calculate k|q_x = kpx*q_k
k_bar_qx <- vector(mode = "list", length = nrow(px_data))
k_bar_qx[[1]] <- kpx[[1]][-nrow(kpx[[1]]), ]*qx_data
for (i in 2:nrow(px_data)) {
  k_bar_qx[[i]] <- kpx[[i]][-nrow(kpx[[i]]), ]*qx_data[-(1:(i - 1)), ]
}

# knowing k|q_x we can calculate the curtate future lifetime at various ages^
e_0 <- apply(k_bar_qx[[1]] * (0:max_age), 2, sum) + 0.5
e_65 <- apply(k_bar_qx[[66]] * (65:max_age), 2, sum) + 0.5
# NOTE: to calculate e_x for early life tables we'll need to manually impute the missing qx's for old ages (cf. the notes)

# plot the distribution of K_0 and K_65 for various calendar years
# (to be continued)

# ^actually we can also use the kpx's for that, using a little probability trick, BUT...
# k|q_x is good to know, because it tells us the distribution (pmf) of K_x
