library(tidyverse)

qx_data <- read_csv("qx_all_M.csv") %>% select(-age) # maybe sort by age first
px_data <- 1 - qx_data # note row i corresponds to age i - 1

# calculate the survival probabilities kpx for each life table
kpx <- vector(mode = "list", length = nrow(px_data))
names(kpx) <- 1:nrow(px_data)
for(i in 1:nrow(px_data)) {
  kpx[[i]] <- rbind(1, cumprod(px_data[i:nrow(px_data), ]))
}
# kpx[[i]] = {k_p_(i-1)} for k = 0, 1, ... 

# now calculate k|q_x = kpx*q_k
k_bar_qx <- vector(mode = "list", length = nrow(px_data))
k_bar_qx[[1]] <- kpx[[1]][-nrow(kpx[[1]]), ]*qx_data
for (i in 2:nrow(px_data)) {
  k_bar_qx[[i]] <- kpx[[i]][-nrow(kpx[[i]]), ]*qx_data[-(1:(i - 1)), ]
}

# knowing k|q_x we can calculate the curtate future lifetime at various ages
# (actually we can also use the kpx's for that, using a little probability trick, BUT...)
# (k|q_x is good to know, because it tells us the distribution (pmf) of K_x)
e_0 <- apply(k_bar_qx[[1]] * (0:120), 2, sum) + 0.5
e_65 <- apply(k_bar_qx[[66]] * (65:120), 2, sum) + 0.5

# plot the distribution of K_0 and K_65 for various calendar years
# (to be continued)

# INTERPOLATE QX DATA
# SORT DATAFRAMES BY COLUMN FIRST! (safety)
qx_data <- read_csv("qx_all_M.csv") %>% select(-age)
years_in_data <- qx_data %>% names() %>% as.numeric %>% sort()
all_years <- seq(from = min(years_in_data), to = max(years_in_data))

# lmk if there's a better way to do the below
qx_all <- as.data.frame(matrix(nrow = nrow(qx_data), ncol = length(all_years)))
names(qx_all) <- as.character(all_years)

# now i want to: (1) put the data that already exists into qx_all
for (i in all_years) {
  if (i %in% years_in_data) qx_all[, as.character(i)] <- qx_data[, as.character(i)]
}

# and then (2) interpolate the rest of the data
for (i in all_years) {
  if (!(i %in% years_in_data)) {
    j <- max(years_in_data[years_in_data < i])
    k <- min(years_in_data[years_in_data > i])
    qx_all[, as.character(i)] <- qx_all[, as.character(i - 1)] - (qx_all[, as.character(j)] - qx_all[, as.character(k)])/(k - j)
  }
}

# i guess i should combine these two for loops into an if-else loop
# then again, that would involve constantly reassigning values to the same column!!
