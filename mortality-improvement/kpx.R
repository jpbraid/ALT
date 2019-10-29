library(tidyverse)

qx_data <- read_csv("qx_all_M.csv") %>% select(-age) # maybe sort by age first
px_data <- 1 - qx_data # note row i corresponds to age i - 1

kpx <- vector(mode = "list", length = nrow(px_data))
names(kpx) <- 1:nrow(px_data)
for(i in 1:nrow(px_data)) {
  kpx[[i]] <- rbind(1, cumprod(px_data[i:nrow(px_data), ]))
}
# kpx[[i]] basically tells me k_p_(i-1) for k = 0, 1, ... 

# get the distribution of T_x via: P(T_x = k) = k|q_0 = kp0*q_k
k_bar_qx <- vector(mode = "list", length = nrow(px_data))
k_bar_qx[[1]] <- kpx[[1]][-nrow(kpx[[1]]), ]*qx_data
for (i in 2:nrow(px_data)) {
  k_bar_qx[[i]] <- kpx[[i]][-nrow(kpx[[i]]), ]*qx_data[-(1:(i - 1)), ]
}

e_0 <- apply(k_bar_qx[[1]] * (0:120), 2, sum) + 0.5
# i can get e_65 etc: e_65 <- apply(k_bar_qx[[66]] * (65:120), 2, sum) + 0.5
# etc!!

# and plot these distro's


# EXTENSION: interpolate (fill in the blanks)
# SORT DATAFRAMES BY COLUMN FIRST! (safety)
qx_data <- read_csv("qx_all_M.csv") %>% select(-age)
years_in_data <- qx_data %>% names() %>% as.numeric %>% sort()
all_years <- seq(from = min(years_in_data), to = max(years_in_data))

# lmk if there's a better way to do the below
qx_all <- as.data.frame(matrix(nrow = nrow(qx_data), ncol = length(all_years)))
names(qx_all) <- as.character(all_years)

for (i in as.character(all_years)) {
  if (i %in% as.character(years_in_data)) qx_all[, i] <- qx_data[, i]
  else {
    # find the nearest two years in the data, call them j and k
    # lol at the type conversion here
    j <- max(which(years_in_data < i))
    k <- min(which(years_in_data > i))
    print(j)
    print(k)
    #qx_all[, i] <- qx_all[, i - 1]  - (qx_all[, j] - qx_all[, k])/(k - j + 1)
  }
}

# this is a MESS
