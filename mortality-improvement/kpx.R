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
#i can get e_65 etc: e_65 <- apply(k_bar_qx[[66]] * (65:120), 2, sum) + 0.5
# etc!!

# and plot these distro's



#### NOTE: for years <= 1986 we only have qx's up to 100; the improvement spreadsheet just repeats the qx100 up to 119, and then makes q120 = 1
