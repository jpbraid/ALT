# read in the qx data from life tables
qx_data <- read_csv("qx_all_M.csv") %>% select(-age) # SORT BY COLUMN
years_in_data <- qx_data %>% names() %>% as.numeric %>% sort()
all_years <- seq(from = min(years_in_data), to = max(years_in_data))

# initialise empty data.frame to contain interpolated qx data
# (is there a better way to do this?)
qx_all <- as.data.frame(matrix(nrow = nrow(qx_data), ncol = length(all_years)))
names(qx_all) <- as.character(all_years)

# now i want to: (1) put the data that already exists into qx_all
for (i in all_years) {
  if (i %in% years_in_data) qx_all[, as.character(i)] <- qx_data[, as.character(i)]
}

# then (2) interpolate the rest of the data
for (i in all_years) {
  if (!(i %in% years_in_data)) {
    j <- max(years_in_data[years_in_data < i])
    k <- min(years_in_data[years_in_data > i])
    qx_all[, as.character(i)] <- qx_all[, as.character(i - 1)] - (qx_all[, as.character(j)] - qx_all[, as.character(k)])/(k - j)
  }
}

#finally let's fill in / interpolate the NA's (as it stands, all the NA's are in a block of high ages and early life tables
# if we get new NA's in a different spot, the below code will break things; so i need to add some kind of check (if j < 100 or ... etc.)
na_columns <- vector()
for (j in 1:ncol(qx_all)) {
  if (sum(is.na(qx_all[, j]) > 0)) na_columns <- c(na_columns, j)
}

for (j in na_columns) {
  index <- min(which(is.na(qx_all[, j])))
  qx_all[index:(nrow(qx_all) - 1), j] <- qx_all[(index - 1), j]
  qx_all[nrow(qx_all), j] <- 1
}

age <- 0:(nrow(qx_all) - 1)
qx_all <- cbind(age, qx_all)
write.csv(qx_all, "qx_all_interpolated_M.csv", row.names = F)
