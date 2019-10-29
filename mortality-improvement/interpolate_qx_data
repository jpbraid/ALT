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
