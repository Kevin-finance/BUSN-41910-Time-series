# Install and load the expm package
# Install and load the expm package
if (!require(expm)) {
  install.packages("expm")
  library(expm)
}

# Define the matrix F
F <- matrix(c(1.3, -0.45, 1, 0), nrow = 2, byrow = TRUE)

# Number of periods for the impulse response function
n_periods <- 20

# Initialize a list to store the impulse response functions
irf_list <- list()

# Compute the impulse response function for each period
for (t in 0:n_periods) {
  irf_list[[t + 1]] <- F %^% t
}

# Convert the list to an array for easier plotting
irf_array <- array(unlist(irf_list), dim = c(2, 2, n_periods + 1))

# Plot the impulse response function
par(mfrow = c(2, 2))
for (i in 1:2) {
  for (j in 1:2) {
    plot(0:n_periods, irf_array[i, j, ], type = "l", col = "blue", lwd = 2,
         xlab = "Time", ylab = paste("IRF[", i, ",", j, "]", sep = ""),
         main = paste("Impulse Response Function for F[", i, ",", j, "]", sep = ""))
  }
}