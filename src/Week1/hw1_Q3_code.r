# %%
# (b) 
set.seed(123)  # For reproducibility

# Define the parameters
alpha <- 0.1
beta <- 0.99
sigma <- 0.5
n <- 100  # Number of observations per realization
m <- 1000  # Number of realizations


# Initialize a vector to store the estimated slope coefficients
estimated_betas <- numeric(m)

# Simulate the AR(1) process and estimate the AR(1) parameter for each realization
for (j in 1:m) {
  # Draw the initial value Y0 from N(mu, sigma^2)
  Y0 <- rnorm(1, mean = 10, sd = sqrt(12.5))
  
  # Initialize the time series
  Y <- numeric(n)
  Y[1] <- Y0
  epsilon <- rnorm(n, mean = 0, sd = sigma)
  # Simulate the AR(1) process
  for (t in 2:n) {
    Y[t] <- alpha + beta * Y[t-1] + epsilon[t]
  }
  
# Estimate the AR(1) parameter using least squares
  ar1_model <- arima(Y, order = c(1, 0, 0), method="CSS")
  estimated_betas[j] <- ar1_model$coef["ar1"]
}

# Present the histogram of the 1000 estimated slope coefficients
hist(estimated_betas, breaks = 30, col = "blue", main = "Histogram of Estimated AR(1) Coefficients", xlab = "Estimated AR(1) Coefficient")

# %%
#(c)
avg_estimated_beta <- mean(estimated_betas)
print(avg_estimated_beta-0.99)
# The average estimated beta is lower than true beta of 0.99, having negative bias.
# The sample size was finite in this case 100 thus the estimated beta is biased. However, as the sample size increases, the bias will decrease.
# %% 