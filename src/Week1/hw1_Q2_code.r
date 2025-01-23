# %%
library(readxl)
library(here)

# Use the here package to construct the file path
library(readxl)
library(here)
library(dplyr)

# Read the excel file into R
seasonal = read_excel(here("data", "Case-Shiller_HP_adjusted.xlsx"), sheet = "Monthly")
non_seasonal = read_excel(here("data", "Case-Shiller_HP_nonadjusted.xlsx"), sheet = "Monthly")

log_diff <- function(x) {
  log_diff_values <- c(NA, diff(log(x)))
  return (log_diff_values)
}

# Apply log and diff to numeric columns using dplyr 
seasonal_log_diff = seasonal %>% 
  mutate(across(where(is.numeric), log_diff, .names = "log_diff_adjusted")) 

non_seasonal_log_diff = non_seasonal %>% 
  mutate(across(where(is.numeric), log_diff, .names = "log_diff_nonadjusted")) 
# %%
# (a) Examine the ACF of the raw growth rates out through lag 48. 
# Do you see a pattern in the autocorrelations? Explain 

acf_non_seasonal = acf(non_seasonal_log_diff$log_diff_nonadjusted, na.action = na.pass, lag.max = 48)

# Non-adjusted series has no clear autocorrelation pattern rather it exhibits seasonal pattern 
# where the autocorrelation shrinks until summer and then spikes up again as it goes toward winter. 
# %%
# (b) Do you think an AR(1) model would work for the raw data? Explain. 

acf(ar1_model_non_seasonal$residuals, main = "ACF of Residuals for Non-Seasonal AR(1) Model", na.action = na.pass)
ar1_model_non_seasonal = arima(na.omit(non_seasonal_log_diff$log_diff_nonadjusted), order = c(1, 0, 0))

# In order for AR(1) model to work, the residual of the model should have no significant dependence.
# However, for the non seasonal data, lag with 1 sit above the blue line which is 95% confidence interval that autocorelation is not zero.
# %%
# (c) Examine the ACF of the adjusted rates. Do you see the same pattern?  Explain. 
acf_seasonal = acf(seasonal_log_diff$log_diff_adjusted, na.action = na.pass, lag.max = 48)

# No, we do not see the same pattern. The adjusted rates have a clear pattern of autocorrelation that decays slowly over time.
# The adjusted rates have a highest autocorrelation at lag 1, .942, and then .89 at lag 2, .85 at lag 3, etc.
# %%
# (d) Fit an AR(1) model to this data.  
ar1_model_seasonal = arima(na.omit(seasonal_log_diff$log_diff_adjusted), order = c(1,0,0))

fitted_ar1_seasonal_model = predict(ar1_model_seasonal, n.ahead = 1) $pred
# %%
## (e) Examine the residual autocorrelations.
acf(ar1_model_seasonal$residuals, main = "ACF of Residuals for Seasonal AR(1) Model", na.action = na.pass)

# (f) Does the AR(1) model fit the data? 
# Yes, most of the autocorrelation values are within the 95% confidence interval for most of the lags.

# %%
# (g) Using residual diagnostics and/or the PACF, find an AR(p) model that fits the data well.
pacf_seasonal = pacf(seasonal_log_diff$log_diff_adjusted, na.action = na.pass, lag.max = 48, main = "PACF of Seasonal Log Diff Adjusted")
print(acf(ar1_model_seasonal$residuals, main = "ACF of Residuals for Seasonal AR Model", na.action = na.pass)) 
# From the ACF plot of residuals and pacf plot , AR(1) model seems to fit the data well as the its over the blue line
# indicating that it is significantly different from zero. 

# %%
# (h) Build the one step ahead forecast for every observation in the sample using your fitted AR(p) model.
# Plot the forecast and the real data on the same plot. 


one_step_ahead_forecasts <- numeric(length(na.omit(seasonal_log_diff$log_diff_adjusted)-1))
beta0 <- ar1_model_seasonal$coef["intercept"]
beta1 <- ar1_model_seasonal$coef["ar1"]

for (i in 1:(length(one_step_ahead_forecasts))) {
  # Use the fitted model parameters to make the one-step-ahead forecast
  one_step_ahead_forecasts[i + 1] = beta0 + beta1 * na.omit(seasonal_log_diff$log_diff_adjusted)[i]
}
print(head(one_step_ahead_forecasts))
print(summary(one_step_ahead_forecasts))

plot(seasonal_log_diff$log_diff_adjusted, type = "l", col = "blue", lwd = 2, ylab = "Log Diff Adjusted", xlab = "Time", main = "Actual vs One-Step-Ahead Forecasts")
lines(one_step_ahead_forecasts, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "One-Step-Ahead Forecasts"), col = c("blue", "red"), lty = 1, lwd = 2)

# %% 
# (i) What is one step ahead out of sample forecast (i.e. the forecast of T+1) and find the 95%  prediction interval.

one_step_ahead_forecast = predict(ar1_model_seasonal, n.ahead = 1)
print("hi")
lower_bound = one_step_ahead_forecast$pred - 1.96 * one_step_ahead_forecast$se
upper_bound = one_step_ahead_forecast$pred + 1.96 * one_step_ahead_forecast$se
print(sprintf("Forecast: %f, Lower Bound: %f, Upper Bound: %f", one_step_ahead_forecast$pred, lower_bound, upper_bound))
