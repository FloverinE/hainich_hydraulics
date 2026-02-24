x <- seq(0,40)
y <- pnorm(seq(0,40), mean=25, sd=5)
spl <- smooth.spline(y ~ x)
plot(x, y, type = "l")
newy <- 0.85
newx <- uniroot(function(x) predict(spl, x, deriv = 0)$y - newy,
                interval = c(0, 40))$root
abline(v = newx, col = "red")


mod_test <- df_sample_nest$fit_mpd_spline[[1]]
df_sample_nest$fit_weibull[[1]]

getPx(df_sample_nest$fit_weibull[[1]], 50)
x = df_sample_nest$data[[1]]$psi_pred_MPa

new_y = 0.1
newx <- uniroot(
  function(x) predict(mod_test, newdata = data.frame(psi_pred_MPa = x), type = "response") - newy,
  interval = range(x)
)$root
newx

plot(x, df_sample_nest$data[[1]]$mpd, type = "l")


x_vec <- seq(min(df_sample_nest$data[[1]]$psi_pred_MPa), 
         max(df_sample_nest$data[[1]]$psi_pred_MPa), length.out = 1000)
y <- predict(mod_test, newdata = data.frame(psi_pred_MPa = x), type = "response")


# Define the target y-value (newy)
new_y <- 0.12  # For 50% embolism

# Use uniroot to find the x-value (psi_pred_MPa) where y equals newy
# Ensure the function evaluates correctly across the grid
root_function <- function(x_val, new_y) {
  predict(mod_test, newdata = data.frame(psi_pred_MPa = x_val), type = "response") - new_y
}

# Provide the interval explicitly within the observed data range
interval <- range(df_sample_nest$data[[1]]$psi_pred_MPa)

new_x <- uniroot(root_function, interval = interval, new_y = 0.12)$root

f.find_px_spline <- function(psi_vec, model_preds, X, spline_model) {

  # Use uniroot to find the x-value (psi_pred_MPa) where y equals newy
  # Ensure the function evaluates correctly across the grid
  root_function <- function(psi_vec, new_y) {
    predict(spline_model, newdata = data.frame(psi_pred_MPa = psi_vec), type = "response") - X
  }
  # Provide the interval explicitly within the observed data range
  interval <- range(psi_vec)

  new_x <- uniroot(root_function, interval = interval, new_y = X)$root
  return(new_x)
}

f.find_px_spline(psi_vec = df_sample_nest$data[[1]]$psi_pred_MPa, 
                 model_preds = predict(df_sample_nest$fit_mpd_spline[[1]]),
                 X = 0.88,
                 spline_model = df_sample_nest$fit_mpd_spline[[1]])

# Plot the curve and add a vertical line at newx
plot(x, y, type = "l", xlab = "psi_pred_MPa", ylab = "Predicted perc_area_cav_scaled",
     main = "SCAM: Predicted Curve with Root-Finding")
if (exists("newx")) {
  abline(v = new_x, col = "red", lty = 2)
  points(new_x, new_y, col = "blue", pch = 16)  # Add the target point
}


# Check if the interval includes the target value
if (sign(root_function(interval[1])) != sign(root_function(interval[2]))) {
  cat("The psi_pred_MPa value where the prediction equals", newy, "is:", newx, "\n")
} else {
  cat("Target y-value not within the range of predictions. Adjust your interval or model.\n")
}
