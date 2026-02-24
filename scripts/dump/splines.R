library(mgcv)

# Example dataset: Ensure your dataset is ready
# test$perc_area_cav is the response variable (y)
# test$psi_pred_MPa is the predictor (x)

# Step 1: Create the unconstrained CR spline basis
X <- smooth.construct.cr.smooth.spec(
  object  = s(psi_pred_MPa, k = 10, bs = "cr"),
  data = test,
  knots = NULL
)
str(X)
# Step 2: Define monotonic decreasing constraints
mono <- mono.con(X$xp, up = FALSE)  # up = FALSE means "decreasing"

# Step 3: Use `pcls` to fit the constrained model
# Prepare the penalized constrained least squares problem
pcls_data <- list(
  X = X$X,               # Design matrix
  S = X$S,               # Penalty matrix
  sp = X$sp,             # Smoothing parameter
  y = test$perc_area_cav, # Response variable
  w = rep(1, nrow(test)), # Weights (can be adjusted if necessary)
  Ain = mono$A,          # Constraint matrix (monotonicity)
  bin = mono$b           # Constraint vector
)

# Solve the penalized regression problem
beta <- pcls(pcls_data)

# Step 4: Generate predictions from the model
fitted_values <- X$X %*% beta
test$predicted_perc_area_cav <- fitted_values

# Optional: Plot the results
plot(test$psi_pred_MPa, test$perc_area_cav, main = "Monotonic Decreasing Spline Fit")
lines(test$psi_pred_MPa, test$predicted_perc_area_cav, col = "red", lwd = 2)
