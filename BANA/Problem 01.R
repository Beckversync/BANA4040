# # Problem 01

# # Read the data
# library(readxl)
# job_data <- read_excel("data.xlsx", col_names = FALSE)
# colnames(job_data) = c("y", "t1", "t2", "t3", "t4")
# job_data


# #01 Graphical Summaries - Scatterplots
# par(mfrow = c(2, 2))  # Arrange plots in 2x2 grid
# plot(job_data$t1, job_data$y, main = "y vs t1", xlab = "t1", ylab = "y") # weak relationship
# plot(job_data$t2, job_data$y, main = "y vs t2", xlab = "t2", ylab = "y") # no relationship
# plot(job_data$t3, job_data$y, main = "y vs t3", xlab = "t3", ylab = "y") # strong relationship
# plot(job_data$t4, job_data$y, main = "y vs t4", xlab = "t4", ylab = "y") # strong relationship


# #-------------------------------------------------------------------
# #02 Performing all possible regressions 

# # Load necessary libraries
# library(leaps)

# # Fit all possible models
# all_models <- regsubsets(y ~ t1 + t2 + t3 + t4, data = job_data, nvmax = 4)

# # Get summary
# summary(all_models) # here is the 4 best model in the combanations of 16 models

# # Extracting Adjusted R-squared
# summary(all_models)$adjr2

# # Extracting BIC
# summary(all_models)$bic

# # Extracting Mallows Cp
# summary(all_models)$cp

# # Extract R-squared
# summary(all_models)$rsq

# # Extract number of predictors used in each model
# num_predictors <- apply(summary(all_models)$which, 1, sum) - 1
# print(num_predictors)

# # Load DAAG package for PRESS calculation
# library(DAAG)

# # Function to compute PRESS
# compute_press <- function(model) {
#   sum((resid(model) / (1 - lm.influence(model)$hat))^2)
# }

# # Fit models and compute PRESS
# press_values <- c(
#   compute_press(lm(y ~ t3, data = job_data)),  # Model with only t3
#   compute_press(lm(y ~ t1 + t3, data = job_data)),  # Model with t1 and t3
#   compute_press(lm(y ~ t1 + t3 + t4, data = job_data)),  # Model with t1, t3, and t4
#   compute_press(lm(y ~ t1 + t2 + t3 + t4, data = job_data))  # Full model
# )

# print(press_values)

# # Function to compute AIC
# compute_aic <- function(model) {
#   n <- length(model$residuals)
#   p <- length(coef(model))
#   log_likelihood <- log(sum(resid(model)^2) / n) * n
#   return(2 * p + log_likelihood)
# }

# # Fit models and compute AIC
# aic_values <- c(
#   compute_aic(lm(y ~ t3, data = job_data)),  
#   compute_aic(lm(y ~ t1 + t3, data = job_data)),  
#   compute_aic(lm(y ~ t1 + t3 + t4, data = job_data)),  
#   compute_aic(lm(y ~ t1 + t2 + t3 + t4, data = job_data))  
# )

# print(aic_values)

# # Create a summary table
# model_results <- data.frame(
#   Model = c("t3", "t1 + t3", "t1 + t3 + t4", "t1 + t2 + t3 + t4"),
#   Num_Predictors = num_predictors,
#   R2 = summary(all_models)$rsq,
#   Adjusted_R2 = summary(all_models)$adjr2,
#   PRESS = press_values,
#   AIC = aic_values,
#   BIC = summary(all_models)$bic,
#   Mallows_Cp = summary(all_models)$cp
# )

# print(model_results)

#-------------------------------------------------------------------
#03 The BEST Model

# By following the criteria (below), we can choose the best model overall which is t1 + t3 + t4
  
# The Criteria of choosing the best model (excerpted from the lecture)
  # R2: larger value is better, but not appropriate to compare models with different number of parameters.
  # R2a,p: larger value is better;
  # Mallow's Cp: lower value is better (value that is close to p is also valued)
  # AICp: lower value is better.
  # BICp: lower value is better.
  # PRESS: models with small PRESS fit well.

# Variable t2 should be excluded, because:
  # adding it does not improve R2
  # adding it increase PRESS and BIC (thus being a worse model selection scores than t1 + t3 +t4)
  # adding it slightly increase AIC and Mallowes Cp (thus being a worse model than t1 + t3 + t4)

# Are the Results Suprising?
  # No, because from the beginning (Graphical Sum), it showed no relationship with y
  # So, Model selection criteria confirmed what the visual analysis already suggested

#-------------------------------------------------------------------

# #04 Fitting the model and model diagnostic

# # Fit the best model (t1, t3, t4)
# best_model <- lm(y ~ t1 + t3 + t4, data = job_data)

# # View summary of model
# summary(best_model)


# # Check Assumptions - All Assumptions are met with no major issues
#   #1 Linearity Assumption using Scatter plot: as we have plotted in the beginning, t3 and t4 have strong relationship, while t1 is weak 


# # Residuals vs. Fitted Values Plot
# plot(best_model$fitted.values, resid(best_model),
#      main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
# abline(h = 0, col = "red")  # Add horizontal line at 0  

#   #2 Mean of errors = 0 using Residuals vs Fitted: there is no pattern
#   #3 Constant Variance using Residuals vs Fitted: the vertical magnitude in the residual plot  is almost the same accross diifferent level of fitted values 
  

# # Load library for ACF plot
# library(forecast)

# # Plot autocorrelation of residuals
# Acf(resid(best_model), main = "ACF of Residuals")
  
#   #4 Uncorrelated Errors using ACF plot: there are no correlation; all values are close to 0 and below the blue lines


# # Normal Q-Q Plot
# qqnorm(resid(best_model), main = "Normal Q-Q Plot")
# qqline(resid(best_model), col = "red")  # Add reference line

#   #5 Normal Assumption using QQ plot: the residuals fall closely to the line, but still a little curve which suggest a thin tails

# # Diagnostic conclusion: All Assumptions are met with no major issues

# # Do we have a good fit for the model?
#   # Yes! The model satisfies all  assumptions with only a slight normality issue (thin tails). But no need to transform because the R2 is 0.956 which is very strong already
  

# #-------------------------------------------------------------------

#05 Which model is the best with a given number of predictors?

# Plot BIC values
# plot(num_predictors, bic_values, type = "b", pch = 19, col = "blue", 
#      main = "BIC vs. Number of Predictors", xlab = "Number of Predictors", ylab = "BIC")

# # Use identify() function to interactively select points
# identify(num_predictors, bic_values, labels = 1:length(num_predictors)) # Click on a point to identify the best model
  
#   # The lowest BIC is when there are 3 predictors, and the highest is when there are 1 predictor. 
#   # However we can only have maximum 2 predictors thus the possible number of predictor that we can choose is 2


# # Plot Mallows' Cp values
# plot(num_predictors, cp_values, type = "b", pch = 19, col = "red", 
#      main = "Mallows' Cp vs. Number of Predictors", xlab = "Number of Predictors", ylab = "Mallows' Cp")

# # Use identify() function to interactively select points
# identify(num_predictors, cp_values, labels = 1:length(num_predictors))

  # The lowest BIC is when there are 3 predictors, and the highest is when there are 1 predictor. 
  # However we can only have maximum 2 predictors thus the possible number of predictor that we can choose is 2


#-------------------------------------------------------------------

# #06 Validation

# # How to find Model 11??? ??????????????????????????????????????????????????

# # Fit Model 11 (assuming t1 + t3 is used based on previous results)
# model_11 <- lm(y ~ t1 + t3, data = job_data)

# # View summary
# summary(model_11) # This gives us the Residual Standard Error (sigma) needed to compute SSE_p

# # Compute SSE - measure how much variation in y which is not explained by the model (the lower score the better fit)
# sse_p <- (summary(model_11)$sigma)^2 * summary(model_11)$df[2]
# print(paste("SSE_p =", sse_p))


# # Install & load DAAG package
# install.packages("DAAG")
# library(DAAG)

# # Compute PRESS - similar to SSE but calculated via leave-one-out cross-validation (LOOCV).
# press_p <- press(model_11)
# print(paste("PRESS_p =", press_p)) # The lower score the better the predictive ability on unseen data

# # Print comparison
# print(paste("SSE_p:", round(sse_p, 2), " | PRESS_p:", round(press_p, 2)))

# # Ideally, SSE and PRESS should be close.
# # BUY PRESS is higher than SSE, so the model overfits (performs well on training data but poorly on new data).
# # Not a huge difference, so mild overfitting, but not extreme.


# #-------------------------------------------------------------------

# #07 Automated search procedure

# # Forward Selection
# forward_model <- step(lm(y ~ 1, data = job_data),  
#                       scope = ~ t1 + t2 + t3 + t4,  
#                       direction = "forward")

#   # View selected model
#   summary(forward_model)


# # Backward Elimination
# backward_model <- step(lm(y ~ t1 + t2 + t3 + t4, data = job_data),  
#                        direction = "backward")

#   # View selected model
#   summary(backward_model)

  
# # Stepwise Selection
# stepwise_model <- step(lm(y ~ 1, data = job_data),  
#                          scope = ~ t1 + t2 + t3 + t4,  
#                          direction = "both")
  
#   # View selected model
#   summary(stepwise_model)
  

# # Print formulas of selected models
# print(paste("Forward Selection Model:", formula(forward_model)))
# print(paste("Backward Elimination Model:", formula(backward_model)))
# print(paste("Stepwise Selection Model:", formula(stepwise_model)))

# # What Model(s) Were Chosen?
#   # all three procedures chose the same model:

#     # t3 (most significant predictor)
#     # t1 (moderate significance)
#     # t4 (still relevant)
#     # 2 was removed in all cases, meaning it does not add much predictive power.

# # Impact of Starting Model:
#   # Forward Selection: Starts from nothing → adds t3 → t1 → t4.
#   # Backward Elimination: Starts with all variables → removes t2 first.
#   # Stepwise Selection: Confirms t3 + t1 + t4 as the best model.

# # In general, different model selection methods may lead to different models because:
  
#   # Forward selection is greedy – it adds variables step by step and might miss a better combination.
#   # Backward elimination starts with all variables – it can be sensitive to collinearity and the order of removals.
#   # Stepwise regression balances both but can still be influenced by starting conditions.


# #-------------------------------------------------------------------

# #08 Searching all models based on specified conditions

# Load necessary package
library(leaps)

# Perform subset selection with a max of 3 predictors
subset_models <- regsubsets(y ~ t1 + t2 + t3 + t4, data = job_data, nvmax = 3, method = "exhaustive")
  # nvmax = 3 => Limits models to at most 3 predictors.
  # method = "exhaustive" => Ensures all possible models are checked.

# Get summary
subset_summary <- summary(subset_models)

# Extract Adjusted R2, BIC, and Mallows' Cp
adj_r2_values <- subset_summary$adjr2
bic_values <- subset_summary$bic
cp_values <- subset_summary$cp

# Find the best models based on each criterion
best_adj_r2_model <- which.max(adj_r2_values)
best_bic_model <- which.min(bic_values)
best_cp_model <- which.min(abs(cp_values - 3))  # Closest to p (number of predictors)

# Print results
print(paste("Best model by Adjusted R2: Model", best_adj_r2_model))
print(paste("Best model by BIC: Model", best_bic_model))
print(paste("Best model by Mallows' Cp: Model", best_cp_model))

# Show which variables are included in the best models
subset_summary$which[best_adj_r2_model, ]  # Best Adjusted R2 Model
subset_summary$which[best_bic_model, ]  # Best BIC Model
subset_summary$which[best_cp_model, ]  # Best Mallows' Cp Model


  