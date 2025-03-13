# Phan Nu Quynh Huong
# Do Minh Quan

# ASG 01
#-------------------------------------------------------------------

# Problem 01

# Read the data
job_data = read.table("job.txt")
job_data
colnames(job_data) = c("y", "t1", "t2", "t3", "t4")
job_data


#01 Graphical Summaries - Scatterplots

# par(mfrow = c(2, 2))  # Arrange plots in 2x2 grid
# plot(job_data$t1, job_data$y, main = "y vs t1", xlab = "t1", ylab = "y") # weak relationship
# plot(job_data$t2, job_data$y, main = "y vs t2", xlab = "t2", ylab = "y") # no relationship
# plot(job_data$t3, job_data$y, main = "y vs t3", xlab = "t3", ylab = "y") # strong relationship
# plot(job_data$t4, job_data$y, main = "y vs t4", xlab = "t4", ylab = "y") # strong relationship


#-------------------------------------------------------------------

#02 Performing all possible regressions 

# Load necessary libraries
library(leaps)

# Fit all possible models
all_models <- regsubsets(y ~ t1 + t2 + t3 + t4, data = job_data, nvmax = 4)

# Get summary
summary(all_models) # here is the 4 best model in the combanations of 16 models

# Extracting Adjusted R-squared
summary(all_models)$adjr2

# Extracting BIC
summary(all_models)$bic

# Extracting Mallows Cp
summary(all_models)$cp

# Extract R-squared
summary(all_models)$rsq

# Extract number of predictors used in each model
num_predictors <- apply(summary(all_models)$which, 1, sum) - 1
print(num_predictors)

# Load DAAG package for PRESS calculation
library(DAAG)

# Function to compute PRESS
compute_press <- function(model) {
  sum((resid(model) / (1 - lm.influence(model)$hat))^2)
}

# Fit models and compute PRESS
press_values <- c(
  compute_press(lm(y ~ t3, data = job_data)),  # Model with only t3
  compute_press(lm(y ~ t1 + t3, data = job_data)),  # Model with t1 and t3
  compute_press(lm(y ~ t1 + t3 + t4, data = job_data)),  # Model with t1, t3, and t4
  compute_press(lm(y ~ t1 + t2 + t3 + t4, data = job_data))  # Full model
)

print(press_values)

# Function to compute AIC
compute_aic <- function(model) {
  n <- length(model$residuals)
  p <- length(coef(model))
  log_likelihood <- log(sum(resid(model)^2) / n) * n
  return(2 * p + log_likelihood)
}

# Fit models and compute AIC
aic_values <- c(
  compute_aic(lm(y ~ t3, data = job_data)),  
  compute_aic(lm(y ~ t1 + t3, data = job_data)),  
  compute_aic(lm(y ~ t1 + t3 + t4, data = job_data)),  
  compute_aic(lm(y ~ t1 + t2 + t3 + t4, data = job_data))  
)

print(aic_values)

# Create a summary table
model_results <- data.frame(
  Model = c("t3", "t1 + t3", "t1 + t3 + t4", "t1 + t2 + t3 + t4"),
  Num_Predictors = num_predictors,
  R2 = summary(all_models)$rsq,
  Adjusted_R2 = summary(all_models)$adjr2,
  PRESS = press_values,
  AIC = aic_values,
  BIC = summary(all_models)$bic,
  Mallows_Cp = summary(all_models)$cp
)

print(model_results) # The results of the best 4 models

# Here is the summary table of all 16 Models

# Function to calculate statistical indices for the model
model_stats <- function(fit, MSE_full){
  # fit: lm object (regression model)
  # MSE_full: Mean Squared Error of the full model used to compute Mallows' Cp
  
  n <- length(fit$residuals) # Number of observations
  p <- length(coef(fit)) # Number of parameters (including intercept)
  
  # Calculate R-squared and adjusted R-squared
  r2 <- summary(fit)$r.squared
  adjr2 <- summary(fit)$adj.r.squared
  
  # Compute the sum of squared errors (SSE) for the model
  sse_p <- sum(resid(fit)^2)
  
  # Compute the PRESS statistic
  hat <- lm.influence(fit)$hat
  res <- resid(fit)
  press <- sum((res/(1 - hat))^2)
  
  # Compute AIC and BIC
  aic_val <- AIC(fit)
  bic_val <- BIC(fit)
  
  # Compute Mallows' Cp
  cp_val <- sse_p / MSE_full - (n - 2*p)
  
  # Return results
  return(c(p, r2, adjr2, press, aic_val, bic_val, cp_val))
}

# Build the full regression model
lm_full <- lm(y ~ t1 + t2 + t3 + t4, data = job_data)

# Compute the sum of squared errors (SSE) of the full model
SSE_full <- sum(resid(lm_full)^2)

# Number of observations in the dataset
n <- nrow(job_data)

# Number of parameters in the full model
p_full <- length(coef(lm_full))

# Compute the Mean Squared Error (MSE) of the full model
MSE_full <- SSE_full / (n - p_full)

# Create an empty dataframe to store results
results <- data.frame(
  Model = character(),
  p = numeric(),
  R2 = numeric(),
  AdjR2 = numeric(),
  PRESS = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  Cp = numeric(),
  stringsAsFactors = FALSE
)

# List of predictor variables
preds <- c("t1", "t2", "t3", "t4")

# Loop through all subsets of predictor variables
for (k in 0:4) {
  if (k == 0) {
    # Model with only intercept
    form <- as.formula("y ~ 1")
    fit <- lm(form, data = job_data)
    stats <- model_stats(fit, MSE_full)
    results <- rbind(results, data.frame(
      Model = "Intercept Only",
      p = stats[1],
      R2 = stats[2],
      AdjR2 = stats[3],
      PRESS = stats[4],
      AIC = stats[5],
      BIC = stats[6],
      Cp = stats[7],
      stringsAsFactors = FALSE
    ))
  } else {
    # Models with k predictor variables
    subset_list <- combn(preds, k, simplify = FALSE)
    for (vars in subset_list) {
      form <- as.formula(paste("y ~", paste(vars, collapse = " + ")))
      fit <- lm(form, data = job_data)
      stats <- model_stats(fit, MSE_full)
      results <- rbind(results, data.frame(
        Model = paste(vars, collapse = " + "),
        p = stats[1],
        R2 = stats[2],
        AdjR2 = stats[3],
        PRESS = stats[4],
        AIC = stats[5],
        BIC = stats[6],
        Cp = stats[7],
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Display results
knitr::kable(results, caption = "Table of Forecast Model Results")



#-------------------------------------------------------------------
# #03 The BEST Model

# # By following the criteria (below), we can choose the best model overall which is t1 + t3 + t4
  
# # The Criteria of choosing the best model (excerpted from the lecture)
#   # R2: larger value is better, but not appropriate to compare models with different number of parameters.
#   # R2a,p: larger value is better;
#   # Mallow's Cp: lower value is better (value that is close to p is also valued)
#   # AICp: lower value is better.
#   # BICp: lower value is better.
#   # PRESS: models with small PRESS fit well.

# # Variable t2 should be excluded, because:
#   # adding it does not improve R2
#   # adding it increase PRESS and BIC (thus being a worse model selection scores than t1 + t3 +t4)
#   # adding it slightly increase AIC and Mallowes Cp (thus being a worse model than t1 + t3 + t4)

# # Are the Results Suprising?
#   # No, because from the beginning (Graphical Sum), it showed no relationship with y
#   # So, Model selection criteria confirmed what the visual analysis already suggested

# #-------------------------------------------------------------------

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

# #05 Which model is the best with a given number of predictors?

# # Extract BIC values
# bic_values <- summary(all_models)$bic

# # Extract Mallows' Cp values
# cp_values <- summary(all_models)$cp

# # Extract number of predictors in each model
# num_predictors <- apply(summary(all_models)$which, 1, sum) - 1

# library(magrittr)
# library(dplyr)
# # Filter models with 1 or 2 predictors
# filtered_models <- data.frame(
#   Model = 1:length(num_predictors),
#   Num_Predictors = num_predictors,
#   BIC = bic_values,
#   Mallows_Cp = cp_values
# ) %>%
#   filter(Num_Predictors <= 2)

# # Print the filtered models
# print(filtered_models)


# # Plot BIC values
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

#   # The lowest BIC is when there are 3 predictors, and the highest is when there are 1 predictor. 
#   # However we can only have maximum 2 predictors thus the possible number of predictor that we can choose is 2


# #-------------------------------------------------------------------

# #06 Validation


# # Fit Model 11 (t3 + t4 is used based on previous results)
# model_11 <- lm(y ~ t3 + t4, data = job_data)

# # View summary
# summary(model_11) # This gives us the Residual Standard Error (sigma) needed to compute SSE_p

# # Compute SSE - measure how much variation in y which is not explained by the model (the lower score the better fit)
# sse_p <- (summary(model_11)$sigma)^2 * summary(model_11)$df[2]
# print(paste("SSE_p =", sse_p))


# # Install & load DAAG package
# library(DAAG)

# # Compute PRESS - similar to SSE but calculated via leave-one-out cross-validation (LOOCV).
# press_p <- press(model_11)
# print(paste("PRESS_p =", press_p)) # The lower score the better the predictive ability on unseen data

# # Print comparison
# print(paste("SSE_p:", round(sse_p, 2), " | PRESS_p:", round(press_p, 2)))


# # PRESS (1449.6) is higher than SSE (1111.31), meaning the model performs worse when predicting new data than when fitting the training data.
# # This suggests some overfitting, but since the difference is not extremely large, the overfitting is not severe.
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

# # Load necessary package
# library(leaps)

# # Perform subset selection with a max of 3 predictors
# subset_models <- regsubsets(y ~ t1 + t2 + t3 + t4, data = job_data, nvmax = 3, method = "exhaustive")
#   # nvmax = 3 => Limits models to at most 3 predictors.
#   # method = "exhaustive" => Ensures all possible models are checked.

# # Get summary
# subset_summary <- summary(subset_models)

# # Extract Adjusted R2, BIC, and Mallows' Cp
# adj_r2_values <- subset_summary$adjr2
# bic_values <- subset_summary$bic
# cp_values <- subset_summary$cp

# # Find the best models based on each criterion
# best_adj_r2_model <- which.max(adj_r2_values)
# best_bic_model <- which.min(bic_values)
# best_cp_model <- which.min(abs(cp_values - 3))  # Closest to p (number of predictors)

# # Print results
# print(paste("Best model by Adjusted R2: Model", best_adj_r2_model))
# print(paste("Best model by BIC: Model", best_bic_model))
# print(paste("Best model by Mallows' Cp: Model", best_cp_model))

# # Show which variables are included in the best models
# subset_summary$which[best_adj_r2_model, ]  # Best Adjusted R2 Model
# subset_summary$which[best_bic_model, ]  # Best BIC Model
# subset_summary$which[best_cp_model, ]  # Best Mallows' Cp Model


# #-------------------------------------------------------------------

# # Problem 02

# #01 Forward Selection Procedure

# # Principles:
# # Start with no predictors and add one variable at a time.
# # Choose the predictor that results in the lowest SSE at each step.
# # Once a predictor is included, it remains in the model.

# # Step 01: x4
# # Step 02: x1, x4
# # Step 03: x1, x2, x4
# # Step 04: x1, x2, x3, x4


# #02 Backward Elimination Procedure

# # Principles:
# # Start with all predictors and remove one at a time.
# # Eliminate the predictor that results in the lowest SSE after removal.
# # Once a predictor is eliminated, it is no longer considered.

# # Step 01: x1, x2, x3, x4
# # Step 02: x1, x2, x3
# # Step 03: x2, x3
# # Step 04: x3


# #-------------------------------------------------------------------

# # Problem 03


# # Load the data
# grocery_data <- read.table("grocery.txt", header = FALSE)
# grocery_data

# # Assign column names
# colnames(grocery_data) <- c("labor", "shipped", "cost", "holiday")


# # Fit the linear regression model
# result <- lm(labor ~ shipped + cost + holiday, data = grocery_data)

# plot(grocery_data$shipped, grocery_data$labor, main = "labor vs shipped", xlab = "x", ylab = "y")
# plot(grocery_data$cost, grocery_data$labor, main = "labor vs cost", xlab = "x", ylab = "y")
# plot(grocery_data$holiday, grocery_data$labor, main = "labor vs holiday", xlab = "x", ylab = "y")

# #-------------------------------------------------------------------

# # 01
# # Residual Analysis
# par(mfrow=c(1,1))
# plot(result$fitted.values, result$residuals, 
#      main="Residuals vs Fitted Values",
#      xlab="Fitted Values", ylab="Residuals", pch=20)
# abline(h=0, col="red", lwd=2)

# # For the majority, there is no clear pattern, thus errors are randomly distributed.
# # For the minority on the right, may suggest outliers / extreme values


# # Standardized Residuals vs Fitted Values
# par(mfrow=c(2,1)) 
# plot(result$fitted.values, rstandard(result), 
#      main="Standardized Residuals vs Fitted Values",
#      xlab="Fitted Values", ylab="Standardized Residuals", pch=20)
# abline(h=c(-2,2), col="red", lwd=2, lty=2)

# # Most values fall within +/- 2, thus no major violations


# # Studentized Residuals vs Fitted Values
# plot(result$fitted.values, rstudent(result), 
#      main="Studentized Residuals vs Fitted Values",
#      xlab="Fitted Values", ylab="Studentized Residuals", pch=20)
# abline(h=c(-2,2), col="red", lwd=2, lty=2)

# # This helps identify potential outliers


# # Detecting Outliers using Studentized Residuals
# sorted_residuals <- sort(rstudent(result), decreasing = TRUE)
# print(sorted_residuals)

# # Bonferroni Outlier Test
# n <- nrow(grocery_data)  # Number of observations
# p <- length(coef(result))  # Number of predictors + intercept
# alpha <- 0.05
# bonferroni_cutoff <- qt(1 - alpha / (2 * n), df = n - p - 1)
# print(paste("Bonferroni cutoff:", bonferroni_cutoff))

# # Identifying outliers
# outliers <- which(abs(rstudent(result)) > bonferroni_cutoff)
# print(paste("Outliers detected at indices:", toString(outliers)))


# #-------------------------------------------------------------------

# # 02

# # (a)
# # Fit Model
# model <- lm(labor ~ shipped + cost + holiday, data = grocery_data)

# # number of observation
# n <- nrow(grocery_data)
# p <- length(coef(model)) - 1 # trừ đi hệ số intercept

# # rstudent
# student.res <- rstudent(model)

# sorted_res <- sort(student.res)
# print("Sorted studentized residuals:")
# print(sorted_res)

# # Bonferroni Method

# alpha <- 0.05
# crit <- qt(1 - alpha/(2*n), df = n - p - 1)
# print("Critical value (Bonferroni):")
# print(crit)

# options(repr.plot.width = 7, repr.plot.height = 6)

# # studentized plot
# plot(student.res,
#      ylab = "Studentized Residuals",
#      main = "Studentized Residuals with Bonferroni Critical Lines",
#      ylim = c(-4, 4))
# abline(h = crit, col = "red", lty = 2)
# abline(h = -crit, col = "red", lty = 2)


# # Find outliers
# outliers <- which(abs(student.res) > crit)
# print("Indices of potential outliers (|t_i| > critical value):")
# print(outliers)


# #-------------------------------------------------------------------

# # 03

# # Get leverage values (h_ii)
# lev <- lm.influence(model)$hat  

# # Print summary of leverage values
# summary(lev)

# sort(lev)

# # Define leverage thresholds
# p <- length(coef(model))  # Number of predictors + intercept
# n <- length(model$fitted.values)  # Number of observations

# threshold_2p <- 2*p/n
# threshold_3p <- 3*p/n
# threshold_5p <- 5*p/n

# # Identify high-leverage points
# high_leverage_2p <- which(lev > threshold_2p)
# high_leverage_3p <- which(lev > threshold_3p)
# high_leverage_5p <- which(lev > threshold_5p)

# # Print indices
# cat("High leverage (2p/n):", high_leverage_2p, "\n")
# cat("High leverage (3p/n):", high_leverage_3p, "\n")
# cat("High leverage (5p/n):", high_leverage_5p, "\n")

# # Plot leverage values
# plot(lev, main="Leverages", ylim=c(0, max(lev)), pch=19)
# abline(h = threshold_2p, col="red", lty=2)  # 2p/n
# abline(h = threshold_3p, col="blue", lty=2) # 3p/n
# abline(h = threshold_5p, col="green", lty=2) # 5p/n

# # Identify points interactively
# identify(lev) #  3 5 16 21 22 43 44 48


# #-------------------------------------------------------------------

# # 04

# # a

# # Remove observation 43
# data2 <- grocery_data[-43, ]

# # Fit the model without observation 43
# model_reduced <- lm(labor ~ shipped + cost + holiday, data = data2)

# # Compare regression coefficients
# summary(model)$coefficients
# summary(model_reduced)$coefficients

# # The "cost" coefficient changes dramatically, from -1.317 to -2.072 (57.3% change), 
# # While the "shipped" coefficient have moderate impact

# # b

# # Add observation 43
# data3 <- data2[43, ]

# # Predict the fitted value for observation 43 using both models
# fitted_full <- predict(model, newdata = data3)
# fitted_reduced <- predict(model_reduced, newdata = data3)

# # Print results
# cat("Fitted value from full model:", fitted_full, "\n")
# cat("Fitted value from reduced model:", fitted_reduced, "\n")


# # c

# # Observation 43 has some influence but is not highly influential based on the following points:
# # Regression Coefficient Changes (4a)
# # The removal of observation 43 significantly changed the coefficient of cost (~57% shift) and affected other coefficients moderately.
# # This suggests that observation 43 has some leverage in the regression model.

# # Fitted Value Changes (4b)
# # The predicted value for observation 43 only changed by 14.876 when removed.
# # This is a small relative difference (~0.34%), meaning observation 43 does not drastically affect predictions.

# # Final Conclusion
# # Observation 43 is moderately influential because it affects regression coefficients, but it does not strongly impact predictions.
# # If an outlier is truly influential, it would substantially change both coefficients and predicted values.
# # Since only the coefficients changed significantly, observation 43 has some influence but is not a critical outlier.
