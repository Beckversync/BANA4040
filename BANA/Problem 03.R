# Problem 03

# Load the data
library(readxl)
# Đọc dữ liệu từ file "grocery.txt"
grocery_data <- read_excel("grocery.xlsx", col_names = TRUE)

# Assign column names
colnames(grocery_data) <- c("labor", "shipped", "cost", "holiday")


# 01
# Fit the linear regression model
result <- lm(labor ~ shipped + cost + holiday, data = grocery_data)

# Residual Analysis
par(mfrow=c(1,1))
plot(result$fitted.values, result$residuals, 
     main="Residuals vs Fitted Values",
     xlab="Fitted Values", ylab="Residuals", pch=20)
abline(h=0, col="red", lwd=2)

# Standardized Residuals vs Fitted Values
par(mfrow=c(2,1)) 
plot(result$fitted.values, rstandard(result), 
     main="Standardized Residuals vs Fitted Values",
     xlab="Fitted Values", ylab="Standardized Residuals", pch=20)
abline(h=c(-2,2), col="red", lwd=2, lty=2)

# Studentized Residuals vs Fitted Values
plot(result$fitted.values, rstudent(result), 
     main="Studentized Residuals vs Fitted Values",
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20)
abline(h=c(-2,2), col="red", lwd=2, lty=2)

# Detecting Outliers using Studentized Residuals
sorted_residuals <- sort(rstudent(result), decreasing = TRUE)
print(sorted_residuals)

# Bonferroni Outlier Test
n <- nrow(grocery_data)  # Number of observations
p <- length(coef(result))  # Number of predictors + intercept
alpha <- 0.05
bonferroni_cutoff <- qt(1 - alpha / (2 * n), df = n - p - 1)
print(paste("Bonferroni cutoff:", bonferroni_cutoff))

# Identifying outliers
outliers <- which(abs(rstudent(result)) > bonferroni_cutoff)
print(paste("Outliers detected at indices:", toString(outliers)))


#-------------------------------------------------------------------

# # 02

# # Load necessary package
# library(MASS)  # For rstudent()

# # Example dataset (replace with your data)
# set.seed(123)
# n <- 50  # Number of observations
# x1 <- rnorm(n)
# x2 <- rnorm(n)
# y <- 3 + 2*x1 - x2 + rnorm(n, sd=2)

# # Fit a linear regression model
# model <- lm(y ~ x1 + x2)

# # Compute studentized residuals
# student_res <- rstudent(model)

# # Bonferroni critical value
# alpha <- 0.05
# p <- length(coef(model))  # Number of predictors + intercept
# t_crit <- qt(1 - alpha / (2 * n), df = n - p - 1)  

# # Plot studentized residuals with critical values
# plot(student_res, ylim=c(-4,4), main="Studentized Residuals")
# abline(h = t_crit, col="red", lwd=2, lty=2)   # Upper threshold
# abline(h = -t_crit, col="red", lwd=2, lty=2)  # Lower threshold

# # Identify outliers
# outliers <- which(abs(student_res) > t_crit)
# cat("Outlier indices:", outliers, "\n")
# cat("Outlier values:", student_res[outliers], "\n")


#-------------------------------------------------------------------

# # 03

# # Get leverage values (h_ii)
# lev <- lm.influence(model)$hat  

# # Print summary of leverage values
# summary(lev)

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
# identify(lev)


# #-------------------------------------------------------------------

# # 04

# # a

# # Fit the full model
# model_full <- lm(labor ~ shipped + cost + holiday, data = grocery_data)

# # Remove observation 43
# data2 <- data[-43, ]

# # Fit the model without observation 43
# model_reduced <- lm(labor ~ shipped + cost + holiday, data = data2)

# # Compare regression coefficients
# summary(model_full)$coefficients
# summary(model_reduced)$coefficients


# # b

# # Predict the fitted value for observation 43 using both models
# fitted_full <- predict(model_full, newdata = data[43, ])
# fitted_reduced <- predict(model_reduced, newdata = data[43, ])

# # Print results
# cat("Fitted value from full model:", fitted_full, "\n")
# cat("Fitted value from reduced model:", fitted_reduced, "\n")


# #c

# # both the regression coefficients (4a) and the predicted values (4b) change significantly 
# # when removing observation 43, then it is influential.
