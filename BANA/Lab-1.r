# Library =============================================================================
library(leaps) # for regsubset()
library(DAAG) # for press()
library(MASS) # for model selection
library(ggplot2)
library(lars)

# Load dataset
teacher <- read.table("teacher_pay.txt", header=TRUE, sep="")
attach(teacher)

# Part 1####
# Convert AREA variable into a factor with appropriate labels
teacher$AREA <- factor(teacher$AREA, levels = c(1, 2, 3), 
                           labels = c("South", "West", "NE/NC"))

# Part 2####
# Fit a linear model with interaction terms
teacher_mdl <- lm(PAY ~ AREA + SPEND + AREA * SPEND, data = teacher)
summary(teacher_mdl)

# Part 3####
# Check assumptions:

# Assumption 1: Linearity => Satisfied
plot(teacher_mdl$residuals ~ teacher_mdl$fitted.values,
     main="Residuals against Fitted Values",
     xlab="Fitted Values",
     ylab="Residuals")
abline(h=0, col="red")

# Assumption 2: Mean of errors = 0 => Satisfied
mean(teacher_mdl$residuals) #-6.408817e-14 -> close to 0

# Assumption 3: Homoscedasticity => Satisfied
library(lmtest)
bptest(teacher_mdl) # p-value = 0.2957 > 0.05

# Assumption 4: Autocorrelation of errors => Violated
acf(teacher_mdl$residuals, main="Autocorrelation of Residuals")
# => Autocorrelation detected

# Assumption 5: Normality of errors => Satisfied
qqnorm(teacher_mdl$residuals)
qqline(teacher_mdl$residuals, col="red")

# Insights:
# + Satisfies linearity, mean of errors = 0, and homoscedasticity but violates autocorrelation.
# + Residuals are not independent and exhibit a pattern across observations.

# Part 4####
# Perform best subset selection
best_model <- regsubsets(PAY ~ ., data = teacher, nvmax = 5)
best_summary <- summary(best_model)

# Select model based on BIC
best_bic_index <- which.min(best_summary$bic)

# Identify the best model's predictors
best_bic_predictors <- best_summary$which[best_bic_index,]
print(best_bic_predictors)

# "
# (Intercept)       SPEND    AREAWest   AREANE/NC 
#        TRUE        TRUE       FALSE        TRUE 
# "

# Plot model selection criteria
par(mfrow=c(1,3))
plot(best_model, scale = "adjr2")  # Best model based on Adjusted R²
plot(best_model, scale = "bic")    # Best model based on BIC
plot(best_model, scale = "Cp")     # Best model based on Mallow’s Cp
par(mfrow=c(1,1))

# Compare stepwise selection methods
start <- lm(PAY~1, data=teacher) # Model with intercept only
end <- lm(PAY~., data=teacher)

# Forward selection
fwd_model <- step(start,
                  scope=list(lower=start, upper=end),
                  direction="forward",
                  trace=FALSE)
summary(fwd_model)

# Backward elimination
bwd_model <- step(end,
                  direction="backward",
                  trace=FALSE)
summary(bwd_model)

# Stepwise (both directions)
stw_model <- step(start,
                  scope=list(lower=start, upper=end),
                  direction="both",
                  trace=FALSE)
summary(stw_model)

# => All three methods align with BIC selection.
# Final model includes SPEND and AREANE/NC.

# Part 5####
final_model <- lm(PAY ~ SPEND + AREA, data = teacher)  # Replace with actual predictors
summary(final_model)

# Compute leverage scores
leverage_scores <- hatvalues(final_model)

# Compute studentized residuals
student_residuals <- rstudent(final_model)

# Plot studentized residuals vs leverage scores
ggplot(data = data.frame(Leverage = leverage_scores, Studentized_Residuals = student_residuals), 
       aes(x = Leverage, y = Studentized_Residuals)) +
  geom_point() +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "blue") + # Mild outlier threshold
  ggtitle("Studentized Residuals vs Leverage Scores") +
  xlab("Leverage Scores") +
  ylab("Studentized Residuals") +
  theme_minimal()

# Part 6####
# Compute leverage scores for each observation
leverage_scores <- hatvalues(teacher_mdl)

# Add leverage values to the dataset
teacher$leverage <- leverage_scores

# Get the number of observations (n) and number of parameters (p)
n <- nrow(teacher)
p <- length(coef(teacher_mdl))

# Compute leverage threshold using (2 * p) / n
leverage_threshold <- (2 * p) / n

# Identify high-leverage observations
high_leverage_states <- teacher[teacher$leverage > leverage_threshold, ]

# Print high-leverage observations
print(high_leverage_states)

# Plot leverage scores for each state
plot(leverage_scores, main = "Leverage Scores", ylab = "Leverage", xlab = "States", pch = 16)
abline(h = leverage_threshold, col = "red", lty = 2)  # Add threshold line

# Identify points interactively
identify(1:n, leverage_scores, labels = rownames(teacher), cex = 0.7)

# Part 7####
# Compute leverage scores and determine high-leverage observations
lev <- hatvalues(model_full)

# Compute leverage threshold
threshold <- 2 * (p) / n
cat("High leverage threshold: ", threshold, "\n")

# Identify high-leverage observations
high_lev_indices <- which(lev > threshold)
cat("Indices of high leverage:", high_lev_indices, "\n\n")

# Compare predictions with and without high-leverage observations
for(i in high_lev_indices) {
  full_pred <- predict(model_full, newdata = teacher[i,])
  
  model_loo <- lm(PAY ~ AREA * SPEND, data = teacher[-i,])
  loo_pred <- predict(model_loo, newdata = teacher[i,])
  
  cat("Observation (state) ", i, ":\n")
  if("STATE" %in% colnames(teacher)) {
    cat("  State:", teacher[i, "STATE"], "\n")
  }
  cat("  Full model prediction: ", full_pred, "\n")
  cat("  Leave-one-out prediction: ", loo_pred, "\n")
  cat("  Difference (Full - LOO): ", full_pred - loo_pred, "\n\n")
}

# Part 8####
# Compute Cook's distance
cooks_d <- cooks.distance(model_full)

# Compute leverage scores
threshold <- 2 * (p + 1) / n
high_lev_indices <- which(lev > threshold)

# Plot Cook's distance
plot(cooks_d, type = "h", 
     main = "Cook's Distance for Each Observation", 
     xlab = "Observation Index", 
     ylab = "Cook's Distance")

# Highlight high-leverage observations
points(high_lev_indices, cooks_d[high_lev_indices], col = "red", pch = 19)

# Add cutoff line
cutoff <- 4 / (n - p - 1)
abline(h = cutoff, col = "blue", lty = 2)

# Add legend
legend("topright", legend = c("Cook's Distance", "High Leverage", paste("Cutoff =", round(cutoff, 3))),
       col = c("black", "red", "blue"), lty = c(1, NA, 2), pch = c(NA, 19, NA))

# Part 9####
# Re-estimate models after removing high-leverage observations
model_list <- list("Full Model" = model_full)
for (i in high_lev_indices) {
  model_i <- lm(PAY ~ AREA * SPEND, data = teacher[-i,])
  model_list[[paste("Without Obs", i)]] <- model_i
}

# Store model coefficients in a matrix
coeff_matrix <- matrix(NA, nrow = length(model_list), ncol = length(coef_names))
rownames(coeff_matrix) <- names(model_list)
colnames(coeff_matrix) <- coef_names

for(j in 1:length(model_list)) {
  coeff_matrix[j, ] <- coef(model_list[[j]])
}

# Plot coefficient comparison
matplot(t(coeff_matrix), type = "b", pch = 1, lty = 1, col = 1:nrow(coeff_matrix),
        xlab = "Coefficient Name", ylab = "Estimated Coefficient", xaxt = "n",
        main = "Comparison of Coefficients (Full Model & Leave-One-Out)")
axis(1, at = 1:length(coef_names), labels = coef_names)

# Print coefficient matrix
print(coeff_matrix)
