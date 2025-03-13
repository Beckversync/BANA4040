# README for Assignment 1: Model Selection in R

## Data Information
- **Data File:** `jobs.txt`
- **Columns:**
  - `y`: Job proficiency score (first column)
  - `t1`, `t2`, `t3`, `t4`: Scores on four aptitude tests
- Ensure to assign appropriate column headings after importing the data.

## Assignment Tasks 1
1. **Graphical Summaries:** Generate scatterplots of job proficiency against each predictor and interpret what the plots suggest.
2. **All Possible Regressions:** Fit all 16 possible models with the four predictors and record model selection metrics: `p`, `R2`, `R2a,p`, `PRESSp`, `AICp`, `BICp`, `Mallows Cp`.
3. **Best Model Selection:** Identify the best model based on each criterion and determine which variable(s) can be excluded.
4. **Model Fitting & Diagnostics:** Fit the best model according to `R2a,p` and assess if model assumptions are met.
5. **Best Model with Two Predictors:** Using `BICp` and `Mallows Cp`, find the best model with two or fewer predictors.
6. **Validation:** Compare `SSEp` and `PRESSp` for Model 11.
7. **Automated Search Procedures:** Perform forward selection, backward elimination, and stepwise regression using the `step()` function and compare selected models.
8. **Model Search with Conditions:** Use the `regsubsets()` function from the `leaps` package to find the best models based on `R2a,p`, `Mallows Cp`, and `BICp`.

## Package Requirements
- **DAAG Package:** For computing `PRESSp`
- **Leaps Package:** For `regsubsets()` function

## Additional Notes
- Ensure all plots and results are properly labeled and interpreted.
- Check that your models meet statistical assumptions and provide diagnostics when needed.
- Maintain clear and concise code structure and comments for readability.

##  Assignment Tasks 2 Outlier Detection in Grocery Data using R

This project aims to detect influential outliers in grocery data using R. The dataset contains weekly activity data from a national grocery retailer, and the analysis focuses on identifying anomalies in the response and predictor variables.

## Dataset Overview
- **File**: `grocery.xlsx`
- **Variables**:
  - **labor**: Total labor hours per week (response variable)
  - **shipped**: Number of cases shipped in a week
  - **cost**: Labor cost as a percentage of total costs
  - **holiday**: Binary indicator (1 if the week includes a holiday, 0 otherwise)

## Analysis Steps

### 1. Residual Analysis
We fit a first-order linear regression model without interaction effects and analyze different types of residuals:

```r
library(readxl)
data <- read_excel("grocery.xlsx")
result <- lm(labor ~ shipped + cost + holiday, data)

# Visualizing residuals
par(mfrow = c(1, 3))

# Plot of residuals vs fitted values
plot(result$fitted.values, result$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")

# Plot of standardized residuals vs fitted values
plot(result$fitted.values, rstandard(result),
     main = "Standardized Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Standardized Residuals")

# Plot of studentized residuals vs fitted values
plot(result$fitted.values, rstudent(result),
     main = "Studentized Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Studentized Residuals")

# Reset graphics layout
par(mfrow = c(1, 1))
```

### 2. Detecting Outliers in the Response Variable
We use studentized residuals and the Bonferroni correction to identify potential outliers:

```r
# Calculate sample size and number of predictors
n <- nrow(data)
p <- length(coef(result)) - 1

# Compute studentized residuals
student.res <- rstudent(result)

# Calculate critical value using Bonferroni correction
alpha <- 0.05
crit <- qt(1 - alpha / (2 * n), df = n - p - 1)

# Plot studentized residuals with critical thresholds
plot(student.res,
     main = "Studentized Residuals with Critical Values",
     ylab = "Studentized Residuals")
abline(h = c(crit, -crit), col = "red", lty = 2)

# Identify outliers
outliers <- which(abs(student.res) > crit)
print("Indices of potential outliers:")
print(outliers)
```

## How to Run the Analysis
1. Ensure that the `grocery.xlsx` file is in your working directory.
2. Open the R environment (e.g., RStudio, VSCode) and load the provided scripts.
3. Execute the code blocks in the order presented.

## Results and Observations
- **Visual analysis**: Residual plots highlight any patterns or anomalies in the data.
- **Outlier detection**: Identified observations with studentized residuals beyond the Bonferroni-corrected thresholds.
