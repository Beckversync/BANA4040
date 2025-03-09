# README for Assignment 1: Model Selection in R

## Data Information
- **Data File:** `jobs.txt`
- **Columns:**
  - `y`: Job proficiency score (first column)
  - `t1`, `t2`, `t3`, `t4`: Scores on four aptitude tests
- Ensure to assign appropriate column headings after importing the data.

## Assignment Tasks
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

