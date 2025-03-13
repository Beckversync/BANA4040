library(readxl)
jobs <- read_excel("data.xlsx", col_names = FALSE)
colnames(jobs) <- c("proficiency", "t1", "t2", "t3", "t4")
summary(jobs)
par(mfrow = c(2, 2))
# Assuming the data frame is 'jobs' with columns proficiency, t1, t2, t3, t4:
best_model <- lm(proficiency ~ t1 + t3 + t4, data = jobs)
summary(best_model)
plot(best_model, which = 1)  # Residuals vs Fitted
plot(best_model, which = 2)  # Normal Q-Q plot
plot(best_model, which = 3)  # Scale-Location plot (spread vs. fitted)
plot(best_model, which = 4)  # Cook's distance
plot(best_model, which = 5)  # Residuals vs Leverage
