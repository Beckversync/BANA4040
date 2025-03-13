library(readxl)
library(ggplot2)
library(MASS)

# Đọc dữ liệu
jobs_data <- read_excel("data.xlsx", col_names = FALSE)
colnames(jobs_data) <- c("y", "t1", "t2", "t3", "t4")

# Mô hình null (chỉ có intercept) và mô hình đầy đủ
null_model <- lm(y ~ 1, data = jobs_data)
full_model <- lm(y ~ t1 + t2 + t3 + t4, data = jobs_data)

# Forward Selection
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Backward Elimination
backward_model <- step(full_model, direction = "backward")

# Stepwise Regression
stepwise_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both")

# Thêm dự đoán vào dữ liệu
jobs_data$pred_forward <- predict(forward_model, jobs_data)
jobs_data$pred_backward <- predict(backward_model, jobs_data)
jobs_data$pred_stepwise <- predict(stepwise_model, jobs_data)

# Biểu đồ so sánh giá trị thực tế và dự đoán
ggplot(jobs_data, aes(x = y)) +
  geom_point(aes(y = pred_forward, color = "Forward Selection")) +
  geom_point(aes(y = pred_backward, color = "Backward Elimination")) +
  geom_point(aes(y = pred_stepwise, color = "Stepwise Regression")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  labs(title = "Actual vs Predicted Values", x = "Actual y", y = "Predicted y") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green"))

# Biểu đồ Residuals để kiểm tra mô hình
ggplot(jobs_data, aes(x = pred_forward, y = residuals(forward_model))) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot - Forward Selection", x = "Predicted Values", y = "Residuals") +
  theme_minimal()

# So sánh AIC của các mô hình
aic_values <- data.frame(
  Model = c("Forward Selection", "Backward Elimination", "Stepwise Regression"),
  AIC = c(AIC(forward_model), AIC(backward_model), AIC(stepwise_model))
)

ggplot(aic_values, aes(x = Model, y = AIC, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "AIC Comparison of Models", y = "AIC Value") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue", "green"))

  # In summary các mô hình
cat("Forward Selection Model Summary:\n")
print(summary(forward_model))

cat("\nBackward Elimination Model Summary:\n")
print(summary(backward_model))

cat("\nStepwise Regression Model Summary:\n")
print(summary(stepwise_model))

# In bảng AIC
aic_values <- data.frame(
  Model = c("Forward Selection", "Backward Elimination", "Stepwise Regression"),
  AIC = c(AIC(forward_model), AIC(backward_model), AIC(stepwise_model))
)
print(aic_values)
