# Giả sử dữ liệu đã được đọc và có các cột: y, t1, t2, t3, t4
# Ví dụ:
library(readxl)
jobs_data <- read_excel("data.xlsx", col_names = FALSE)
colnames(jobs_data) <- c("y", "t1", "t2", "t3", "t4")

# Mô hình "null" chỉ có intercept và mô hình đầy đủ có tất cả các biến
null_model <- lm(y ~ 1, data = jobs_data)
full_model <- lm(y ~ t1 + t2 + t3 + t4, data = jobs_data)

#-----------------------------------------------
# 1. Forward Selection: Bắt đầu từ null_model, thêm dần biến
#-----------------------------------------------
forward_model <- step(null_model,
                      scope = list(lower = null_model, upper = full_model),
                      direction = "forward")
cat("Forward Selection Model:\n")
print(summary(forward_model))


#-----------------------------------------------
# 2. Backward Elimination: Bắt đầu từ full_model, loại bỏ dần biến không cần thiết
#-----------------------------------------------
backward_model <- step(full_model, direction = "backward")
cat("\nBackward Elimination Model:\n")
print(summary(backward_model))


#-----------------------------------------------
# 3. Stepwise Regression (both directions): Kết hợp thêm và loại bỏ biến
#-----------------------------------------------
stepwise_model <- step(null_model,
                       scope = list(lower = null_model, upper = full_model),
                       direction = "both")
cat("\nStepwise Regression Model:\n")
print(summary(stepwise_model))
