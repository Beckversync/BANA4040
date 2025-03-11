# Problem 1 , part 1:

library(readxl)
jobs <- read_excel("data.xlsx", col_names = FALSE)
colnames(jobs) <- c("proficiency", "t1", "t2", "t3", "t4")
summary(jobs)
par(mfrow = c(2, 2))

# Biểu đồ proficiency vs t1
plot(jobs$t1, jobs$proficiency,
     xlab = "Test 1 (t1)",
     ylab = "Job Proficiency",
     main = "Proficiency vs t1")
# Biểu đồ proficiency vs t2
plot(jobs$t2, jobs$proficiency,
     xlab = "Test 2 (t2)",
     ylab = "Job Proficiency",
     main = "Proficiency vs t2")
# Biểu đồ proficiency vs t3
plot(jobs$t3, jobs$proficiency,
     xlab = "Test 3 (t3)",
     ylab = "Job Proficiency",
     main = "Proficiency vs t3")
# Biểu đồ proficiency vs t4
plot(jobs$t4, jobs$proficiency,
     xlab = "Test 4 (t4)",
     ylab = "Job Proficiency",
     main = "Proficiency vs t4")
par(mfrow = c(1, 1)) # Đặt lại cấu hình vùng vẽ về mặc định (1x1)

# Problem 1 , part 2:

model_stats <- function(fit, MSE_full){
  n <- length(fit$residuals)    # số quan sát
  p <- length(coef(fit))        # số tham số ước lượng (k + 1, kể cả intercept)
  # R^2 và R^2 hiệu chỉnh
  r2     <- summary(fit)$r.squared
  adjr2  <- summary(fit)$adj.r.squared
  # SSE_p (Tổng bình phương sai số của mô hình fit)
  sse_p  <- sum(resid(fit)^2)
  # Tính PRESS = Σ [ e_i / (1 - h_ii) ]^2
  hat    <- lm.influence(fit)$hat
  res    <- resid(fit)
  press  <- sum((res/(1 - hat))^2)
  # AIC và BIC
  aic_val <- AIC(fit)
  bic_val <- BIC(fit)
  # Mallows' Cp = SSE_p / MSE_full - (n - 2p)
  cp_val  <- sse_p / MSE_full - (n - 2*p)
  return(c(p, r2, adjr2, press, aic_val, bic_val, cp_val))
}
# Mô hình đầy đủ
lm_full <- lm(proficiency ~ t1 + t2 + t3 + t4, data = jobs)
# SSE_full = Tổng bình phương sai số của mô hình đầy đủ
SSE_full <- sum(resid(lm_full)^2)
# Kích thước mẫu
n <- nrow(jobs)
# p_full = số tham số của mô hình đầy đủ (4 biến + intercept = 5)
p_full <- length(coef(lm_full)) 
# MSE_full
MSE_full <- SSE_full / (n - p_full)
# Chuẩn bị data frame lưu kết quả
resultsp2 <- data.frame(
  Model   = character(),
  p       = numeric(),  # số tham số (k + 1)
  R2      = numeric(),
  AdjR2   = numeric(),
  PRESS   = numeric(),
  AIC     = numeric(),
  BIC     = numeric(),
  Cp      = numeric(),
  stringsAsFactors = FALSE
)
preds <- c("t1", "t2", "t3", "t4")
# Tính cho tất cả các kích cỡ tập con k = 0..4
for (k in 0:4) {
  # Lấy tất cả tổ hợp k phần tử trong preds
  subset_list <- combn(preds, k)
  # Trường hợp k=0 nghĩa là không có biến dự báo (chỉ intercept)
  if(k == 0){
    form <- as.formula("proficiency ~ 1")  # chỉ intercept
    fit <- lm(form, data = jobs)
    stats <- model_stats(fit, MSE_full)
    resultsp2 <- rbind(
      resultsp2,
      data.frame(
        Model = "Intercept Only",
        p     = stats[1],
        R2    = stats[2],
        AdjR2 = stats[3],
        PRESS = stats[4],
        AIC   = stats[5],
        BIC   = stats[6],
        Cp    = stats[7],
        stringsAsFactors = FALSE
      )
    )
  } else {
    # Nếu k>0, ta lần lượt duyệt qua các tổ hợp
    for (i in 1:ncol(subset_list)) {
      vars <- subset_list[, i]
      # Tạo công thức hồi quy
      form <- as.formula(paste("proficiency ~", paste(vars, collapse = " + ")))
      # Fit mô hình
      fit <- lm(form, data = jobs)
      stats <- model_stats(fit, MSE_full)
      resultsp2 <- rbind(
        resultsp2,
        data.frame(
          Model = paste(vars, collapse = " + "),
          p     = stats[1],
          R2    = stats[2],
          AdjR2 = stats[3],
          PRESS = stats[4],
          AIC   = stats[5],
          BIC   = stats[6],
          Cp    = stats[7],
          stringsAsFactors = FALSE
        )
      )
    }
  }
}
# Xem kết quả
View(resultsp2)


# Problem 1 , part 5:
# 1. Suppose we have the following data for models with p ≤ 3:
modelNames <- c("Intercept only", 
                "t1", "t2", "t3", "t4",
                "t1 + t2", "t1 + t3", "t1 + t4",
                "t2 + t3", "t2 + t4", "t3 + t4")

p <- c(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)  # total parameters (intercept + #predictors)

BICvals <- c(224.6868, 220.2216, 220.8130, 187.0721, 192.6581,
             215.5250, 163.5496, 188.9037, 190.1177, 192.8944, 178.6830)

Cpvals  <- c(515.9646, 375.3447, 384.8325, 84.2465, 110.5974,
             269.7800, 17.1130, 80.5653, 85.5196, 97.7978, 47.1540)
par(mfrow = c(1,2))  # Chia vùng vẽ thành 1 hàng, 2 cột
# Vẽ BIC
plot(BICvals, 
     xlab = "Model index", 
     ylab = "BIC", 
     main = "BIC for Models (p ≤ 3)",
     pch = 19)
# Vẽ Cp
plot(Cpvals, 
     xlab = "Model index", 
     ylab = "Mallow's Cp", 
     main = "Cp for Models (p ≤ 3)",
     pch = 19)
# Reset lại vùng vẽ về mặc định (tránh ảnh hưởng các lệnh sau)
par(mfrow = c(1,1))


identify(BICvals, labels = modelNames)
identify(Cpvals, labels = modelNames)

# Problem 1 , part 6:
library(DAAG)
library(readxl)

predictors <- c("t1", "t2", "t3", "t4")
subset_list <- unlist(
  lapply(1:length(predictors), function(k) {
    combn(predictors, k, simplify = FALSE)
  }),
  recursive = FALSE
)
resultsp5 <- data.frame(
  model = character(),
  SSE   = numeric(),
  PRESS = numeric(),
  stringsAsFactors = FALSE
)

n <- nrow(jobs_data)
for (vars in subset_list) {

  formula_str <- paste("y ~", paste(vars, collapse = " + "))
  
  fit <- lm(as.formula(formula_str), data = jobs_data)

  p <- length(coef(fit))               # số tham số ước lượng
  rse <- summary(fit)$sigma            # residual standard error
  SSEp <- rse^2 * (n - p)

  PRESSp <- press(fit)  # từ gói DAAG

  resultsp5 <- rbind(results, data.frame(
    model = formula_str,
    SSE   = SSEp,
    PRESS = PRESSp
  ))
}

View(resultsp5)
# Problem 1 , part 7:
library(readxl)
library(ggplot2)
library(MASS)
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

# Problem 1 , part 8:
# install.packages("leaps")   # nếu chưa cài
library(leaps)
fit_sub <- regsubsets(y ~ t1 + t2 + t3 + t4,
                      data = jobs_data,
                      method = "exhaustive",  # duyệt tất cả tổ hợp
                      nbest = 1,
                      nvmax = 4)              # tối đa 4 biến (có thể ít hơn)
summary_sub <- summary(fit_sub)

# summary_sub$which  => Ma trận TRUE/FALSE cho biết biến nào được chọn
# summary_sub$rsq    => R^2
# summary_sub$adjr2  => Adjusted R^2
# summary_sub$cp     => Mallow's Cp
# summary_sub$bic    => BIC
# 1) Mô hình tốt nhất theo Adjusted R^2 (lớn nhất)
best_adjr2_index <- which.max(summary_sub$adjr2)

# 2) Mô hình tốt nhất theo Mallow's Cp (nhỏ nhất)
best_cp_index <- which.min(summary_sub$cp)

# 3) Mô hình tốt nhất theo BIC (nhỏ nhất)
best_bic_index <- which.min(summary_sub$bic)

# Kiểm tra biến nào được chọn ở mô hình best_adjr2_index
summary_sub$which[best_adjr2_index, ]
fit_sub2 <- regsubsets(y ~ t1 + t2 + t3 + t4,
                       data = jobs_data,
                       method = "exhaustive",
                       nbest = 1,
                       nvmax = 3,
                       force.in = "t1",
                       force.out = "t2")

summary_sub2 <- summary(fit_sub2)
summary_sub2$which
summary_sub2$adjr2
summary_sub2$cp
summary_sub2$bic

# Problem 2 , part 1:
library(readxl)
# Đọc dữ liệu từ file "grocery.txt"
data <- read_excel("grocery.xlsx", col_names = TRUE)

# Xây dựng mô hình hồi quy tuyến tính với các biến dự báo: shipped, cost, và holiday
result <- lm(labor ~ shipped + cost + holiday, data = data)

# Thiết lập cửa sổ đồ họa chia thành 1 hàng, 3 cột để vẽ 3 biểu đồ cạnh nhau
par(mfrow = c(1, 3))

# Biểu đồ Residuals vs Fitted Values
plot(result$fitted.values, result$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals",
     pch = 19)
abline(h = 0, col = "red")

# Biểu đồ Standardized Residuals vs Fitted Values
plot(result$fitted.values, rstandard(result),
     main = "Standardized Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Standardized Residuals",
     pch = 19)
abline(h = 0, col = "red")

# Biểu đồ Studentized Residuals vs Fitted Values
plot(result$fitted.values, rstudent(result),
     main = "Studentized Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Studentized Residuals",
     pch = 19)
abline(h = 0, col = "red")

par(mfrow = c(1,1))

# Problem 2 , part 2:
library(readxl)
data <- read_excel("grocery.xlsx", col_names = TRUE)

# Xây dựng mô hình hồi quy first order với các biến dự đoán: shipped, cost, và holiday
fit <- lm(labor ~ shipped + cost + holiday, data = data)

# Lấy số quan sát (n) và số biến dự báo (p)
n <- nrow(data)
p <- length(coef(fit)) - 1  # trừ đi hệ số intercept

# Tính phần dư studentized (rstudent)
student.res <- rstudent(fit)

# --- (a) Sắp xếp các phần dư studentized và in ra
sorted_res <- sort(student.res)
print("Sorted studentized residuals:")
print(sorted_res)

# Tính giá trị tới hạn theo phương pháp Bonferroni
# Sử dụng α = 0.05. Giá trị tới hạn: qt(1 - α/(2*n), df = n - p - 1)
alpha <- 0.05
crit <- qt(1 - alpha/(2*n), df = n - p - 1)
print("Critical value (Bonferroni):")
print(crit)

# --- (b) Vẽ biểu đồ phần dư studentized và overlay các đường giới hạn tới hạn
plot(student.res, 
     ylab = "Studentized Residuals", 
     main = "Studentized Residuals with Bonferroni Critical Lines",
     ylim = c(-4, 4))
abline(h = crit, col = "red", lty = 2)
abline(h = -crit, col = "red", lty = 2)

# --- (c) Liệt kê các quan sát có |studentized residual| > giá trị tới hạn
outliers <- which(abs(student.res) > crit)
print("Indices of potential outliers (|t_i| > critical value):")
print(outliers)
