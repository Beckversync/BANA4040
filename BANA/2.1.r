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

# Reset lại layout đồ họa về mặc định
par(mfrow = c(1,1))
