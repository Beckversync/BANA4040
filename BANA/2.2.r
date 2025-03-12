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
