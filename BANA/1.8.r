# install.packages("leaps")   # nếu chưa cài
library(leaps)
library(readxl)
# Ví dụ: Dữ liệu có cột: y, t1, t2, t3, t4
jobs_data <- read_excel("data.xlsx")
colnames(jobs_data) <- c("y", "t1", "t2", "t3", "t4")
# Tìm mô hình tốt nhất theo mọi số biến (từ 1 đến 4) trong {t1, t2, t3, t4}
# nbest=1 nghĩa là chỉ lấy 1 mô hình tốt nhất cho mỗi số biến.
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

summary_sub$which[best_cp_index, ]

summary_sub$which[best_bic_index, ]

# Lưu ý: force.in / force.out trong leaps thường là chỉ số cột nếu x,y là dạng ma trận
# Nhưng với công thức, một số phiên bản cho phép ta truyền tên trực tiếp (nếu không được, hãy dùng chỉ số).
fit_sub2 <- regsubsets(y ~ t1 + t2 + t3 + t4,
                       data = jobs_data,
                       method = "exhaustive",
                       nbest = 1,
                       nvmax = 3,
                       force.in = "t1",
                       force.out = "t2")

summary_sub2 <- summary(fit_sub2)


