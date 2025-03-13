#-----------------------------------------------
# 1) Chuẩn bị dữ liệu và gói cần thiết
#-----------------------------------------------
# install.packages("DAAG") # cài nếu chưa có
library(DAAG)
# install.packages("readxl") # cài nếu cần đọc Excel
library(readxl)

# Giả sử bạn đã đọc dữ liệu vào jobs_data:
jobs_data <- read_excel("data.xlsx", col_names = FALSE)
colnames(jobs_data) <- c("y", "t1", "t2", "t3", "t4")

#-----------------------------------------------
# 2) Tạo danh sách tất cả các tổ hợp biến
#-----------------------------------------------
predictors <- c("t1", "t2", "t3", "t4")

# Hàm combn() sẽ lấy tất cả tổ hợp k phần tử từ danh sách, 
# lapply(...) để duyệt k = 1..4, rồi unlist(..., recursive=FALSE) gộp lại thành một list.
subset_list <- unlist(
  lapply(1:length(predictors), function(k) {
    combn(predictors, k, simplify = FALSE)
  }),
  recursive = FALSE
)

#-----------------------------------------------
# 3) Khởi tạo khung kết quả
#-----------------------------------------------
results <- data.frame(
  model = character(),
  SSE   = numeric(),
  PRESS = numeric(),
  stringsAsFactors = FALSE
)

# Số dòng (số quan sát)
n <- nrow(jobs_data)

#-----------------------------------------------
# 4) Vòng lặp: ước lượng từng mô hình, tính SSE & PRESS
#-----------------------------------------------
for (vars in subset_list) {
  # Tạo chuỗi công thức, ví dụ "y ~ t1 + t3" v.v.
  formula_str <- paste("y ~", paste(vars, collapse = " + "))
  
  # Fit mô hình
  fit <- lm(as.formula(formula_str), data = jobs_data)
  
  # Tính SSE:
  # - Cách 1: Dùng công thức SSE = sigma^2 * (n - p),
  #           trong đó p = số tham số (kể cả intercept).
  p <- length(coef(fit))               # số tham số ước lượng
  rse <- summary(fit)$sigma            # residual standard error
  SSEp <- rse^2 * (n - p)
  
  # - Cách 2 (tương đương): SSEp = sum(resid(fit)^2)
  # SSEp <- sum(resid(fit)^2)

  # Tính PRESS:
  PRESSp <- press(fit)  # từ gói DAAG

  # Lưu kết quả
  results <- rbind(results, data.frame(
    model = formula_str,
    SSE   = SSEp,
    PRESS = PRESSp
  ))
}

#-----------------------------------------------
# 5) Xem kết quả
#-----------------------------------------------
print(results)


