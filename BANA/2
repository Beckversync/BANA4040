library(readxl)

# Đọc dữ liệu từ file "jobs.txt" (không có header)
jobs <- read_excel("data.xlsx", col_names = FALSE)
colnames(jobs) <- c("proficiency","t1","t2","t3","t4")
# Hàm tính một loạt các chỉ số thống kê quan trọng cho việc chọn mô hình
model_stats <- function(fit, MSE_full){
  # fit: là đối tượng lm (mô hình hồi quy)
  # MSE_full: MSE của mô hình đầy đủ (có tất cả biến) để tính Mallows' Cp
  
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
results <- data.frame(
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
    
    results <- rbind(
      results,
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
      
      results <- rbind(
        results,
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
results

