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


# # From clicking around, you'll find the point with the smallest Cp or BIC. 
# # In the table, we see that "t1 + t3" has the lowest BIC and lowest Cp.
