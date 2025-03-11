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