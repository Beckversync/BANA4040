### Phân tích và Trực quan hóa Dữ liệu với R

Trong đoạn mã này, chúng ta thực hiện việc đọc, xử lý và trực quan hóa dữ liệu từ file Excel bằng ngôn ngữ R.

#### 1. Đọc dữ liệu từ file
```r
library(readxl)

# Đọc dữ liệu từ file "data.xlsx" (không có header)
jobs <- read_excel("data.xlsx", col_names = FALSE)
```
Ở đây, chúng ta sử dụng thư viện `readxl` để đọc dữ liệu từ file Excel. `col_names = FALSE` nghĩa là dữ liệu không có dòng tiêu đề.

#### 2. Gán tên cột cho dataframe
```r
# Gán tên cột cho dataframe
colnames(jobs) <- c("proficiency", "t1", "t2", "t3", "t4")
```
Sau khi đọc xong, chúng ta gán tên cột để dễ dàng thao tác và truy xuất dữ liệu.

#### 3. Kiểm tra dữ liệu
```r
# Hiển thị tóm tắt dữ liệu để kiểm tra
summary(jobs)
```
Hàm `summary()` giúp chúng ta kiểm tra nhanh các thông tin thống kê cơ bản như giá trị lớn nhất, nhỏ nhất, trung vị, v.v.

#### 4. Vẽ các biểu đồ scatterplots
```r
# Thiết lập vùng vẽ 2 x 2 để hiển thị 4 biểu đồ cùng lúc
par(mfrow = c(2, 2))
```
Chúng ta thiết lập vùng vẽ thành lưới 2x2 để hiển thị đồng thời 4 biểu đồ.

```r
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
```
Mỗi biểu đồ scatterplot thể hiện mối quan hệ giữa `proficiency` và từng bài kiểm tra (`t1`, `t2`, `t3`, `t4`).

#### 5. Khôi phục cấu hình vùng vẽ
```r
# Đặt lại cấu hình vùng vẽ về mặc định (1x1)
par(mfrow = c(1, 1))
```
Sau khi vẽ xong, chúng ta khôi phục lại vùng vẽ về chế độ hiển thị mặc định.

### Kết luận
Với đoạn mã này, chúng ta dễ dàng thực hiện việc đọc dữ liệu, gán tên cột, kiểm tra thông tin và trực quan hóa mối quan hệ giữa các biến qua các biểu đồ scatterplot. Điều này giúp ta đánh giá nhanh mức độ tương quan giữa `proficiency` và các bài kiểm tra.

