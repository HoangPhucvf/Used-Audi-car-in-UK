folder_path <- "C:\\Users\\ADMIN\\Desktop\\duong"
list_of_files <- list.files(path = folder_path, pattern = "*.csv")
print(list_of_files)
temp_list <- list()
library(lmtest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(knitr)
library(car)       # Để tính VIF
library(sandwich)  # Robust standard errors
library(ggcorrplot)

for (file_name in list_of_files) {
  file_path <- file.path(folder_path, file_name)
  data <- read.csv(file_path)
  temp_list[[file_name]] <- data
  cat("Đã đọc file:", file_name, "\n")
}

combined_data_d <- do.call(rbind, temp_list) #data được kết hợp
head(combined_data_d)
# Hoặc xem cấu trúc của nó
str(combined_data_d)
View(combined_data_d)


data <- combined_data_d

data$year_mileage <- data$year * data$mileage
data$mpg_squared <- data$mpg^2
data$tax_mpg <- data$tax * data$mpg
data$tax_cubed <- data$tax^3

# Tạo mô hình hồi quy tuyến tính với log(price)
model <- lm(log(price) ~ year + mileage + transmission + fuelType +
              tax + mpg + engineSize + model +
              year_mileage + mpg_squared + tax_mpg + tax_cubed,
            data = data)

# Tóm tắt mô hình
summary(model)
resettest(model, power = 2:3)


# Hàm để vẽ residual plot cho một biến
plot_residual_vs_var <- function(var, var_name) {
  plot(var, resid(model), 
       xlab = var_name, 
       ylab = "Residuals", 
       main = paste("Residuals vs", var_name))
  abline(h = 0, col = "red", lty = 2)
  lines(lowess(var, resid(model)), col = "blue")
}

# Vẽ residual plot cho từng biến liên tục
par(mfrow = c(3, 3)) # Sắp xếp các biểu đồ thành lưới 3x3
plot_residual_vs_var(data$year, "Year")
plot_residual_vs_var(data$mileage, "Mileage")
plot_residual_vs_var(data$tax, "Tax")
plot_residual_vs_var(data$mpg, "MPG")
plot_residual_vs_var(data$engineSize, "Engine Size")
plot_residual_vs_var(data$year_mileage, "Year * Mileage")
plot_residual_vs_var(data$mpg_squared, "MPG^2")
plot_residual_vs_var(data$tax_mpg, "Tax * MPG")
plot_residual_vs_var(data$tax_cubed, "Tax^3")
par(mfrow = c(1, 1)) # Reset lại bố cục

# Đối với các biến dummy (transmission, fuelType), dùng boxplot
# Residuals theo transmission
boxplot(resid(model) ~ data$transmission, 
        xlab = "Transmission", 
        ylab = "Residuals", 
        main = "Residuals vs Transmission")

# Residuals theo fuelType
boxplot(resid(model) ~ data$fuelType, 
        xlab = "Fuel Type", 
        ylab = "Residuals", 
        main = "Residuals vs Fuel Type")

#vẽ dis của residual
# Tính residuals
residuals <- residuals(model)

# Vẽ biểu đồ phân phối
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#69b3a2", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Residuals",
       x = "Residuals", y = "Density") +
  theme_minimal()


residuals <- residuals(model)
fitted_values <- fitted(model)

# Vẽ residual plot
plot(fitted_values, residuals,
     main="Residual Plot", 
     xlab="Fitted Values (Predicted)", 
     ylab="Residuals", 
     pch=19, 
     col="blue")

# Thêm đường ngang tại y = 0
abline(h=0, col="red", lwd=2)

gqtest(model)
#GQ = 0.80704, df1 = 5280, df2 = 5279, p-value = 1
#alternative hypothesis: variance increases from segment 1 to 2

bptest(model)  #p-value < 2.2e-16 < 0.05 -> có hetero


white_test <- function(model) {
  u2 <- residuals(model)^2                      # lấy bình phương residuals
  X <- model.matrix(model)                      # ma trận thiết kế từ mô hình gốc
  X2 <- X^2                                     # bình phương các biến (tương tác bậc hai)
  X_white <- cbind(X, X2[, -1])                 # bỏ intercept bình phương, rồi nối với X gốc
  aux_model <- lm(u2 ~ X_white - 1)             # mô hình phụ không intercept
  n <- nobs(model)                              # số quan sát
  R2 <- summary(aux_model)$r.squared
  LM <- n * R2                                  # White's test statistic
  df <- ncol(X_white)                           # bậc tự do
  pval <- 1 - pchisq(LM, df)
  cat("White Test:\n")
  cat("LM =", LM, ", df =", df, ", p-value =", pval, "\n")
}
white_test(model)
#White Test:
#LM = 2916.727 , df = 27 , p-value = 0  có hetero
#mô hình có hetero


# price ~ year
ggplot(data = combined_data_d, aes(x = year, y =price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: price vs year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# price ~ mileage
ggplot(data = combined_data_d, aes(x = mileage, y = price))+
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: price vs mileage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# price ~ tax
ggplot(data = combined_data_d, aes(x = tax, y = price))+
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: price vs tax") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))






# log(price) ~ year
ggplot(data = combined_data_d, aes(x = year, y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: log(price) vs year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# log(price) ~ mileage
ggplot(data = combined_data_d, aes(x = mileage, y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: log(price) vs mileage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# log(price) ~ tax
ggplot(data = combined_data_d, aes(x = tax, y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: log(price) vs tax") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# log(price) ~ mpg
ggplot(data = combined_data_d, aes(x = mpg, y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: log(price) vs mpg") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# log(price) ~ engineSize
ggplot(data = combined_data_d, aes(x = engineSize, y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: log(price) vs engineSize") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = combined_data_d, aes(x = fuelType, y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: log(price) vs fuel") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = combined_data_d, aes(x = transmission, y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "red") +
  ggtitle("Linear Regression: log(price) vs transmission") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Lấy residuals từ mô hình
residuals <- resid(model)


# 1. Residual Plot cho Year
plot_residual_vs_var(data$year, "Year", residuals)

# 2. Residual Plot cho Mileage
plot_residual_vs_var(data$mileage, "Mileage", residuals)

# 3. Residual Plot cho Tax
plot_residual_vs_var(data$tax, "Tax", residuals)

# 4. Residual Plot cho MPG
plot_residual_vs_var(data$mpg, "MPG", residuals)

# 5. Residual Plot cho Engine Size
plot_residual_vs_var(data$engineSize, "Engine Size", residuals)

# 6. Residual Plot cho Year * Mileage
plot_residual_vs_var(data$year_mileage, "Year * Mileage", residuals)

# 7. Residual Plot cho MPG^2
plot_residual_vs_var(data$mpg_squared, "MPG^2", residuals)

# 8. Residual Plot cho Tax * MPG
plot_residual_vs_var(data$tax_mpg, "Tax * MPG", residuals)

# 9. Residual Plot cho Tax^3
plot_residual_vs_var(data$tax_cubed, "Tax^3", residuals)

# 10. Residual Plot cho Transmission (dùng boxplot)
boxplot(resid(model) ~ data$transmission, 
        xlab = "Transmission", 
        ylab = "Residuals", 
        main = "Residual Plot: Transmission",
        col = rgb(0, 0, 1, alpha = 0.2),  # Màu xanh đậm tương tự
        border = "black")

# 11. Residual Plot cho Fuel Type (dùng boxplot)
boxplot(resid(model) ~ data$fuelType, 
        xlab = "Fuel Type", 
        ylab = "Residuals", 
        main = "Residual Plot: Fuel Type",
        col = rgb(0, 0, 1, alpha = 0.2),  # Màu xanh đậm tương tự
        border = "black")










#residuals vs fitted plot
plot(model, which = 1)
library(lmtest)
bptest(model) #BP test
library(whitestrap)
#white test
white_test <- function(model) {
  # Lấy phần dư và giá trị fitted
  residuals <- residuals(model)
  fitted <- fitted(model)
  # Tạo dataframe mới với bình phương và tích chéo
  white_data <- data.frame(
    res_sq = residuals^2,
    fitted_sq = fitted^2,
    fitted = fitted
  )
  # Hồi quy phần dư bình phương theo các biến
  white_model <- lm(res_sq ~ fitted + fitted_sq, data = white_data)
  summary(white_model)
}
white_test(model)

coeftest(model, vcov = vcovHC(model, type = "HC3"))
