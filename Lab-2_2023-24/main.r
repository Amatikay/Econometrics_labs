## Генерация нормальных распределений
n <- 40
norm_sample1 <- rnorm(n,mean = 10,sd = 5)
norm_sample2 <- rnorm(n,mean = 30,sd = 5)
norm_sample3 <- rnorm(n,mean = 3,sd = 5)

# Общее среднее и частные средние
cat("\n\t### Общее среднее и частные средние ###\n")
cat("Частные средние:\n")

norm_sample1_mean <- mean(norm_sample1)
norm_sample2_mean <- mean(norm_sample2)
norm_sample3_mean <- mean(norm_sample3)

cat ("Выборка №1: ",norm_sample1_mean, "\n")
cat ("Выборка №2: ",norm_sample2_mean, "\n")
cat ("Выборка №3: ",norm_sample3_mean, "\n")



norm_sample <- c(norm_sample1, norm_sample2 , norm_sample3)

norm_sample_mean <- mean(norm_sample)

cat("Общее среднее: ", norm_sample_mean, "\n")


## Графики для средних

# Создание ящика для каждой выборки
boxplot(norm_sample1, norm_sample2, norm_sample3, col = c("lightblue", "lightgreen", "lightcoral"))

# Добавление горизонтальной линии для общего среднего
abline(h = norm_sample_mean, col = "red")


## Дисперсии

sigma_squared <- 1/n * sum((norm_sample - norm_sample_mean)^2)

sigma1_squared <- 1/n * sum((norm_sample1 - norm_sample1_mean)^2)
sigma2_squared <- 1/n * sum((norm_sample2 - norm_sample1_mean)^2)
sigma3_squared <- 1/n * sum((norm_sample3 - norm_sample1_mean)^2)

# Средняя внутригрупповая дисперсия. | Как правильно это написать на английском? 
sigma_insidegroup_squared <- 1/n * sum(c(sigma1_squared * n, sigma2_squared * n, sigma3_squared * n))

#Межгрупповая дисперсия
sigma_betwines_squared <- 1/n * sum((c(norm_sample1_mean, norm_sample2_mean, norm_sample3_mean)- norm_sample)^2 * n)

cat("\n\t### Дисперсии ###\n")
cat("Общая дисперсия: ", sigma_squared, "\n")
cat("Средняя внутригрупповая дисперсия: ", sigma_insidegroup_squared, "\n")
cat("Межгрупповая дисперсия: ", sigma_betwines_squared, "\n")
