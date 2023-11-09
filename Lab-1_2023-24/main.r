library(moments)
library(fitdistrplus)
#Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



#Poisson distribution
lambda <- 5
poisson_sample <- rpois(200, lambda)  # Генерируем выборку с распределением Пуассона

#Sort sample
sorted_poisson_sample <- sort(poisson_sample)

#Create frequency table 
freq_table <- table(sorted_poisson_sample)
relative_frequencies <- prop.table(freq_table, margin = NULL)

#Create variation series
variation_series <- data.frame(
  Значение = as.numeric(names(freq_table)),
  Абсолютная_частота = as.numeric(freq_table),
  Относительная_частота = as.numeric(relative_frequencies)
)

cat("Ряд абсолютных и оносительных частот\n")
print(variation_series)


#Frequency Polygon
sample_hist <- hist(poisson_sample)
lines(sample_hist$counts ~ sample_hist$mids)

#Estimates of statistics
cat("Числовые характеристики\n\n")
cat("Математическое ожидание\n")
mean(poisson_sample) # Математическое ожидание
cat("Дисперсия\n")
var(poisson_sample) # Дисперсия
cat("Стандартное отклонение\n")
sd(poisson_sample) 	# Стандартное отклониение
cat("Медиана\n")
median(poisson_sample) # Медиана
cat("Квантили\n")
quantile(poisson_sample, probs = 0.5) # Квантили
cat("Мода\n")
Mode(poisson_sample) # Мода | Функция описана в начале файла

#Theoretical Mean and Var for Poisson distribution
cat("Теоретические мат ожидания и дисперсия для распределения Пуассона\n")
M_Pois_distr <- lambda
D_Pois_distr <- lambda

#MLE тут остановился. Для распределения пуассона метод максимального правдоподобия сводится к 
lambda_est <- sum(poisson_sample) / length(poisson_sample)
cat("Estimates of lambda\n")
print(lambda_est)

#Асиметрия и эксцесс нужна библиотека моментов, но она не подключается почему то

#cat("Коэффиценты асиметрии и эксцесса\n")
#skewness(poisson_sample)
#kurtosis(poisson_sample)


##########################################################################
#                                                                        #
#                                                                        #
#                         Задание №2                                     #
#                                                                        #
#                                                                        #
##########################################################################


cat("\n############\n#Задание №2#\n############\n\n")


# Параметры распределения
n <- 200
lambda <- 10

# Генерация случайных чисел из экспоненциального распределения
x_exp <- rexp(n, lambda)

# Теоретические значения среднего и дисперсии
M_x_exp <- 1/lambda
D_x_exp <- 1/(lambda^2)

# Вывод теоретических значений
cat("Математическое ожидание:\n", M_x_exp, "\n")
cat("Дисперсия:\n", D_x_exp, "\n")

# Выборочные числовые характеристики
summary_stats <- summary(x_exp)
cat("Выборочные числовые характеристики:\n")
print(summary_stats)

# Выборочное среднее (Арифметическое ожидание)
mean_value <- mean(x_exp)
cat("Выборочное среднее: ", mean_value, "\n")

# График
hist(x_exp, freq = FALSE)
x_dexp <- seq(0, 1, 0.01)
y_dexp <- dexp(x_dexp, rate = lambda)
lines(x_dexp, y_dexp)

# Оценка параметров распределения
dist_fit <- fitdist(x_exp, 'exp')
cat("Оценка параметров распределения:\n")
print(dist_fit)

# MLE 
lambda_est_mle <- sum(x_exp) / length(x_exp)
cat("MLE:\n", lambda_est_mle)

# Тесты
ks_test_result <- ks.test(x_exp, "pexp", dist_fit$estimate)$p.value
chisq_test_result <- chisq.test(x_exp, dexp(x_exp, rate = M_x_exp), rescale.p = TRUE)

cat("Тест Колмогорова Смирнова | p-value:\n", ks_test_result, "\n")
print(chisq_test_result)

