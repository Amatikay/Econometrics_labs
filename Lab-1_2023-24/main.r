#library(moments)
#Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



#Poisson distribution
poisson_sample <- rpois(200, 5)  # Генерируем выборку с распределением Пуассона

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

cat("Ряд абсолютн и оносительных частот\n")
print(variation_series)


#Frequency Polygon
sample_hist <- hist(poisson_sample)
lines(sample_hist$counts ~ sample_hist$mids)

#Estimates of statistics
cat("Числовые характеристики\n")
mean(poisson_sample) # Математическое ожидание
var(poisson_sample) # Дисперсия
sd(poisson_sample) 	# Стандартное отклониение
median(poisson_sample) # Медиана
quantile(poisson_sample, probs = 0.5) # Квантили
Mode(poisson_sample)

#Асиметрия и эксцесс нужна библиотека моментов, но она не подключается почему то
#skewness(poisson_sample)
#kurtosis(poisson_sample)
