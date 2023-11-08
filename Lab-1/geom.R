options(warn=-1)
setwd("/home/sergey/University/University/Эконометрика/Эконометрика_I/Lab-1")
n<-500
p <- 0.2
r_geom <- rgeom(n, prob = p)
# Переписать дисперсию и мат ожидание
# p - probability of success
# q - probability of failure
M_x_geom <- (1-p)/p
D_x_geom <- (1-p)*(1-p)/p
print("Expected value")
print(M_x_geom)
print("Dispersion")
print(D_x_geom)

#print(fitdistr,"geometric")

print(summary(r_geom))
# Выборочнео среднее (Арифметическое ожиднаие) оно выводится как один из параметров summary, но пусть будет
print("Arithmetic mean")
print(mean(r_geom))
#x11()
hist(r_geom, freq =F)
#hist(r_geom,"geometric")
#x11()
#plot(r_geom,type="l")


#полигон частот
h1<- just(r_geom)
lines(h1$counts tilda (исправь тильду) h1$mids)

x_tbl <- table(r_geom) # Таблицa частот


x_val <- as.numeric(names(x_tbl)) # получаем числовые значения таблицы частотного распределения
x_df <- data.frame(count=as.numeric(x_tbl), value=x_val) # фрейм данных по таблице частот

# расширение для заполнения пустот из за 0-ого отсчета
all_x_val <- data.frame(value = 0:max(x_val))
x_df <- merge(all_x_val, x_df, by="value", all.x=TRUE)
x_df$count[is.na(x_df$count)] <- 0  # В значения которые NA (not avalible) записать нули.

# Get theoretical probabilities
x_df$eprob <- dgeom(x_df$val, p)




print(chisq.test(x=x_df$count, p=x_df$eprob, rescale.p=TRUE))

# Вытигивание себя и коня из болта за свои же волосы. По выборке строится эмпирическое распределение, полгая что оно теоретическое - генереруется 2000 псевдовыборок. по ним уже идут суждения. Непараметрическую статистику не учил, а метод подсмотрел в документации.bootstrap evaluation
print(chisq.test(x=x_df$count, p=x_df$eprob, rescale.p=TRUE,
   simulate.p.value=TRUE)) #Вычисление p-value Методом Монте-Карло с 2000 реплик(По умолчанию)
