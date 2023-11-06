setwd("/home/sergey/University/University/Эконометрика/Эконометрика_I/Lab-1")
options(warn=-1)#Так делать ну вообще не надо, но отчаные вермена требут отчанных мер. Отключение предупреждений.


# exponential distribution
library(fitdistrplus)
n<-200
lambda <- 10
x_exp <- rexp(n, lambda)# генерация псевдослычаных числе экспонециально распределенных
M_x_exp <- (1/lambda)
D_x_exp <- (1/(lambda*lambda))
print("Expected value")
print( M_x_exp)
print("Dispersion")
print(D_x_exp)


# Выборочные числоывые характеристики
print(summary(x_exp))
# Выборочнео среднее (Арифметическое ожиднаие) оно выводится как один из параметров summary, но пусть будет
print("Arithmetic mean")
print(mean(x_exp))

# График
#x11() # or X11() - запуск графического окна. Тк у меня всего один график - окно создастся автоматически.
hist(x_exp, freq=F)
x_dexp=seq(0, 1, 0.01)
y_dexp=dexp(x_dexp,rate = lambda)
#x11()
lines(x_dexp,y_dexp)

# Оцка параметров
print(fitdist(x_exp,'exp'))


# Tests
# p-value > 0.05 => distribution not refused
print('Kolmogarov-Smirnov test | p.value:')

print(ks.test(x_exp , "pexp", fitdist(x_exp,'exp')$estimate)$p.value)
print(chisq.test(x_exp , dexp(x_exp , rate=M_x_exp) ,rescale.p = TRUE ))

