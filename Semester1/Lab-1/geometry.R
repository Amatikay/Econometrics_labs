setwd("/home/sergey/University/University/Эконометрика/Эконометрика_I/Lab-1")
# The rgeom function generates a list of random values that represent the number of failures before the first success

n<-200
p <- 0.2
p_geom <- rgeom(n, prob = p)
# Переписать дисперсию и мат ожидание
# p - probability of success
# q - probability of failure
M_x_geom <- (1-p)/p
D_x_geom <- (1-p)*(1-p)/p
print("Expected value")
print( M_x_geom)
print("Dispersion")
print(D_x_geom)


# Выборочные числоывые характеристики
print(summary(p_geom))
# Выборочнео среднее (Арифметическое ожиднаие) оно выводится как один из параметров summary, но пусть будет
print("Arithmetic mean")
print(mean(p_geom))
x11()
hist(p_geom, freq =F)
x11()
plot(p_geom,type="l")

# Оцка параметров
print(fitdist(p_geom,'geom'))

xnc=hist(p_geom,plot=FALSE)$counts
xnb=hist(p_geom,plot=FALSE)$breaks
k=length(xnc)
xnb[1]=0;xnb[k+1]=Inf
pnth=pgeom(xnb,mean(p_geom),sd(p_geom))
thfr=pnth[2:(k+1)]-pnth[1:k]

# выборка не проходит тест колмогорова-смирнова. Я впринципе не до конца понимаю какой тест нужно использовать для геометрического, тк это распределение количества нейдачных до удачного опыта.. я запуталься:(
print(ks.test(p_geom , "pgeom", fitdist(p_geom,'geom')$estimate))

# Хи квадрат. С теоретическими данными.

