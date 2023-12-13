n <- 200
a <- 2
b <- 3
epsilon <- rnorm(n, 0, 1)

x <- rnorm(n, 0, .8)
y <- a + b * x + epsilon
plot(y~x)
summary(lm(y ~ x))
abline(lm(y ~ x))


b_est <- sum(x,y)/sum(x^2)
a_est <- mean(y) - b_est*mean(x)
a_est
b_est
y_est <- a_est+b_est*x

RSS <- sum((y-y_est)^2)
TSS <- sum((y-mean(y))^2)
R_2 <- 1- RSS/TSS
RSS
TSS
R_2

F <- (R_2/(1-R_2))*(n-2)
F
qf(1 - .05, 1, n-2) #Крит значение статистики

S2 <- sum((y - y_est)^2) / (n - 2)
SE <- sqrt(S2)
S2_b <- S2 / sum((x - mean(x))^2)
SE_b <- sqrt(S2_b)
S2_a <- S2 * (mean(x^2)) / sum((x - mean(x))^2)
SE_a <- sqrt(S2_a)
t_value_a <- a_est / SE_a
t_value_b <- b_est / SE_b
t_value_a
t_value_b
qt(1 - .05/2, df = n - 2) #квантиль t распределения

epsilon <- rlnorm(n, 0, .8)
i <- 1:n

x <- 1+0.1*i
y <- a*(x^b)*epsilon
x <-log(x)
y <-log(y)
plot(y~x)
plot(exp(y)~exp(x))

plot(y~x)
reg <- lm(y~x)
summary(reg)
abline(reg)

exp(reg$coefficients)

b_est <- sum(x,y)/sum(x^2)
a_est <- mean(y) - b_est*mean(x)
a_est
b_est

y_est <- a_est+b_est*x
RSS <- sum((y-y_est)^2)
TSS <- sum((y-mean(y))^2)
R_2 <- 1- RSS/TSS
RSS
TSS
R_2

F <- (R_2/(1-R_2))*(n-2)
F
qf(1 - .05, 1, n-2) #Крит значение статистики

S2 <- sum((y - y_est)^2) / (n - 2)
SE <- sqrt(S2)
S2_b <- S2 / sum((x - mean(x))^2)
SE_b <- sqrt(S2_b)
S2_a <- S2 * (mean(x^2)) / sum((x - mean(x))^2)
SE_a <- sqrt(S2_a)
t_value_a <- a_est / SE_a
t_value_b <- b_est / SE_b
t_value_a
t_value_b
qt(1 - .05/2, df = n - 2) #квантиль t распределения

epsilon <- rlnorm(n, 0, .8)
i <- 1:n

x <- 1 + .04 * i
y <- a * exp(b * x) * epsilon

y <- log(y)

plot(y ~ x)
plot(exp(y) ~ x)

plot(y~x)
reg <- lm(y~x)
summary(reg)
abline(reg)

exp(reg$coefficients[1])

b_est <- sum(x,y)/sum(x^2)
a_est <- mean(y) - b_est*mean(x)
a_est
b_est

y_est <- a_est+b_est*x
RSS <- sum((y-y_est)^2)
TSS <- sum((y-mean(y))^2)
R_2 <- 1- RSS/TSS
RSS
TSS
R_2

F <- (R_2/(1-R_2))*(n-2)
F
qf(1 - .05, 1, n-2) #Крит значение статистики

S2 <- sum((y - y_est)^2) / (n - 2)
SE <- sqrt(S2)
S2_b <- S2 / sum((x - mean(x))^2)
SE_b <- sqrt(S2_b)
S2_a <- S2 * (mean(x^2)) / sum((x - mean(x))^2)
SE_a <- sqrt(S2_a)
t_value_a <- a_est / SE_a
t_value_b <- b_est / SE_b
t_value_a
t_value_b
qt(1 - .05/2, df = n - 2) #квантиль t распределения

epsilon <- rnorm(n, 0, .8)
i <- 1:n

x <- 1 + .1 * i
y <- a + b*log(x) + epsilon
x <- log(x)

plot(y ~ x)
plot(y ~ exp(x))

plot(y~x)
reg <- lm(y~x)
summary(reg)
abline(reg)

a <- .8
b <- 20

x <- 1 + .1 * i

y <- a + b/x + epsilon

x1 <- 1/x
# Использую новую перменную,не полчается переопределить изначальную. 
# Из за формата хранения числа в компьютере?
plot(y ~ x)
plot(y ~ x1)

plot(y~x1)
reg <- lm(y~x1)
summary(reg)
abline(reg)


