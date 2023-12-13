# Я не знаю как назывались перменные в изначальном файле, из за проблем с кодировкой. 
# Я переименовал их в x,y
R_sqare_vector <- c()
A_vector <- c()
data <- read.csv("./Data/Non_lin.csv",sep=";")
data$x <- as.numeric(gsub(",", ".", data$x))
data$y <- as.numeric(gsub(",", ".", data$y))
data
plot(data$y~data$x)

summary(lm(data$y~data$x))

summary(lm(data$y~data$x))$r.squared 

#?lm() Не вижу ошибку апроксимации

A <- (1/max(data$n)) * sum(abs( data$y - lm(data$x~data$y)$fitted.values ) / data$y)
A

R_sqare_vector <- c(R_sqare_vector,summary(lm(data$y~data$x))$r.squared)
A_vector <- c(A_vector, A)

x <- data$x
y <- data$y

x <- log(x)
y <- log(y)

summary(lm(y~x))

summary(lm(y~x))$r.squared 

A <- (1/max(data$n)) * sum(abs(y - lm(y~x)$fitted.values ) / y)
A

R_sqare_vector <- c(R_sqare_vector, summary(lm(y~x))$r.squared)
A_vector <- c(A_vector, A)

x <- data$x
y <- data$y

y <- log(y)

summary(lm(y~x))

summary(lm(y~x))$r.squared 

A <- (1/max(data$n)) * sum(abs( data$y - lm(y~x)$fitted.values ) / y)
A

R_sqare_vector <- c(R_sqare_vector, summary(lm(y~x))$r.squared)
A_vector <- c(A_vector, A)

x <- data$x
y <- data$y

x <- log(x)

summary(lm(y~x))

summary(lm(y~x))$r.squared 

A <- (1/max(data$n)) * sum(abs(y-lm(y~x)$fitted.values)/y)
A

R_sqare_vector <- c(R_sqare_vector, summary(lm(y~x))$r.squared)
A_vector <- c(A_vector, A)

x <- data$x
y <- data$y

x <- sqrt(x)

summary(lm(y~x))

summary(lm(y~x))$r.squared 

A <- (1/max(data$n)) * sum(abs( y - lm(y~x)$fitted.values ) / y)
A

R_sqare_vector <- c(R_sqare_vector, summary(lm(y~x))$r.squared)
A_vector <- c(A_vector, A)

x <- data$x
y <- data$y

x1 <- 1/x

summary(lm(y~1))

summary(lm(y~x1))$r.squared 

A <- (1/max(data$n)) * sum(abs( y - lm(y~x1)$fitted.values ) / y)
A

R_sqare_vector <- c(R_sqare_vector, summary(lm(y~x))$r.squared)
A_vector <- c(A_vector, A)

R_sqare_vector
A_vector

max(R_sqare_vector)
min(A_vector)

x <- data$x
y <- data$y

x <- log(x)
y <- log(y)

model <- lm(y ~ x)
predictions <- predict(model, interval = "confidence", level = 0.95)
plot(x, y)
abline(model, col = "blue") 
lines(x, predictions[, "lwr"], col = "red", lty = 2)
lines(x, predictions[, "upr"], col = "red", lty = 2)

x <- data$x
y <- data$y

x <- log(x)

model <- lm(y ~ x)
predictions <- predict(model, interval = "confidence", level = 0.95)
plot(x, y)
abline(model, col = "blue") 
lines(x, predictions[, "lwr"], col = "red", lty = 2)
lines(x, predictions[, "upr"], col = "red", lty = 2)


