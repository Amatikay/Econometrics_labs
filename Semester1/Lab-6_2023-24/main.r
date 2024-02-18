library(scatterplot3d)
data <- read.csv("./Data/Rent.csv", sep='\t')
data

N  <- 200
epsilon <- rnorm(N,0,1)
theta <- cbind(5,8,12)
i <- 1:N
x <- 1 + .1*i  
y <- theta[,1] + theta[,2]*x + theta[,3]*x^2 + epsilon

X <- cbind(rep(1, N), x, x^2) 
# Nreg <- lm(y~x+x^2) # Почему так не работает?
Nreg <- lm(y~X[,2]+X[,3])
summary(Nreg)

scatterplot3d(X[,2], X[,3], y, main = "3D Scatter Plot", xlab = "X", ylab = "X^2", zlab = "Y")

theta_est <- solve(t(X) %*% X) %*% t(X) %*% y

rownames(theta_est) <- c("theta1", "theta2","theta3")
theta_est

S_sqare <- (1 / (N - 3)) * t(y - X %*% theta_est) %*% (y - X %*% theta_est)
S_sqare[1]

S_square_theta <- S_sqare[1] * solve(t(X) %*% X)
S_square_theta

y_estimate <- theta_est[1] + theta_est[2]*X[,2] + theta_est[3]*X[,3]
scatterplot3d(X[,2], X[,3], y_estimate, main = "3D Scatter Plot Estimated")

RSS <- sum((y-y_estimate)^2)
TSS <- sum((y-mean(y))^2)
R_2 <- 1- RSS/TSS
RSS
TSS
R_2

t <- theta_est
