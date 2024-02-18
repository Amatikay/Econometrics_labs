library(scatterplot3d)
data <- read.csv("./Data/Rent.csv", sep='\t')
data

N  <- 20
epsilon <- rnorm(N,0,1)
tetha <- cbind(5,8,12)
i <- 1:N
x <- 1 + .1*i  
y <- tetha[,1] + tetha[,2]*x + tetha[,3]*x^2 + epsilon

X <- cbind(rep(1, N), x, x^2) 
# Nreg <- lm(y~x+x^2) # Почему так не работает?
Nreg <- lm(y~X[,2]+X[,3])
summary(Nreg)

scatterplot3d(X[,2], X[,3], y, main = "3D Scatter Plot", xlab = "X", ylab = "X^2", zlab = "Y")

tetha_est <- solve(t(X) %*% X) %*% t(X) %*% y

rownames(tetha_est) <- c("tetha1", "tetha2","tetha3")
tetha_est

S_sqare <- (1 / (N - 3)) * t(y - X %*% tetha_est) %*% (y - X %*% tetha_est)
S_sqare[1]

S_square_theta <- S_sqare[1] * solve(t(X) %*% X)
S_square_theta

y_estimate <- tetha_est[1] + tetha_est[2]*X[,2]+tetha_est[3]+X[,3]
scatterplot3d(X[,2], X[,3], y_estimate, main = "3D Scatter Plot Estimated")

RSS <- sum((y-y_estimate)^2)
TSS <- sum((y-mean(y))^2)
R_2 <- 1- RSS/TSS
RSS
TSS
R_2


