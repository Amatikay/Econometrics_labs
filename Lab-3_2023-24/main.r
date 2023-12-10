N <- 200
X1 <- rnorm(N,0,1)

r_1 <- .15
r_2 <- .8

X2 <- r_1 * X1 + sqrt(1 - r_1^2) * rnorm(N,0,1)
X3 <- r_2 * X1 + sqrt(1 - r_2^2) * rnorm(N,0,1)

plot(X1,X2,type='p', main='X1:X2')
plot(X1,X3,type='p', main='X1:X3')
plot(X2,X3,type='p', main='X1:X3')

r_1_estimate <- cor(X1,X2)
r_2_estimate <- cor(X1,X3)
r_3_estimate <- cor(X2,X3)
r_1_estimate
r_2_estimate
r_3_estimate

t_statistic_1 <- cor.test(X1,X2)$estimate
t_statistic_2 <- cor.test(X1,X3)$estimate
t_statistic_3 <- cor.test(X2,X2)$estimate
t_statistic_1
t_statistic_2
t_statistic_3

z_25 <- qnorm(0.975)
Z_Fisher_transform_1 <- 0.5 * log((1 + t_statistic_1) / (1 - t_statistic_1))

left_bound_1 <- tanh(Z_Fisher_transform_1 - z_25 / sqrt(N - 3))
right_bound_1 <- tanh(Z_Fisher_transform_1 + z_25 / sqrt(N - 3))

Z_Fisher_transform_2 <- 0.5 * log((1 + t_statistic_2) / (1 - t_statistic_2))

left_bound_2 <- tanh(Z_Fisher_transform_2 - z_25 / sqrt(N - 3))
right_bound_2 <- tanh(Z_Fisher_transform_2 + z_25 / sqrt(N - 3))

Z_Fisher_transform_3 <- 0.5 * log((1 + t_statistic_3) / (1 - t_statistic_3))

left_bound_3 <- tanh(Z_Fisher_transform_3 - z_25 / sqrt(N - 3))
right_bound_3 <- tanh(Z_Fisher_transform_3 + z_25 / sqrt(N - 3))

cat(left_bound_1, ' ', right_bound_1, '\n')
cat(left_bound_2, ' ', right_bound_2, '\n')
cat(left_bound_3, ' ', right_bound_3, '\n')

spearman_estimate_1 <- cor.test(X1,X2,method = "spearman")$estimate
spearman_estimate_2 <- cor.test(X1,X3,method = "spearman")$estimate
spearman_estimate_3 <- cor.test(X2,X3,method = "spearman")$estimate

spearman_estimate_1
spearman_estimate_2
spearman_estimate_3

cor.test(X1,X2,method = "spearman")
cor.test(X1,X3,method = "spearman")
cor.test(X2,X3,method = "spearman")

n <- 50
p <- .2
q1 <- .5
q2 <- .3

n_matrix <- matrix(0, nrow = 2, ncol = 2)
rownames(n_matrix) <- c("A", "NOT A")
colnames(n_matrix) <- c("B", "NOT B")

n1 <- rbinom(1, n, p)
n2 <- n - n1

n_matrix[1, 1] <- rbinom(1, n1, q1)
n_matrix[1, 2] <- n1 - n_matrix[1, 1]
n_matrix[2, 1] <- rbinom(1, n2, q2)
n_matrix[2, 2] <- n2 - n_matrix[2, 1]
n_matrix

library(rcompanion)

chisq.test(n_matrix)

cramerV(n_matrix)

cramerV(n_matrix, ci=TRUE)

data <- read.csv("./Data/RentR.csv", sep=';')
data$s <- as.numeric(gsub(",", ".", data$s))

data

plot(data$rent, data$s , type='p')


cor.test(data$rent, data$s)

cor(data[ ,-1])

cor( data[, c("rent", "s", "rooms")])

cor.test(data$rooms , data$s)

chisq.test(data$walls, data$district)

if ( chisq.test(data$walls, data$district)$p.value < 0.05) { cat("Есть корреляция!") }
