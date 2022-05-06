# import::from(utils.r, inv_cdf, inverse_transform_sampling, gini_mle, gini_mme, gini_theoretical, bias, mse)
source("src/utils.r")

# set a seed for reproductability
set.seed(42)

# Generate sample of size n = 20 by using inverse transform sampling
rv_vector <- inverse_transform_sampling(n = 20, inv_cdf = inv_cdf)

# plot an histogram of the random variable vector
hist(rv_vector, breaks = 50, freq = FALSE, xlab = "X", main = "random sample")

# compute Gini coefficients
gini_mle(rv_vector = rv_vector, n = 20)
gini_mme(rv_vector = rv_vector, n = 20)

# Generate N = 1000 times the sample
x <- sim(N = 1000, n = 20, f = inverse_transform_sampling, inv_cdf)

gini_mle_sample <- x["gini-mle-sample", ]
gini_mme_sample <- x["gini-mme-sample", ]

# histogram of gini samples
par(mfrow = c(1, 2))
hist(gini_mle_sample, breaks = 50, main = "", xlab = "Gini MLE sample", col = "steelblue")
abline(v = gini_theoretical(theta_1), col = "green", lty = 2)
hist(gini_mme_sample, breaks = 50, main = "", xlab = "Gini MME sample", col = "red")
abline(v = gini_theoretical(theta_1), col = "green", lty = 2)
legend("topright", c("Gini MLE", "Gini MME", "True Gini"), fill = c("steelblue", "red", "green"))

# boxplot of gini samples
par(mfrow = c(1, 2))
boxplot(gini_mle_sample, col = "grey")
abline(h = gini_theoretical(theta_1), col = "brown", lty = 2)
boxplot(gini_mme_sample, col = "grey")
abline(h = gini_theoretical(theta_1), col = "brown", lty = 2)


bias_mle <- bias(sample = gini_mle_sample, theoretical = gini_theoretical(theta_1))
bias_mme <- bias(sample = gini_mme_sample, theoretical = gini_theoretical(theta_1))

variance_mle <- var(gini_mle_sample)
variance_mme <- var(gini_mme_sample)

mse_mle <- mse(sample = gini_mle_sample, theoretical = gini_theoretical(theta_1))
mse_mme <- mse(sample = gini_mme_sample, theoretical = gini_theoretical(theta_1))

print(bias_mle, variance_mle, mse_mle)
print(bias_mme, variance_mme, mse_mme)


# repeat computation for sample size n = 20, 40, 60, 80, 100, 150, 200, 300, 400, 500
sample_sizes <- c(20, 40, 60, 80, 100, 150, 200, 300, 400, 500)
statistical_quantities <- c("gini-bias-mle", "gini-bias-mme", "gini-variance-mle", "gini-variance-mme", "gini-mse-mle", "gini-mse-mme")

matrix <- matrix(NA, nrow = length(statistical_quantities), ncol = length(sample_sizes))
rownames(matrix) <- statistical_quantities
colnames(matrix) <- sample_sizes

for (n in sample_sizes) {
  x <- sim(N = 1000, n = n, f = inverse_transform_sampling, inv_cdf)

  gini_mle_sample <- x["gini-mle-sample", ]
  gini_mme_sample <- x["gini-mme-sample", ]

  bias_mle <- bias(sample = gini_mle_sample, theoretical = gini_theoretical(theta_1))
  bias_mme <- bias(sample = gini_mme_sample, theoretical = gini_theoretical(theta_1))

  variance_mle <- var(gini_mle_sample)
  variance_mme <- var(gini_mme_sample)

  mse_mle <- mse(sample = gini_mle_sample, theoretical = gini_theoretical(theta_1))
  mse_mme <- mse(sample = gini_mme_sample, theoretical = gini_theoretical(theta_1))

  matrix[, as.character(n)] <- c(bias_mle, bias_mme, variance_mle, variance_mme, mse_mle, mse_mme)
}

par(mfrow = c(1, 2))
plot(x = sample_sizes, y = matrix["gini-bias-mle", ], main = "", xlab = "N", ylab = "Gini MLE biases", col = "steelblue")
plot(x = sample_sizes, y = matrix["gini-bias-mme", ], main = "", xlab = "N", ylab = "Gini MME biases", col = "red")

par(mfrow = c(1, 2))
plot(x = sample_sizes, y = matrix["gini-variance-mle", ], main = "", xlab = "N", ylab = "Gini MLE variances", col = "steelblue")
plot(x = sample_sizes, y = matrix["gini-variance-mme", ], main = "", xlab = "N", ylab = "Gini MME variances", col = "red")

par(mfrow = c(1, 2))
plot(x = sample_sizes, y = matrix["gini-mse-mle", ], main = "", xlab = "N", ylab = "Gini MLE Mean Squared Error", col = "steelblue")
plot(x = sample_sizes, y = matrix["gini-mse-mme", ], main = "", xlab = "N", ylab = "Gini MME Mean Squared Error", col = "red")


#
sim_n20 <- sim(N = 1000, n = 20, f = inverse_transform_sampling, inv_cdf)
result_n20 <- sqrt(20) * (sim_n20["gini-mle-sample", ] - gini_theoretical(theta_1))

sim_n100 <- sim(N = 1000, n = 100, f = inverse_transform_sampling, inv_cdf)
result_n100 <- sqrt(100) * (sim_n100["gini-mle-sample", ] - gini_theoretical(theta_1))

sim_n500 <- sim(N = 1000, n = 500, f = inverse_transform_sampling, inv_cdf)
result_n500 <- sqrt(500) * (sim_n500["gini-mle-sample", ] - gini_theoretical(theta_1))

par(mfrow = c(1, 3))
hist(result_n20, breaks = 50, main = "sample size: n = 20", xlab = "$\\sqrt{n}(\\hat{G}_{\\text{MLE}} - G_{\\theta_1^0, \\theta_2^0} )$", col = "steelblue")
hist(result_n100, breaks = 50, main = "sample size: n = 100", xlab = "$\\{n}(\\hat{G}_{\\text{MLE}} - G_{\\theta_1^0, \\theta_2^0} )$", col = "steelblue")
hist(result_n500, breaks = 50, main = "sample size: n = 500", xlab = "$\\sqrt{n}(\\hat{G}_{\\text{MLE}} - G_{\\theta_1^0, \\theta_2^0} )$", col = "steelblue")