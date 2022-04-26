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

x["gini-mle-sample", ]

# histogram of gini samples
par(mfrow = c(1, 2))
hist(x["gini-mle-sample", ], breaks = 50, main = "", xlab = "Gini MLE sample", col = "steelblue")
abline(h = gini_theoretical(theta_1), col = "brown", lty = 2)
hist(x["gini-mme-sample", ], breaks = 50, main = "", xlab = "Gini MME sample", col = "red")
abline(h = gini_theoretical(theta_1), col = "brown", lty = 2)
legend("topright", c("MLE", "MME"), fill = c("steelblue", "red"))

# boxplot of gini samples
par(mfrow = c(1, 2))
boxplot(x["gini_mle_sample", ], col = "grey")
abline(h = gini_theoretical(theta_1), col = "brown", lty = 2)
boxplot(x["gini_mme_sample", ], col = "grey")
abline(h = gini_theoretical(theta_1), col = "brown", lty = 2)


# repeat computation for sample size n = 20, 40, 60, 80, 100, 150, 200, 300, 400, 500
n_vector <- c(20, 40, 60, 80, 100, 150, 200, 300, 400, 500)

i <- 0
for (n in n_vector) {
  number_of_iterations <- 1000

  gini_mle_sample <- numeric(number_of_iterations)
  gini_mme_sample <- numeric(number_of_iterations)

  x <- sim(N = number_of_iterations, n = n, f = inverse_transform_sampling, inv_cdf)

  gini_mle_biases[i] <- mean(gini_mle_sample_bias)
  gini_mle_variances[i] <- mean(gini_mle_sample_variance)
  gini_mle_mses[i] <- mean(gini_mle_sample_mse)

  gini_mme_biases[i] <- mean(gini_mme_sample_bias)
  gini_mme_variances[i] <- mean(gini_mme_sample_variance)
  gini_mme_mses[i] <- mean(gini_mme_sample_mse)

  i <- i + 1
}

par(mfrow = c(1, 2))
plot(x = n_vector, y = gini_mle_biases, main = "", xlab = "Gini MLE biases", col = "steelblue")
plot(x = n_vector, y = gini_mme_biases, main = "", xlab = "Gini MME biases", col = "red")

par(mfrow = c(1, 2))
plot(x = n_vector, y = gini_mle_variances, main = "", xlab = "Gini MLE variances", col = "steelblue")
plot(x = n_vector, y = gini_mme_variances, main = "", xlab = "Gini MME variances", col = "red")

par(mfrow = c(1, 2))
plot(x = n_vector, y = gini_mle_mses, main = "", xlab = "Gini MLE Mean Squared Error", col = "steelblue")
plot(x = n_vector, y = gini_mme_mle_mses, main = "", xlab = "Gini MME Mean Squared Error", col = "red")