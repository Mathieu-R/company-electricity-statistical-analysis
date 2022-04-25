import::from(utils.r, inv_cdf, inverse_transform_sampling, gini_mle, gini_mme, gini_theoretical, bias, mse)

# Generate sample of size n = 20

theta_1 <- 3
theta_2 <- 1

# set a seed for reproductability
set.seed(42)

rv_vector <- inverse_transform_sampling(n = n, inv_cdf = inv_cdf)

# plot an histogram of the random variable vector
hist(rv_vector, freq = F, xlab = "X", main = "random sample")

# compute Gini coefficients
gini_mle(rv_vector = rv_vector)
gini_mme(rv_vector = rv_vector)

# Generate N = 1000 times the sample

# N: simulation size (i.e. number of samples)
# n: sample size
# f: function to generate random variables
# ... any other parameters given to f
sim <- function(N = 1000, n = 20, f, ...) {
  # compute a matrix of random variables based on the distributin f
  x <- matrix(f(N * n, ...), nrow = n)

  gini_mle_estimator <- gini_mle(rv_vector = x, n = n)
  gini_mme_estimator <- gini_mme(rv_vector = x, n = n)

  stats <- apply(
    X = x,
    MARGIN = 2,
    FUN = function(y) {
      c(
        mean(y), 
        gini_mle_estimator,
        gini_mme_estimator,
        bias(sample = gini_mle_estimator, theoretical = gini_theoretical(theta_1)),
        bias(sample = gini_mme_estimator, theoretical = gini_theoretical(theta_1)),
        var(gini_mle_estimator),
        var(gini_mme_estimator),
        mse(sample = gini_mle_estimator, theoretical = gini_theoretical(theta_1)),
        mse(sample = gini_mme_estimator, theoretical = gini_theoretical(theta_1)),
      )
    }
  )

  rownames(stats) <- c("sample-mean", "gini-mle-sample", "gini-mme-sample", "gini-mle-bias-", "gini-mme-bias", "gini-mle-variance", "gini-mme-variance", "gini-mle-mse", "gini-mme-mse")
  return(stats)
}

x <- sim(N = 1000, n = 20, f = inverse_transform_sampling, inv_cdf)

# histogram of gini samples

par(mfrow=c(1, 2))
hist(x["gini-mle-sample"], main = "", xlab = "Gini MLE sample", col = "steelblue")
hist(x["gini-mme-sample"], main = "", xlab = "Gini MME sample", col = "red")

legend('topright', c('MLE', 'MME'), fill=c('steelblue', 'red'))

# boxplot of gini samples

par(mfrow = c(1, 2))
boxplot(gini_mle_sample, col = "grey")
abline(h = theoretical_gini(theta_1), col = "brown", lty = 2)
boxplot(gini_mme_sample, col = "grey")
abline(h = theoretical_gini(theta_1), col = "brown", lty = 2)


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
plot(x = n_vector, y = gini_mle_variances, main = "", xlab = "Gini MLE biases", col = "steelblue")
plot(x = n_vector, y = gini_mme_varigini_mle_variances, main = "", xlab = "Gini MME biases", col = "red")

par(mfrow = c(1, 2))
plot(x = n_vector, y = gini_mle_mses, main = "", xlab = "Gini MLE biases", col = "steelblue")
plot(x = n_vector, y = gini_mme_mle_mses, main = "", xlab = "Gini MME biases", col = "red")