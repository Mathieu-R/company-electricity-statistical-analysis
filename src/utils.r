theta_1 <- 3
theta_2 <- 1

# cumulative density function
cdf <- function(x) {
  (-1 / x^3)
}

# inverse of cumulative density function
inv_cdf <- function(y) {
  (1 / ((1 - y)^(1 / 3)))
}

# generate random variables vector from the inverse cdf
inverse_transform_sampling <- function(n, inv_cdf) {
  # generate randoms numbers from the uniform distribution U(0,1)
  data_unif <- runif(n)
  rv_vector <- inv_cdf(y = data_unif)
}

# maximum likelihood method for gini coefficient estimator
gini_mle <- function(rv_vector, n) {
  return(1 / ((2 * n) / (sum(log(rv_vector / min(rv_vector)))) - 1))
}

# method of moment for gini coefficient estimator
gini_mme <- function(rv_vector, n) {
  return(1 / ((2 * (n) * mean(rv_vector) - min(rv_vector)) / (n * (mean(rv_vector) - min(rv_vector))) - 1))
}

gini_theoretical <- function(theta_1) {
  return(1 / ((2 * theta_1) - 1))
}

bias <- function(sample, theoretical) {
  mean(sample) - theoretical
}

mse <- function(sample, theoretical) {
  mean((sample - theoretical)^2)
}

# N: simulation size (i.e. number of samples)
# n: sample size
# f: function to generate random variables
# ... any other parameters given to f
sim <- function(N = 1000, n = 20, f, ...) {
  # compute a matrix of random variables based on the distribution f
  # each column correspond to one simulation
  x <- matrix(f(N * n, ...), nrow = n)

  gini_mle_estimators <- numeric(N)
  gini_mme_estimators <- numeric(N)

  for (i in 1:N) {
    gini_mle_estimators[i] <- gini_mle(rv_vector = x[, i], n = n)
    gini_mme_estimators[i] <- gini_mme(rv_vector = x[, i], n = n)
  }

  print(gini_mle_estimators)

  stats <- apply(
    X = x,
    MARGIN = 2,
    FUN = function(y) {
      c(
        mean(y),
        gini_mle_estimators,
        gini_mme_estimators,
        bias(sample = gini_mle_estimators, theoretical = gini_theoretical(theta_1)),
        bias(sample = gini_mme_estimators, theoretical = gini_theoretical(theta_1)),
        var(gini_mle_estimators),
        var(gini_mme_estimators),
        mse(sample = gini_mle_estimators, theoretical = gini_theoretical(theta_1)),
        mse(sample = gini_mme_estimators, theoretical = gini_theoretical(theta_1))
      )
    }
  )

  rownames(stats) <- c("sample-mean", "gini-mle-sample", "gini-mme-sample", "gini-mle-bias", "gini-mme-bias", "gini-mle-variance", "gini-mme-variance", "gini-mle-mse", "gini-mme-mse")
  return(stats)
}