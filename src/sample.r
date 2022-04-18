# Generate sample of size n = 20

# f(x) = 3/x^4
# F(x) = \int_0^{x} 3/t^4 dt = -[t^{-3}]_0^{x} = - 1/x^3
# F^{-1}(y) = (-1/y)^{1/3}

theta_1 <- 3
theta_2 <- 1

number_of_samples <- 20

# cumulative density function
cdf <- function(x) {
  return (-1 / x^3)
}

# inverse of cumulative density function
inv_cdf <- function(y) {
  return (1 / ((1 - y)^(1 / 3)))
}

# generate random variables vector from the inverse cdf
generate_random_variables_vector <- function(number_of_samples, inv_cdf) {
  # generate randoms numbers from the uniform distribution U(0,1)
  data_unif <- runif(number_of_samples)

  rv_vector <- inv_cdf(y = data_unif)

  return rv_vector
}

# maximum likelihood method for gini coefficient estimator
gini_mle <- function(rv_vector) {
  number_of_samples = len(rv_vector)
  return (1 / ((2 * number_of_samples) / (sum(log(rv_vector / min(rv_vector)))) - 1))
} 

# method of moment for gini coefficient estimator
gini_mme <- function(rv_vector) {
  number_of_samples = len(rv_vector)
  return (1 / ((2 * (number_of_samples) * mean(rv_vector) - min(rv_vector) )/ (number_of_samples * (mean(rv_vector) - min(rv_vector))) - 1))
}

theoretical_gini <- function(theta_1) {
  return (1 / ((2 * theta_1) - 1))
}

rv_vector = generate_random_variables_vector(number_of_samples = number_of_samples, inv_cdf = inv_cdf)

# plot an histogram of the random variable vector
hist(rv_vector, freq=F, xlab="X", main="random sample")

# compute Gini coefficients
gini_mle(rv_vector = rv_vector)
gini_mme(rv_vector = rv_vector)

# Generate N = 1000 times the sample

number_of_iterations <- 1000

gini_mle_sample <- numeric(number_of_iterations)
gini_mme_sample <- numeric(number_of_iterations)

for (i in 1:number_of_iterations) {
  set.seed(i)
  # generate random variables
  rv_vector <- generate_random_variable(number_of_samples = number_of_samples, inv_cdf = inv_cdf)

  # compute gini coefficients
  gini_mle <- gini_mle(rv_vector = rv_vector)
  gini_mme <- gini_mme(rv_vector = rv_vector)

  gini_mle_sample[i] <- gini_mle
  gini_mme_sample[i] <- gini_mme
}

hist(gini_mle_sample, main="", xlab="Gini MLE & MME samples", col="steelblue")
hist(gini_mme_sample, col="red", add=TRUE)
legend('topright', c('MLE', 'MME'), fill=c('steelblue', 'red'))

boxplot(gini_mle_sample, col="grey"); abline(h=theoretical_gini(theta_1), col="brown", lty=2)
boxplot(gini_mme_sample, col="grey"); abline(h=theoretical_gini(theta_1), col="brown", lty=2)

# Bias, Variance and Mean Squared Error of these samples
gini_mle_sample_bias <- mean(gini_mle_sample) - theoretical_gini(theta_1)
gini_mme_sample_bias <- mean(gini_mme_sample) - theoretical_gini(theta_1)

gini_mle_sample_variance <- sd(gini_mle_sample)^2
gini_mme_sample_variance <- sd(gini_mme_sample)^2

gini_mle_sample_mse <- mean((gini_mle_sample - theoretical_gini(theta_1))^2)
gini_mme_sample_mse <- mean((gini_mme_sample - theoretical_gini(theta_1))^2)

gini_mle_sample_bias
gini_mme_sample_bias

gini_mle_sample_variance
gini_mme_sample_variance

gini_mle_mse
gini_mme_mse