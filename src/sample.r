# Generate sample of size n = 20

# f(x) = 3/x^4
# F(x) = \int_0^{x} 3/t^4 dt = -[t^{-3}]_0^{x} = - 1/x^3
# F^{-1}(y) = (-1/y)^{1/3}

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

  rv_vector <- inv_cdf(data_unif)

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
  # generate random variables
  rv_vector <- generate_random_variable(number_of_samples = number_of_samples, inv_cdf = inv_cdf)

  # compute gini coefficients
  gini_mle <- gini_mle(rv_vector = rv_vector)
  gini_mme <- gini_mme(rv_vector = rv_vector)

  G_MLE_array[i] <- gini_mle
  G_MME_array[i] <- gini_mme
}

hist(gini_mle_sample, main="", xlab="Gini MLE & MME samples", col="steelblue")
hist(gini_mme_sample, col="red", add=TRUE)
legend('topright', c('MLE', 'MME'), fill=c('steelblue', 'red'))

boxplot(gini_mle_sample)
boxplot(gini_mme_sample)

# Bias, Variance and Mean Squared Error of these samples
gini_mle_sample_variance <- sd(gini_mle_sample)^2
gini_mme_sample_variance <- sd(gini_mme_sample)^2