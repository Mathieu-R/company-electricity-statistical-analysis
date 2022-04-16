# f(x) = 3/x^4
# F(x) = \int_0^{x} 3/t^4 dt = -[t^{-3}]_0^{x} = - 1/x^3
# F^{-1}(y) = (-1/y)^{1/3}

number_of_samples <- 20

# generate randoms numbers from the uniform distribution U(0,1)
U <- runif(number_of_samples)

# compute X = F^{-1}(u)
X <- 1 / (1-U)^(1/3)

X

# plot an histogram
hist(X, freq=F, xlab="X", main="random sample")

# compute Gini coefficients
G_MLE <- 1 / ((2 * number_of_samples) / (sum(log(X / min(X)))) - 1)
G_MME <- 1 / ((2 * (number_of_samples) * mean(X) - min(X) )/ (number_of_samples * (mean(X) - min(X))) - 1)

G_MLE
G_MME