# f(x) = 3/x^4
# F(x) = \int_0^{x} 3/t^4 dt = -[t^{-3}]_0^{x} = - 1/x^3
# F^{-1}(y) = (-1/y)^{1/3}

number_of_samples <- 20
U <- runif(number_of_samples)
X <- (-1/U)^(1/3)

hist(X, freq=F, xlab="X", main="random sample")
