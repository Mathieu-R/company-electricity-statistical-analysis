install.packages("e1071", dependencies=TRUE)
library(e1071)

df <- read.csv2(file="src/electricity_consumption_dataset.txt", sep=";", dec=".")

# scatter plot to visualize the linear relationship
# not linear at all (should transform X => X^2 ?)

#pdf("src/figures/scatter_plot.pdf")

scatter.smooth(x=df$X, y=df$Y, xlab="Electricity consumption (MWh)", ylab="Productivity (1000€ / day)", main="Electricity consumption ~ Productivity")

#dev.off()

# boxplot to check for any outliers
par(mfrow=c(1, 2))
boxplot(df$X, main="Electricity consumption")
boxplot(log(df$Y), main="Productivity")

# density plot to check if the variable is close to normal distribution
par(mfrow=c(1, 2))
plot(density(df$X), main="Density plot: Electricity consumption", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df$X))))
plot(density(df$Y), main="Density plot: Productivity", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df$Y))))

# correlation 
cor(df$X, df$Y)

lm(X~Y, data=df)