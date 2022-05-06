library(tidyverse)
library(e1071)

df <- read.csv2(file = "src/electricity_consumption_dataset.txt", sep = ";", dec = ".")

# X = Electricity consumption (MWh)
# Y = Productivity (1000€ / day)

# rename columns
df <- df %>%
  rename(electricity_consumption = X, productivity = Y)

# scatter plot to visualize the linear relationship
# not linear at all (should transform X => X^2 ?)

ggplot(df, aes(x = electricity_consumption, y = productivity)) +
  geom_jitter() +
  geom_smooth() +
  ggtitle("Productivity ~ Electricity consumption") +
  xlab("Electricity consumption (MWh)") +
  ylab("Productivity (1000€ / day)")

# boxplot to show outliers

par(mfrow = c(1, 2))

ggplot(df, aes(x = "", y = productivity)) +
  geom_boxplot()

ggplot(df, aes(x = "", y = electricity_consumption)) + 
  geom_boxplot()

simple_lm <- lm(productivity ~ electricity_consumption, data = df)

summary(simple_lm)

# checking normality of the residuals
ggplot(simple_lm, aes(sample = .resid)) +
  stat_qq()

# residues analysis
ggplot(simple_lm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) 
  #geom_smooth(se = FALSE)

log_lm <- lm(productivity ~ electricity_consumption + log2(electricity_consumption), data = df)

summary(log_lm)

ggplot(log_lm, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("residuals")

# checking normality of the residuals
qqnorm(log_lm$residuals)

ggplot(log_lm, aes(sample = .resid)) +
  stat_qq()

ggplot(log_lm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)

ggplot(df, aes(x = electricity_consumption, y = productivity)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + log2(x^2), se = FALSE)

# boxplot to check for any outliers
par(mfrow = c(1, 2))
boxplot(df$electricity_consumption, main = "Electricity consumption")
boxplot(log(df$productivity), main = "Productivity")

# density plot to check if the variable is close to normal distribution
par(mfrow = c(1, 2))
plot(density(df$electricity_consumption), main = "Density plot: Electricity consumption", ylab = "Frequency")
plot(density(df$productivity), main = "Density plot: Productivity", ylab = "Frequency")

# correlation
cor(df$electricity_consumption, df$productivity)