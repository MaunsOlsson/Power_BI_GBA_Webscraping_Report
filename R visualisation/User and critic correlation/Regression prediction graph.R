# Predict regression graph
library(ggplot2)
library(ggthemes)

dataset <- dataset[!is.na(dataset$`User score`), ]

# Predict intervall, one could easily do this by predict(model, interval = "predict", level = ...), however this is basically manually done
X <- dataset$`Critics Score`
Y <- dataset$`User score`

X <- cbind(1, matrix(data = X))
Y <- matrix(data = Y)
B <- solve(t(X) %*% X) %*% t(X) %*% Y
B

SSE <- as.numeric(t(Y - X %*% B) %*% (Y - X %*% B))
MSE <- SSE/318

SB2 <- solve(t(X) %*% X) * MSE

newdata <- data.frame(intercept = 1, var2 = 50)
colnames(newdata)[2] <- colnames(dataset)[2]
X0 <- matrix(data = c(1, 50), nrow = 2)
Y0 <- t(X0) %*% B

S2Y <- t(X0) %*% SB2 %*% X0
S2pred <- MSE + S2Y

# This is bad code and will slow down the dashboard.
# This is caused by the equation both being saved and calculated for every value. To speed up the code
# one would need to remove all these rows... but hey, it works so don't fix what isn't broken yet
dataset$over <- dataset$Prediction + qt(p = 1 - (dataset$Alpha/100)/2, df = nrow(X) - 2) * as.numeric(sqrt(S2pred))
dataset$under <- dataset$Prediction - qt(p = 1 - (dataset$Alpha/100)/2, df = nrow(X)- 2) * as.numeric(sqrt(S2pred))

# visualisation
ggplot(data = dataset, mapping = aes(x = `Critics Score`, y = `User score`)) +
  geom_point(aes(col = "Observation")) +
  theme_bw() +
  scale_x_continuous(limits = c(20, 100)) +
  scale_y_continuous(limits = c(2, 10)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(col = "Regression line")) +
  geom_point(mapping = aes(x = X0[1], y = Prediction[1], col = "Prediction"), size = 4) +
  geom_point(mapping = aes(x = X0[1], y = over[1], col = "Upper prediction \nintervall"), shape = 15, size = 4) +
  geom_point(mapping = aes(x = X0[1], y = under[1], col = "Lower prediction \nintervall"), shape = 15, size = 4) +
  scale_color_manual(name = "Explanation", values=c('Observation' = "darkblue",
                                                    'Regression line' = "darkred",
                                                    'Prediction' = 'orange',
                                                    'Upper prediction \nintervall' = 'darkgreen',
                                                    'Lower prediction \nintervall' = 'lightgreen')) +
  ylab("User Score") + xlab("Critic Score")