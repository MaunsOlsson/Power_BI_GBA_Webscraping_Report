# Assumption regression graph
library(ggplot2)
library(ggthemes)
library(gridExtra)

model <- lm(data = dataset, formula = `User score` ~ `Critics Score`)

res <- model$residuals
fit <- model$fitted.values
ord <- 1:length(fit)

p1 <- ggplot(mapping = aes(x = fit, y = res)) +
  geom_point(col = "darkblue") + theme_bw() +
  xlab("Estimation") + ylab("Residual") +
  scale_y_continuous(limits = c(-4, 3), breaks = seq(-4, 3, 1)) +
  scale_x_continuous(limits = c(4, 9), breaks = seq(4, 9, 1)) +
  labs(caption = "Figure 1: Residual on fitted value")

p2 <- ggplot(mapping = aes(x = ord, y = res)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  xlab("Order") + ylab("Residual") +
  scale_y_continuous(limits = c(-4, 3), breaks = seq(-4, 3, 1)) +
  scale_x_continuous(limits = c(0, 320), breaks = seq(0, 320, 40)) +
  labs(caption = "Figure 2: Residual by order") +
  theme_bw()

p3 <- ggplot(mapping = aes(sample = model$residuals)) + 
  geom_qq_line(col = "red", size = 1) +
  geom_qq(col = "darkblue", size = 2) + 
  theme_bw() + labs(caption = "Figure 3: QQ-plot") +
  xlab("Theoretical") + ylab("Residual")


p4 <- ggplot(mapping = aes(x = res)) +
  geom_histogram(col = "black", fill = "lightblue") +
  theme_bw() +
  scale_x_continuous(limits = c(-4, 3), breaks = seq(-4, 3, 1)) +
  labs(caption = "Figure 4: Residual on histogram") +
  xlab("Residual") + ylab("Count")

grid.arrange(p1, p2, p3, p4)