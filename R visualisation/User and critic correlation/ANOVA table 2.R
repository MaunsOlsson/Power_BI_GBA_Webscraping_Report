# Parameters table
library(gridExtra)

model <- lm(data = dataset, formula = `User score` ~ `Critics Score`)
Output <- anova(model)
Output[, -5] <- round(Output[, -5], 3)
Output[2, 4:5] <- ""
g <- grid.table(Output, theme = ttheme_minimal())
print(g)