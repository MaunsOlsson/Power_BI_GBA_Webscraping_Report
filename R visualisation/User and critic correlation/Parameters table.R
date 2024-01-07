# Parameters table
library(gridExtra)

model <- lm(data = dataset, formula = `User score` ~ `Critics Score`)
sumry <- summary(model)
Output <- sumry$coefficients
rownames(Output) <- c("B0", "B1")
Output[ ,-4] <- round(Output[ ,-4], 3)

g <- grid.table(Output, theme = ttheme_minimal())
print(g)
