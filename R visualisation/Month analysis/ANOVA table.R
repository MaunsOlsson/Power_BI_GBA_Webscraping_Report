# ANOVA table
library(gridExtra)
dataset <- dataset[!is.na(dataset$Month),]
dataset$Month <- as.factor(dataset$Month)

if(length(unique(dataset$Month)) != 1){
  Model <- aov(data = dataset, formula = `Mean score` ~ Month)
  Output <- anova(Model)
  Output[2, 4:5] <- ''
  row.names(Output) <- c("Between", "Within")
  g <- grid.table(Output, theme = ttheme_minimal())
  print(g)
} else {
  Output <- data.frame(ERROR = "There is only one level examined. \nClick on more months to get a ANOVA table")
  row.names(Output) <- ""
  g <- grid.table(Output, theme = ttheme_minimal())
  print(g)
}
