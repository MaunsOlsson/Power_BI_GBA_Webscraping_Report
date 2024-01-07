# Violin graph
library(ggplot2)
library(dplyr)

# Transforming data
dataset <- dataset[!is.na(dataset$Month),]
dataset$Month <- as.factor(dataset$Month)
dataset$Month <- recode(dataset$Month,
                        '1' = "Jan", '2' = "Feb",
                        '3' = "Mar", '4' = "Apr",
                        '5' = "May", '6' = "Jun",
                        '7' = "Jul", '8' = "Aug",
                        '9' = "Sep", '10' = "Oct",
                        '11' = "Nov", '12' = "Dec")



# Plotting
ggplot(data = dataset, aes(x= Month, y=`Mean score`, fill = Month)) + 
  geom_violin() + stat_summary(fun.y=mean, geom="point", shape=10, size=4, col = "black") + ylab("Score") +
  theme_minimal() + scale_y_continuous(limits = c(2, 9.5), breaks = seq(2, 9.5, 1.5),
                                       minor_breaks = seq(2, 9.5, 0.5)) +
  theme(legend.position="none") +
  scale_fill_manual(values=c('Jan' = "#bc2a95",
                             'Feb' = "#8d2095",
                             'Mar' = "#5d1793",
                             'Apr' = "#1652ca",
                             'May' = "#4db1c0",
                             'Jun' = "#409629",
                             'Jul' = "#7fc73d",
                             'Aug' = "#fdff4d",
                             'Sep' = "#fcca4f",
                             'Oct' = "#f39d38",
                             'Nov' = "#ef6f2e",
                             'Dec' = "#ec3223"))