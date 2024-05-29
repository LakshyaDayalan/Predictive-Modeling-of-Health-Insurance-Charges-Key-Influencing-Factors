data<-read.csv("insurance.csv")
# Create dummy variables 

smoker_dummy <- ifelse(data$smoker == "yes", 1, 0) 
sex_dummy <- ifelse(data$sex == "male", 1, 0) 
region_dummy <- factor(data$region, levels = c("southwest", "southeast", "northwest", "northeast")) 

unique(data)
data <- cbind(data, model.matrix(~ smoker_dummy + sex_dummy + region_dummy - 1, data))

head(data,3)

library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

colnames(data)
str(data)
corr <- cor(data[c("age", "bmi", "children", "charges","sex_dummy", "smoker_dummy","region_dummysouthwest", "region_dummysoutheast","region_dummynorthwest",
"region_dummynortheast")])

plot(corr)



# Create a heatmap of correlation between all numeric variables
library(corrplot)



corr <- cor(data[, c("age", "bmi", "children", "charges","sex_dummy", "smoker_dummy","region_dummysouthwest", "region_dummysoutheast","region_dummynorthwest",
                           "region_dummynortheast")])
corrplot(corr, method = "color", type = "lower", tl.col = "black", tl.srt = 45)
