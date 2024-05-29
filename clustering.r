# Load necessary libraries
library(dplyr)
library(cluster)
library(factoextra)

# Load the data
data <- read.csv("insurance.csv")

# Convert categorical variables to factors
data$sex <- as.factor(data$sex)
data$smoker <- as.factor(data$smoker)
data$region <- as.factor(data$region)

data
# Create binary codes for the "sex" variable
data$sex <- ifelse(data$sex == "male", 1, 0)

# Create binary codes for the "smoker" variable
data$smoker <- ifelse(data$smoker == "yes", 1, 0)

# Create binary codes for the "region" variable
data$region <- ifelse(data$region == "northeast", 1, 
                      ifelse(data$region == "northwest", 2, 
                             ifelse(data$region == "southeast", 3, 4)))


corr <- cor(data)



# Create a heatmap of correlation between all numeric variables
library(corrplot)

#corr <- cor(insurance[, c("age", "bmi", "children)

corrplot(corr, method = "color", type = "lower", tl.col = "black", tl.srt = 45)


library(ggplot2)
library(reshape2)

# Convert correlation matrix to long format
df_cor <- melt(cor(insurance), varnames = c("bmi","smoker","age","region","children","sex", "charges"), value.name = "correlation")

# Create heatmap
ggplot(insurance, aes(x = data, y = data$charges, fill = cor(data))) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Correlation Heatmap") +
  xlab("Features") +
  ylab("Features") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5, title.position = "top", title.vjust = -1))



# Select the features to be used for clustering
features <- c("age", "sex", "bmi", "children", "smoker", "region", "charges")
X <- data[features]

# Standardize the data and convert to data frame
X_df <- as.data.frame(scale(X))
X_df
# Perform clustering using k-means algorithm
set.seed(42)
kmeans_model <- kmeans(X_df, centers = 3)

# Add cluster labels to data frame
X_df$cluster <- kmeans_model$cluster



# Plot the clusters using clusplot
clusplot(X_df, kmeans_model$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)





library(ggplot2)

ggplot(X_df, aes(x=bmi, y=charges, color=factor(cluster))) +
  geom_point() +
  xlab("BMI") +
  ylab("Charges") +
  ggtitle("Insurance Data Clustering")

# Create a PCA plot
library(cluster)
clusplot(X_df[,1:7], kmeans_model$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

#WITHOUT LABELS
clusplot(X_df[,1:7], kmeans_model$cluster, color = TRUE, shade = TRUE, lines = 0)



# Create a PCA plot using factoextra
library(factoextra)
pca <- prcomp(X_df[,1:7])
fviz_eig(pca)
fviz_pca_ind(pca, geom.ind = "point", col.ind = kmeans_model$cluster, pointsize = 2)



#animation
library(animation)
set.seed(2345)
km <- kmeans.ani(X_df,4)



#silhuotte
kResult <- pam(X_df,2,metric="euclidean",cluster.only = FALSE)
summary(kResult)
spam <- silhouette(kResult)
plot(spam)




#decison tree
library(rpart)
library(rpart.plot)
library(caret)
