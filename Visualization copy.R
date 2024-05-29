# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

#Reading the data
data <- read.csv("insurance.csv")
# Check the dimensions of the dataset
dim(data1)

#structure of the data
str(data1)

# View the first few rows of the dataset
head(data,5)


# Check for missing values
sum(is.na(data1))


# Generate summary statistics for the numeric variables
summary(data1 %>% select(age, bmi, children, charges))

# Correlation Matrix between numerical variables
cor(data1[c("age", "bmi", "children", "charges")])


#code from H

# Generate frequency tables for the categorical variables
table(data1$sex)
table(data1$smoker)
table(data1$region)

# Proportion of categorical variables
#plot into pie for sex
#smoker - bar or better
data1 %>%
  group_by(sex) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count))

data1 %>%
  group_by(smoker) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count))

data1 %>%
  group_by(region) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count))

#plot individual plots and then couple the factors

ggplot(insurance, aes(x = age)) + 
  geom_histogram(binwidth = 2, fill = "#69b3a2", color = "black") + 
  ggtitle("Distribution of Age")

ggplot(insurance, aes(x = bmi)) + 
  geom_histogram(binwidth = 2, fill = "#69b3a2", color = "black") + 
  ggtitle("Distribution of BMI")

ggplot(insurance, aes(x = children)) + 
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") + 
  ggtitle("Distribution of Children")

ggplot(insurance, aes(x = charges)) + 
  geom_histogram(binwidth = 5000, fill = "#69b3a2", color = "black") + 
  ggtitle("Distribution of Charges")


# Check the relationship between age and charges using a scatter plot
ggplot(insurance, aes(x = age, y = charges)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "#404080") +
  ggtitle("Relationship between Age and Charges")

# Check the relationship between bmi and charges using a scatter plot
ggplot(insurance, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "#404080") +
  ggtitle("Relationship between BMI and Charges")

# Check the relationship between smoker and charges using a violin plot
ggplot(insurance, aes(x = smoker, y = charges, fill = smoker)) +
  geom_violin(alpha = 0.5) +
  ggtitle("Relationship between Smoker and Charges")

# Check the relationship between region and charges using a box plot
ggplot(insurance, aes(x = region, y = charges, fill = region)) +
  geom_boxplot(alpha = 0.5) +
  ggtitle("Relationship between Region and Charges")

#Quantisation of Categorical factors 

## Convert sex and smoker columns to factors
data$sex <- as.factor(data$sex)
data$smoker <- as.factor(data$smoker)

## Convert factors to numeric values
data$sex <- as.numeric(data$sex) - 1
data$smoker <- as.numeric(data$smoker) - 1
data$region <- as.factor(data$region)
data$region <- as.numeric(data$region)

# Create dummy variables for region
region_dummies <- model.matrix(~region - 1, data = data)
colnames(region_dummies) <- gsub("region", "", colnames(region_dummies))
data <- cbind(data, region_dummies)

head(data,5)


## NORMALIZATION

# Selecting the numerical columns to normalize
numerical_cols <- c("age", "bmi", "children", "charges")

# Creating a new data frame with only the numerical columns
numerical_data <- data[, numerical_cols]

# Normalizing the numerical columns
normalized_data <- as.data.frame(scale(numerical_data))

# Adding the non-numerical columns back to the normalized data
normalized_data$sex <- data$sex
normalized_data$smoker <- data$smoker
normalized_data$region <- data$region

# Viewing the normalized data
head(normalized_data)
head(data,2)


## IDENTIFYING AND PRINTING UNIQUE VALUES 

for(col in names(data)) {
  if(col != "charges" && col != "bmi") {
    print(paste(col, ":", length(unique(data[[col]])), "unique values"))
    print(unique(data[[col]]))
  }
}


# Print unique values for each column except charges and bmi
for (col in colnames(data)) {
  if (col != "charges" & col != "bmi") {
    cat("Unique values for column", col, ":", length(unique(data[[col]])), "\n")
  }
}

# Print number of unique values for charges and bmi
cat("Number of unique values for charges:", length(unique(data$charges)), "\n")
cat("Number of unique values for bmi:", length(unique(data$bmi)), "\n")



# print counts of unique values for all columns except charges and bmi
for (col in names(data)) {
  if (col != "charges" & col != "bmi") {
    cat("Column:", col, "\n")
    cat("No. of unique values:", length(unique(data[[col]])), "\n")
    cat("Unique values:\n")
    print(unique(data[[col]]))
    cat("\n")
  }
}



## Descriptive Analysis


#Descriptive Analysis


ggplot(data, aes(x = sex, y = charges)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Charges") +
  ggtitle("Charges vs Sex")

ggplot(data, aes(x = children, y = charges)) +
  geom_boxplot() +
  labs(x = "Children", y = "Charges") +
  ggtitle("Charges vs Children")


ggplot(data, aes(x = bmi, y = charges)) +
  geom_boxplot() +
  labs(x = "BMI", y = "Charges") +
  ggtitle("Charges vs BMI")

data1<-read.csv("insurance.csv")

ggplot(data, aes(x = bmi, y = charges, color = age)) +
  geom_point() +
  labs(x = "Bmi", y = "Charges", color = "Age") +
  ggtitle("Charges vs Bmi by Age")

ggplot(data1, aes(x = bmi, y = charges, color = region)) +
  geom_point() +
  labs(x = "Bmi", y = "Charges", color = "region") +
  ggtitle("Charges vs Bmi by region ")

ggplot(data1, aes(x = bmi, y = charges, color = smoker)) +
  geom_point() +
  labs(x = "Bmi", y = "Charges", color = "smoker") +
  ggtitle("Charges vs Bmi by smoker ") +
  scale_color_manual(values = c("blue", "red"))


ggplot(data1 %>% group_by(age, smoker) %>% summarize(mean_charges = mean(charges)),
       aes(x = age, y = mean_charges, color = smoker)) +
  geom_line(aes(group = smoker), size = 1) +
  labs(x = "BMI", y = "Mean Charges", color = "Smoker") +
  ggtitle("Mean Charges vs age by Smoker") +
  scale_color_manual(values = c("blue", "red")) 


#can be done after binning
ggplot(data1, aes(x = bmi, y =charges, color = sex)) +
  geom_line(aes(group = smoker), size = 1) +
  labs(x = "Bmi", y = "Charges", color = "sex") +
  ggtitle("Charges vs Bmi by sex")


# Summarize mean charges by sex
summarized_by_sex <- data1 %>%
  group_by(sex) %>%
  summarize(mean_charges = mean(charges))

# Plot mean charges by sex
ggplot(summarized_by_sex, aes(x = sex, y = mean_charges)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Charges by Sex", x = "Sex", y = "Mean Charges")

# Summarize mean charges by number of children
summarized_by_children <- data %>%
  group_by(children) %>%
  summarize(mean_charges = mean(charges))

# Plot mean charges by number of children
ggplot(summarized_by_children, aes(x = children, y = mean_charges)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Charges by Number of Children", x = "Number of Children", y = "Mean Charges")


# Summarize mean charges by smoker status
summarized_by_smoker <- data1 %>%
  group_by(smoker) %>%
  summarize(mean_charges = mean(charges))

# Plot mean charges by smoker status
ggplot(summarized_by_smoker, aes(x = smoker, y = mean_charges)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Charges by Smoker Status", x = "Smoker Status", y = "Mean Charges")

# Summarize mean charges by region
summarized_by_region <- data1 %>%
  group_by(region) %>%
  summarize(mean_charges = mean(charges))

# Plot mean charges by region
ggplot(summarized_by_region, aes(x = region, y = mean_charges)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Charges by Region", x = "Region", y = "Mean Charges")

data1 %>%
  group_by(sex) %>%
  summarize(mean_charges = mean(charges)) %>%
  ggplot(aes(x = sex, y = mean_charges, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(x = "Sex", y = "Mean Charges", title = "Mean Charges by Sex")

data %>%
  group_by(children) %>%
  summarize(mean_charges = mean(charges)) %>%
  ggplot(aes(x = children, y = mean_charges, fill = children)) +
  geom_col() +
  labs(x = "Number of Children", y = "Mean Charges", title = "Mean Charges by Number of Children")

data1 %>%
  group_by(smoker) %>%
  summarize(mean_charges = mean(charges)) %>%
  ggplot(aes(x = smoker, y = mean_charges, fill = smoker)) +
  geom_col() +
  scale_fill_manual(values = c("blue", "red")) +  # set custom colors
  labs(x = "Smoker", y = "Mean Charges", title = "Mean Charges by Smoking Status")
  
data1 %>%
  group_by(region) %>%
  summarize(mean_charges = mean(charges)) %>%
  ggplot(aes(x = region, y = mean_charges, fill = region)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Mean Charges", title = "Mean Charges by Region")


# summarize charges by smoker and bmi
data_summary <- data1 %>%
  group_by(smoker, bmi) %>%
  summarize(mean_charges = mean(charges))

# create line graph
ggplot(data_summary, aes(x = bmi, y = mean_charges, color = smoker)) +
  geom_line(aes(group = smoker), size = 1) +
  labs(x = "Bmi", y = "Mean charges", color = "Smoker") +
  ggtitle("Mean charges vs Bmi by smoker") +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw()

ggplot(data1 %>% group_by(bmi, smoker) %>% summarize(mean_charges = mean(charges)),
       aes(x = bmi, y = mean_charges, color = smoker)) +
  geom_line(aes(group = smoker), size = 1) +
  labs(x = "BMI", y = "Mean Charges", color = "Smoker") +
  ggtitle("Mean Charges vs BMI by Smoking Status") +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw()



ggplot(data1 %>% group_by(age, smoker) %>% summarize(mean_charges = mean(charges)),
       aes(x = age, y = mean_charges, color = smoker)) +
  geom_line(aes(group = smoker), size = 1) +
  labs(x = "BMI", y = "Mean Charges", color = "Smoker") +
  ggtitle("Mean Charges vs BMI by Smoking Status") +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw()


ggplot(data1, aes(x = age, y = charges)) +
  geom_point() +
  labs(x = "Age", y = "Charges") +
  ggtitle("Charges vs Age")

ggplot(data1, aes(x = bmi, y = charges)) +
  geom_point() +
  labs(x = "BMI", y = "Charges") +
  ggtitle("Charges vs BMI")

pairs(data[, c("age", "sex", "bmi", "children", "smoker", "region", "charges")])


ggplot(data1, aes(x = factor(sex), y = charges)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Charges") +
  ggtitle("Charges vs Sex")

ggplot(data1, aes(x = factor(children), y = charges)) +
  geom_boxplot() +
  labs(x = "Children", y = "Charges") +
  ggtitle("Charges vs Children")

ggplot(data1, aes(x = factor(smoker), y = charges)) +
  geom_boxplot() +
  labs(x = "Smoker", y = "Charges") +
  ggtitle("Charges vs Smoker")

ggplot(data1, aes(x = region, y = charges)) +
  geom_boxplot() +
  labs(x = "Region", y = "Charges") +
  ggtitle("Charges vs Region")


ggplot(data1, aes(y = charges, x = sex, fill = sex)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Charges", fill = "Sex", title = "Boxplot of Charges by Sex")

ggplot(data1, aes(y = charges, x = smoker, fill = smoker)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#ADD8E6", "#FF5733")) + # set custom colors
  labs(x = "Smoker", y = "Charges", fill = "Smoker", title = "Boxplot of Charges by Smoking Status")


# Visualize the distribution of the numeric variables using histograms and box plots
ggplot(data1, aes(x = age)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Age", y = "Count") +
  ggtitle("Distribution of Age")

ggplot(data1, aes(x = bmi)) +
  geom_histogram(binwidth = 3, color = "black", fill = "lightblue") +
  labs(x = "BMI", y = "Count") +
  ggtitle("Distribution of BMI")

ggplot(data1, aes(x = children)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(x = "Children", y = "Count") +
  ggtitle("Distribution of Children")

ggplot(data1, aes(x = charges)) +
  geom_histogram(binwidth = 5000, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Charges")


# Box plot of charges by categorical variables
ggplot(data1, aes(x = sex, y = charges, fill = smoker)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Charges", fill = "Smoker") +
  ggtitle("Charges by Sex and Smoker Status")


ggplot(data1, aes(x = region, y = charges, fill = smoker)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#ADD8E6", "#FF5733")) + # set custom colors
  labs(x = "Region", y = "Charges", fill = "Smoker") +
  ggtitle("Charges by Region and Smoker Status")


# Scatterplot of charges by age and smoker status
ggplot(data1, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.7) +
  labs(x = "Age", y = "Charges", color = "Smoker") +
  ggtitle("Healthcare Charges by Age and Smoker Status")

# Relationship between 3 factors
library(plotly)
plot_ly(insurance, x = ~age, y = ~bmi, z = ~charges, color = ~smoker,
        colors = c("#69b3a2", "#404080"), type = "scatter3d",
        marker = list(opacity = 0.9, size = 2),
        showlegend = TRUE) %>%
  layout(scene = list(xaxis = list(title = "Age"),
                      yaxis = list(title = "BMI"),
                      zaxis = list(title = "Charges")))

ggplot(insurance, aes(x = bmi, y = charges, color = factor(children))) +
  geom_point(alpha = 0.5) +
  ggtitle("Relationship between BMI, Children, and Charges") +
  labs(x = "BMI", y = "Charges", color = "Children")



ggplot(insurance, aes(x = bmi, y = charges, color = smoker, size = bmi)) +
  geom_point(alpha = 0.5) +
  ggtitle("Relationship between BMI, Smoker Status, and Charges") +
  labs(x = "BMI", y = "Charges", color = "Smoker", size = "BMI")



ggplot(insurance, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  ggtitle("Relationship between BMI, Smoker Status, and Charges") +
  labs(x = "BMI", y = "Charges", color = "Smoker Status")


# Create a new column for children as binary (yes=1 or above, no=0)
insurance$has_children <- ifelse(insurance$children > 0, "yes", "no")

# Check the relationship between BMI, Children, and Charges using a bar plot
ggplot(insurance, aes(x = has_children, y = charges, fill = factor(children))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  ggtitle("Relationship between BMI, Children, and Charges") +
  labs(x = "Has Children", y = "Charges", fill = "Children")


# Create a new column for children as binary (yes=1 or above, no=0)
insurance$has_children <- ifelse(insurance$children > 0, "yes", "no")

# Calculate the mean charges for each group
charges_summary <- aggregate(charges ~ has_children + children, data = insurance, FUN = mean)

# Plot the mean charges for each group using a bar plot
ggplot(charges_summary, aes(x = has_children, y = charges, fill = factor(children))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  ggtitle("Relationship between BMI, Children, and Charges") +
  labs(x = "Has Children", y = "Average Charges", fill = "Children")

insurance %>%
  mutate(bmi_cat = cut(bmi, breaks = c(0, 18.5, 24.9, 29.9, Inf), labels = c("Underweight", "Normal weight", "Overweight", "Obese"))) %>%
  group_by(bmi_cat, smoker, children) %>%
  summarise(avg_charges = mean(charges)) %>%
  ggplot(aes(x = bmi_cat, y = avg_charges, fill = smoker)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ children) +
  ggtitle("Average Charges by BMI, Smoker Status, and Children") +
  labs(x = "BMI Category", y = "Average Charges", fill = "Smoker Status")

#binning

insurance %>%
  mutate(bmi_cat = cut(bmi, breaks = c(0, 18.5, 24.9, 29.9, Inf), labels = c("Underweight", "Normal weight", "Overweight", "Obese"))) %>%
  group_by(bmi_cat, smoker, children) %>%
  summarise(avg_charges = mean(charges)) %>%
  ggplot(aes(x = bmi_cat, y = avg_charges, fill = smoker)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ children) +
  ggtitle("Average Charges by BMI, Smoker Status, and Children") +
  labs(x = "BMI Category", y = "Average Charges", fill = "Smoker Status")


# Calculate the average charges for each combination of smoker, children, and BMI
insurance_summary <- insurance %>%
  group_by(smoker, children, bmi_cat) %>%
  summarise(avg_charges = mean(charges),
            count = n())

# Standardize the average charges
insurance_summary <- insurance_summary %>%
  mutate(z_score = (avg_charges - mean(avg_charges))/sd(avg_charges))

# Plot the standardized average charges for each combination of smoker, children, and BMI
ggplot(insurance_summary, aes(x = smoker, y = z_score, fill = factor(children))) +
  geom_col(position = "dodge") +
  facet_wrap(~bmi_cat, ncol = 2) +
  ggtitle("Standardized Average Charges by Smoker, Children, and BMI") +
  labs(x = "Smoker", y = "Standardized Average Charges", fill = "Children")






# Convert BMI into categorical variable
insurance$bmi_cat <- cut(insurance$bmi, 
                         breaks = c(-Inf, 18.5, 25, 30, Inf), 
                         labels = c("Underweight", "Normal", "Overweight", "Obese"))

# Convert children to binary variable
insurance$children_status <- ifelse(insurance$children > 0, "yes", "no")

# Create a new column for smoker status
insurance$smoker_status <- ifelse(insurance$smoker == "yes", "Smoker", "Non-smoker")

# Calculate average charges by BMI category, smoker status, and children status
charges_summary <- insurance %>% 
  group_by(bmi_cat, smoker_status, children_status) %>% 
  summarise(mean_charges = mean(charges), .groups = "drop")

# Plot the bar chart with labels
ggplot(charges_summary, aes(x = bmi_cat, y = mean_charges, fill = smoker_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~children_status) +
  labs(title = "Average Charges by BMI Category, Smoker Status, and Children Status",
       x = "BMI Category", y = "Mean Charges", fill = "Smoker Status") +
  geom_text(aes(label = scales::comma(round(mean_charges, 2)), 
                y = mean_charges + 500), 
            position = position_dodge(width = 1),
            size = 3)


# Create age bins with names
age_bins <- cut(x = insurance$age,
                breaks = c(18, 27, 39, 51, 64),
                labels = c("Young Adult", "Adult", "Middle-aged", "Senior"))

# Print the bins with names
cat("Age Bins: ", table(age_bins), "\n")


# Create BMI bins
bmi_bins <- cut(x = insurance$bmi,
                breaks = c(15.96, 26.3, 30.4, 34.69, 53.13),
                labels = c("Underweight", "Normal", "Overweight", "Obese"))

# Print the bins
cat("BMI Bins: ", table(bmi_bins))

# Create age and BMI bins using ifelse function
insurance$age_bins <- ifelse(insurance$age < 20, "Young Adult", 
                             ifelse(insurance$age <30, "Adult",
                                    ifelse(insurance$age <50, "Middle-age", 
                                           ifelse(insurance$age < 64, "Senior","veryold"))))

insurance$bmi_bins <- ifelse(insurance$bmi < 19, "Underweight", 
                             ifelse(insurance$bmi < 27, "Normal weight", 
                                    ifelse(insurance$bmi < 35, "Overweight", 
                                           ifelse(insurance$bmi > 54, "obese", "Too obese"))))

summary(insurance)
# View the updated dataset
head(insurance)


# Create age bins using cut function
insurance$age_bin <- cut(insurance$age, breaks = c(18, 30, 40, 50, 65))

# View the updated dataset
head(insurance)


# Plot age, BMI, and smoker_status using a stacked bar chart
ggplot(data = insurance, aes(x = age_bins, fill = bmi_bins)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(smoker_status)) +
  scale_x_discrete(name = "Age") +
  scale_fill_manual(name = "BMI", values = c("Underweight" = "#FFCC00", "Normal" = "#00BFFF", "Overweight" = "#FFA07A", "Obese" = "#FF5733")) +
  labs(title = "BMI distribution by Age and Smoker Status") +
  theme_bw()


# Create a heatmap of correlation between all numeric variables
library(corrplot)

corr <- cor(insurance[, c("age", "bmi", "children", "charges")])
corrplot(corr, method = "color", type = "lower", tl.col = "black", tl.srt = 45)



# Create a violin plot of charges grouped by region
ggplot(data = insurance, aes(x = region, y = charges, fill = region)) +
  geom_violin() +
  scale_fill_discrete(guide = FALSE) +
  labs(title = "Distribution of Charges by Region") +
  theme_bw()



# Create a histogram with density curves of charges by age bins
ggplot(data = insurance, aes(x = charges, fill = age_bins)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.5, color = "red") +
  facet_wrap(~age_bins, ncol = 1) +
  labs(title = "Distribution of Charges by Age Bins") +
  theme_bw()

# Create a stacked bar chart of charges by BMI and smoker status
ggplot(data = insurance, aes(x = bmi_bins, y = charges, fill = smoker)) +
  geom_bar(stat = "identity") +
  labs(title = "Charges by BMI and Smoker Status") +
  theme_bw()

ggplot(data = insurance, aes(x = bmi_bins, y = charges, fill = smoker)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~smoker, ncol = 2) +
  labs(title = "Charges by BMI and Smoker Status") +
  theme_bw()



ggplot(data = insurance, aes(x = bmi_bins, y = charges, fill = smoker)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Charges by BMI and Smoker Status", y = "Charges") +
  scale_fill_manual(values = c("#FFA500", "#00BFFF"), 
                    labels = c("Non-Smoker", "Smoker")) +
  theme_bw()


# Set the order of bmi_bins levels
insurance$bmi_bins <- factor(insurance$bmi_bins, levels = c("Underweight", "Normal weight", "Overweight", "Obese", "Too obese"))

# Create a stacked bar chart of charges by BMI and smoker status
ggplot(data = insurance, aes(x = bmi_bins, y = charges, fill = smoker)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Charges by BMI and Smoker Status", y = "Charges") +
  scale_fill_manual(values = c("#FFA500", "#00BFFF"), 
                    labels = c("Non-Smoker", "Smoker")) +
  theme_bw()


# Set the order of bmi_bins levels
insurance$bmi_bins <- factor(insurance$bmi_bins, levels = c("Underweight", "Normal weight", "Overweight", "Obese", "Too obese"))

ggplot(data = insurance, aes(x = bmi_bins, y = charges, fill = smoker, color = sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Charges by BMI, Smoker Status, and Sex", y = "Charges") +
  scale_fill_manual(values = c("#FFA500", "#00BFFF"), 
                    labels = c("Non-Smoker", "Smoker")) +
  scale_color_manual(values = c("#990000", "#000099"), labels = c("Female", "Male")) +
  theme_bw()



ggplot(data = insurance, aes(x = bmi_bins, y = charges, fill = smoker)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Charges by BMI, Smoker Status and Sex", y = "Charges") +
  scale_fill_manual(values = c("#FFA500", "#00BFFF"), 
                    labels = c("Non-Smoker", "Smoker")) +
  facet_grid(. ~ sex) +
  theme_bw()


ggplot(data = insurance, aes(x = bmi_bins, y = charges, 
                             group = interaction(bmi_bins, smoker, sex),
                             color = smoker)) +
  geom_line(size = 1) +
  labs(title = "Interaction of Charges by BMI, Smoker Status, and Sex", y = "Average Charges") +
  scale_color_manual(values = c("#00BFFF", "#FFA500"), 
                     labels = c("Non-Smoker", "Smoker")) +
  theme_bw()

#Clustering
library(ggplot2)

ggplot(insurance, aes(x=bmi, y=charges, color=smoker)) + 
  geom_point() +
  stat_smooth(method="lm", formula = y ~ x, se=F) +
  xlab("BMI") +
  ylab("Charges") +
  ggtitle("Interaction between smoking and BMI on insurance charges") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic()



ggplot(insurance, aes(x=age, y=charges, colour=smoker)) +
  geom_smooth(method="lm", se=FALSE) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  labs(title="Age and Smoking Interaction", x="Age", y="Insurance Charges")



ggplot(insurance, aes(x=bmi, y=charges, colour=smoker)) +
  geom_smooth(method="lm", se=FALSE) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  labs(title="BMI and Smoking Interaction", x="BMI", y="Insurance Charges")



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

# Create binary codes for the "sex" variable
data$sex <- ifelse(data$sex == "male", 1, 0)

# Create binary codes for the "smoker" variable
data$smoker <- ifelse(data$smoker == "yes", 1, 0)

# Create binary codes for the "region" variable
data$region <- ifelse(data$region == "northeast", 1, 
                      ifelse(data$region == "northwest", 2, 
                             ifelse(data$region == "southeast", 3, 4)))

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

# Load the data
data <- read.csv("insurance.csv", header = TRUE)

# View the first few rows of the data
head(data)

# Check for missing values
sum(is.na(data))

# Check the data types of each variable
str(data)

# Generate summary statistics for the numerical variables
summary(data[c("age", "BMI", "children", "charges")])

# Visualize the distribution of the numerical variables
library(ggplot2)
ggplot(data, aes(x = charges)) + 
  geom_histogram(binwidth = 1000) + 
  labs(title = "Distribution of Charges")

ggplot(data, aes(x = age)) + 
  geom_histogram() + 
  labs(title = "Distribution of Age")

ggplot(data, aes(x = bmi)) + 
  geom_histogram() + 
  labs(title = "Distribution of BMI")

ggplot(data, aes(x = children)) + 
  geom_histogram() + 
  labs(title = "Distribution of Children")



# Create dummy variables for categorical variables
data <- within(data, {
  sex <- factor(sex, levels = c("male", "female"))
  smoker <- factor(smoker, levels = c("no", "yes"))
  region <- factor(region, levels = c("northeast", "northwest", "southeast", "southwest"))
  sex <- as.numeric(sex) - 1
  smoker <- as.numeric(smoker) - 1
  region <- as.numeric(region) - 1
})


# Split the data into training and testing sets
set.seed(123)
train_idx <- sample(nrow(data), 0.7 * nrow(data))
train <- data[train_idx, ]
test <- data[-train_idx, ]


# Fit a linear regression model
lm.fit <- lm(charges ~ ., data = train)
summary(lm.fit)
pred_lm <- predict(lm.fit, newdata = test)
mse_lm <- mean((pred_lm - test$charges)^2)
rmse_lm <- sqrt(mse_lm)
rmse_lm
adjr_lm <- summary(lm.fit)$adj.r.squared
adjr_lm

#vif test
library(car)
vif(lm.fit)
# Fit a lasso regression model
library(glmnet)
x <- model.matrix(charges ~ ., data = train)[,-1]
y <- train$charges
lasso.fit <- glmnet(x, y, alpha = 1)
plot(lasso.fit)
pred_lasso <- predict(lasso.fit, s = 0.01, newx = x_test)
mse_lasso <- mean((pred_lasso - y_test)^2)
rmse_lasso <- sqrt(mse_lasso)
rmse_lasso
adjr_lasso <- NA # Lasso regression does not provide adjusted R-squared



# Fit a random forest model
library(randomForest)
rf.fit <- randomForest(charges ~ ., data = train, ntree = 10000, importance = TRUE)
plot(rf.fit)
pred_rf <- predict(rf.fit,newdata = test)
mse_rf <- mean((pred_rf - test$charges)^2)
mse_rf
rmse_rf <- sqrt(mse_rf)
adjr_rf <- NA # Random forest does not provide adjusted R-squared



library(caret)
library(pls)

# create principal components using the train set
pca_fit <- preProcess(train, method = "pca")
train_pca <- predict(pca_fit, train)
test_pca <- predict(pca_fit, test)



# train the random forest model on the principal components
rf_fit2 <- randomForest(train$charges ~ ., 
                        data = train_pca, 
                        ntree = 10000, 
                        importance = TRUE)

# make predictions on the test set using the principal components
pred_rf2 <- predict(rf_fit, newdata = test_pca)

# calculate RMSE
mse_rf2 <- mean((pred_rf2 - test$charges)^2)
mse_rf2
rmse_rf2 <- sqrt(mse_rf2)
rmse_rf2
adjr_rf2 <- NA


# Fit a decision tree model
library(rpart)
tree.fit <- rpart(charges ~ ., data = train, method = "anova")
printcp(tree.fit)
plot(tree.fit)

# Plot the decision tree
library(rpart.plot)
options(scipen = 999) # disable scientific notation
rpart.plot(tree.fit, extra = 1, box.palette = c("#FFFFFF", "#EFEFEF"),cex = 0.9)
pred_tree <- predict(tree.fit, newdata = test)
mse_tree <- mean((pred_tree - test$charges)^2)
rmse_tree <- sqrt(mse_tree)
rmse_tree
adjr_tree <- NA # Decision tree does not provide adjusted R-squared



#Adjusting hyperparameters using grid search and cross-validation:

library(caret)

# define the parameter grid
param_grid <- expand.grid(cp = seq(0.001, 0.1, 0.001))

# perform grid search using 5-fold cross-validation
tree_fit2 <- train(charges ~ ., 
                   data = train, 
                   method = "rpart", 
                   trControl = trainControl(method = "cv", 
                                            number = 5),
                   tuneGrid = param_grid)

# make predictions on the test set
pred_tree2 <- predict(tree_fit2, newdata = test)
# calculate RMSE
mse_tree2 <- mean((pred_tree2 - test$charges)^2)
rmse_tree2 <- sqrt(mse_tree2)
rmse_tree2
adjr_tree2 <- NA

# Gradient Boosting model using xgboost
x_train <- as.matrix(train[, -7])
y_train <- train$charges
x_test <- as.matrix(test[, -7])
y_test <- test$charges

xgb.fit <- xgboost(data = x_train, label = y_train, nrounds = 1000, objective = "reg:squarederror", eta = 0.01, max_depth = 5, gamma = 0.1, subsample = 0.5, colsample_bytree = 0.5)
pred_xgb <- predict(xgb.fit, newdata = x_test)
mse_xgb <- mean((pred_xgb - y_test)^2)
rmse_xgb <- sqrt(mse_xgb)
rmse_xgb
adjr_xgb <- NA # xgboost does not provide adjusted R-squared


# Gradient Boosting model using lightgbm
lgb.fit <- lgb.train(data = lgb.Dataset(x_train, label = y_train), objective = "regression", num_leaves = 50, learning_rate = 0.01, nrounds = 1000, lambda_l2 = 0.1, bagging_fraction = 0.8, bagging_freq = 5, verbose = -1)
pred_lgb <- predict(lgb.fit, x_test)
mse_lgb <- mean((pred_lgb - y_test)^2)
rmse_lgb <- sqrt(mse_lgb)
rmse_lgb
adjr_lgb <- NA #


# Support vector regression model
svm.fit <- svm(charges ~ ., data = train, kernel = "linear", cost = 10, epsilon = 0.1)
pred_svm <- predict(svm.fit, newdata = test)
mse_svm <- mean((pred_svm - y_test)^2)
mse_svm
rmse_svm <- sqrt(mse_svm)
rmse_svm
adjr_svm <- NA #





metrics_df <- data.frame(Model = c("Linear Regression", "Lasso Regression", "Random Forest", 
                                   "Random Forest with PCA", "Decision Tree", "Decision Tree2 Hyperparamete", 
                                   "GB using XGboost", "GB using LightGBM", "Support Vector"),
                         MSE = c(mse_lm, mse_lasso, mse_rf, mse_rf2, mse_tree, mse_tree2, mse_xgb, mse_lgb, mse_svm),
                         RMSE = c(rmse_lm, rmse_lasso, rmse_rf, rmse_rf2, rmse_tree, rmse_tree2, rmse_xgb, rmse_lgb, rmse_svm),
                         Adj_R_Squared = c(adjr_lm, adjr_lasso, adjr_rf, adjr_rf2, adjr_tree, adjr_tree2, adjr_xgb, adjr_lgb, adjr_svm))

print(metrics_df)












