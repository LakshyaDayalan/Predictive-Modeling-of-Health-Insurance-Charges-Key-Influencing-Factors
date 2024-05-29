# Load the dataset
insurance <- read.csv("insurance.csv")

# View the first few rows of the dataset
head(insurance)

# Check the structure of the dataset
str(insurance)

# Check the summary statistics of the numerical variables
summary(insurance[, c("age", "bmi", "children", "charges")])

# Check the distribution of the numerical variables using histograms
library(ggplot2)
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


# Convert the categorical variables to factors
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

# Split the dataset into training and testing sets
library(caret)
set.seed(123)
trainIndex <- createDataPartition(insurance$charges, p = 0.7, list = FALSE)
train <- insurance[trainIndex, ]
test <- insurance[-trainIndex, ]

# Build a linear regression model
model_lm <- lm(charges ~ ., data = train)
summary(model_lm)

# Build a decision tree model
library(rpart)
model_dt <- rpart(charges ~ ., data = train, method = "anova")
summary(model_dt)
plot(model_dt, main = "Decision Tree")

# Build a random forest model
library(randomForest)
model_rf <- randomForest(charges ~ ., data = train)
summary(model_rf)


# Check the relationship between age and charges using a scatter plot
ggplot(insurance, aes(x = age, y = charges)) +
  geom_point(alpha = 0.5, color = "#69b3a2") +
  geom_smooth(method = "lm", se = FALSE, color = "#404080") +
  ggtitle("Relationship between Age and Charges")

# Check the relationship between bmi and charges using a scatter plot
ggplot(insurance, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.5, color = "#69b3a2") +
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




#next


ggplot(insurance, aes(x = bmi, y = charges, color = smoker, size = bmi)) +
  geom_point(alpha = 0.5) +
  ggtitle("Relationship between BMI, Smoker Status, and Charges") +
  labs(x = "BMI", y = "Charges", color = "Smoker", size = "BMI")



ggplot(insurance, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  ggtitle("Relationship between BMI, Smoker Status, and Charges") +
  labs(x = "BMI", y = "Charges", color = "Smoker Status")



#next

ggplot(insurance %>% filter(region == "southeast"), 
       aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  ggtitle("Relationship between BMI, Smoker, and Charges in Southeast Region") +
  labs(x = "BMI", y = "Charges", color = "Smoker")



summary(insurance)


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


