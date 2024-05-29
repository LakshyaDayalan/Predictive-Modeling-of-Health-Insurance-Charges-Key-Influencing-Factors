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


[4/11 1:44 PM] Singi Reddy, Harish Reddy




#


library(randomForest)

library(gbm)

library(lmtest)

library(car)

library(xgboost)



# Importing the dataset

insurance_data <- read.csv("insurance.csv")



# Checking the structure of the dataset

str(insurance_data)



# Converting categorical variables into factors

insurance_data$sex <- as.factor(insurance_data$sex)

insurance_data$smoker <- as.factor(insurance_data$smoker)

insurance_data$region <- as.factor(insurance_data$region)


insurance_data$smoker <- ifelse(insurance_data$smoker == "yes", 1, 0)

# Splitting the dataset into training and testing sets

set.seed(123)

train_index <- sample(nrow(insurance_data), 0.8 * nrow(insurance_data))

train_data <- insurance_data[train_index, ]

test_data <- insurance_data[-train_index, ]


# Model 1: BMI, Age, Smoker, and Region

model1 <- lm(charges ~ bmi + age + smoker + region + children+sex, data = train_data)

pred1 <- predict(model1, newdata = test_data)

summary(model1)

RMSE1 <- RMSE(pred1, test_data$charges)

RMSE1


library(ggplot2)

# create a data frame with the actual and predicted values
results <- data.frame(actual = test_data$charges, predicted = pred1)

# plot the actual vs predicted values
ggplot(results, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = coef(model1)[1], slope = coef(model1)[2]) +
  labs(x = "Actual Charges", y = "Predicted Charges") +
  ggtitle("Linear Regression Model")

library(ggplot2)

# create a data frame with the independent variable (bmi) values and the predicted charges
pred_df <- data.frame(bmi = test_data$bmi, charges = test_data$charges)

# plot the predicted charges vs bmi
ggplot(pred_df, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "BMI", y = "Charges") +
  ggtitle("Linear Regression Model: Charges vs BMI")


# create a data frame with the independent variable (bmi) values and the predicted charges
pred_df <- data.frame(bmi = test_data$bmi, charges = pred1)

# plot the predicted charges vs bmi
ggplot(pred_df, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "BMI", y = "Charges") +
  ggtitle("Linear Regression Model: Charges vs BMI")


#random forest

rf_model <- randomForest(charges ~ age + sex + bmi + children + smoker + region, data = train_data,)
rf_model

summary(rf_model)

#Making predictions on test data

rf_predictions <- predict(rf_model, newdata = test_data)

rf_predictions





#Evaluating the model using RMSE

rf_rmse <- sqrt(mean((rf_predictions - test_data$charges)^2))

rf_rmse



#Creating the interaction variable between smoker and bmi



train_data$smoker_bmi_interaction <- train_data$smoker * train_data$bmi

test_data$smoker_bmi_interaction <- test_data$smoker * test_data$bmi



#Building a linear regression model with the interaction variable

lm_interaction_model <- lm(charges ~ age + sex + bmi + children + smoker + region + smoker_bmi_interaction, data = train_data)



#Getting the model summary

summary(lm_interaction_model)



#Making predictions on test data

interaction_predictions <- predict(lm_interaction_model, newdata = test_data)



#Evaluating the model using RMSE

interaction_rmse <- sqrt(mean((interaction_predictions - test_data$charges)^2))

interaction_rmse



#Building a random forest model with the interaction variable

rf_interaction_model <- randomForest(charges ~ age + sex + bmi + children + smoker + region + smoker_bmi_interaction, data = train_data)



#Making predictions on test data

rf_interaction_predictions <- predict(rf_interaction_model, newdata = test_data)



#Evaluating the model using RMSE

rf_interaction_rmse <- sqrt(mean((rf_interaction_predictions - test_data$charges)^2))

rf_interaction_rmse



#Building a gradient boosting model with the interaction variable

#gb_interaction_model <- xgboost(charges ~ age + sex + bmi + children + smoker + region + smoker_bmi_interaction, data = train_data)



#Making predictions on test data

#gb_interaction_predictions <- predict(gb_interaction_model, newdata = tes_data)



#Evaluating the model using RMSE

#gb_interaction_rmse <- sqrt(mean((gb_interaction_predictions - test_data$charges)^2))

#gb_interaction_rmse



#Creating additional interaction variables

train_data$smoker_age_interaction <- train_data$smoker * train_data$age

test_data$smoker_age_interaction <- test_data$smoker * test_data$age

train_data$bmi_age_interaction <- train_data$bmi * train_data$age

test_data$bmi_age_interaction <- test_data$bmi * test_data$age



#Building a random forest model with all interaction variables

rf_all_interaction_model <- randomForest(charges ~ age + sex + bmi + children + smoker + region + smoker_bmi_interaction + smoker_age_interaction + bmi_age_interaction, data = train_data)



#Making predictions on test data

rf_all_interaction_predictions <- predict(rf_all_interaction_model, newdata = test_data)



#Evaluating the model using RMSE

rf_all_interaction_rmse <- sqrt(mean((rf_all_interaction_predictions - test_data$charges)^2))

rf_all_interaction_rmse



#Building a random forest model with all interaction variables

rf_all_interaction_model <- randomForest(charges ~ age + sex + bmi + children + smoker + region + smoker_bmi_interaction + smoker_age_interaction + bmi_age_interaction, data = trainData)










