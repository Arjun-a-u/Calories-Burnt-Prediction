install.packages("reshape2")
install.packages("xgboost")
install.packages("Metrics")

library(reshape2)
library(caret)
library(xgboost)
library(Metrics)



calories<-read.csv('D:/data/calories.csv')
View(calories)
exercise_data<-read.csv('D:/data/exercise.csv')
View(exercise_data)

# Combine the data frames along columns with 'Calories' column
calories_data <- cbind(exercise_data, calories)
View(calories_data) # DATA

shape <- dim(calories_data)
print(shape)
str(calories_data)

# Count the number of missing values (NA) in each column
missing_counts <- colSums(is.na(calories_data))
print(missing_counts)

#Data Analysis
summary(calories_data)

calories_data$Gender <- ifelse(calories_data$Gender == "male", 0, 
                               ifelse(calories_data$Gender == "female", 1, calories_data$Gender))
View(calories_data)


######################Data Visualization################
library(ggplot2)
# Create a countplot for the 'Gender' column
ggplot(calories_data, aes(x = Gender)) +
  geom_bar() +
  labs(x = "Gender", y = "Count", title = "Countplot of Gender")


ggplot(calories_data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Distribution of Age")


# Create a histogram (distribution plot) for the 'Height' column
ggplot(calories_data, aes(x = Height)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Height", y = "Frequency", title = "Distribution of Height")

## finding the distribution of "Weight" column
ggplot(calories_data, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Weight", y = "Frequency", title = "Distribution of Weight")

#Finding the Correlation in the dataset
#Positive Correlation
#Negative Correlation
correlation <- cor(calories_data[, sapply(calories_data, is.numeric)])
heatmap_plot <- ggplot(data = melt(correlation), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")

# Print the heatmap plot
print(heatmap_plot)



#Separating features and Target
# Create the predictor variable data frame (X) by excluding 'User_ID' and 'Calories' columns
X <- calories_data[, !(names(calories_data) %in% c('User_ID', 'Calories'))]

# Create the target variable data frame (Y) by selecting the 'Calories' column
Y <- calories_data$Calories
head(X)
head(Y)

#Splitting the data into training data and Test data
splitIndex <- createDataPartition(Y, p = 0.8, list = FALSE)
X_train <- X[splitIndex, ]
X_test <- X[-splitIndex, ]
Y_train <- Y[splitIndex]
Y_test <- Y[-splitIndex]
#Print the dimensions of X
cat("Dimensions of X_train:", dim(X_train), "\n")
cat("Dimensions of X_test:", dim(X_test), "\n")

#Model Training
#XGBoost Regressor
data_matrix <- data.matrix(X_train)
nrounds <- 100

model <- xgboost(data = data.matrix(calories_data), 
                 label = calories_data$Calories, 
                 objective = "reg:squarederror", 
                 nrounds = nrounds,  # Specify the number of rounds
                 verbose = 0, 
                 params = list("silent" = 1))
print(model)

#Evaluation

#Prediction on Test Data

#train_data_prediction <- predict(model, newdata = data.matrix(calories_data))
#print(train_data_prediction)

#test_data_matrix <- data.matrix(X_test)
test_data_prediction <- predict(model, newdata = data.matrix(X_test))
print(test_data_prediction)


###Mean Absolute Error
# Calculate Mean Absolute Error (MAE)
mae <- mae(Y_test, test_data_prediction)
print(mae)

#model$feature_names <- colnames(X_test)
#test_data_prediction <- predict(model, newdata = data.matrix(X_test))
#print(test_data_prediction)

install.packages("caTools")

library(caTools)
split = sample.split(calories_data, SplitRatio = 0.7)
train = subset(calories_data, split == TRUE)
test = subset(calories_data, split == FALSE)
View(train)
View(test)
lmodel<-lm(Calories~.,train)
summary(lmodel)
pred<-predict(lmodel,test)
pred


