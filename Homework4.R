
install.packages("palmerpenguins")
install.packages("ISLR")
install.packages("caret")
install.packages("naivebayes")
install.packages("naivebayes")
install.packages("pROC")

library(caret)
library(naivebayes)
library(ISLR)
library(dplyr)
library(ggplot2)
library(e1071)  
library(palmerpenguins)

head(penguins)

#split into training and testing data randomly 
Ind <- sample(2, nrow(penguins), replace=T, prob=c(0.8, 0.2))
train <- penguins[Ind==1,] #training data (80%)
test <- penguins[Ind==2,] #testing data (20%)

#create naive bayes model to predict penguin species using all data available: island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex, year
model <- naive_bayes(species ~ ., data=train, usekernel=T)

#plot the naive bayes model
plot(model)

#naive bayes predictions
nb_predictions <- predict(model, test)

#plot the predictions for each species in a matrix 
nb_conf_matrix <- table(nb_predictions, test$species)

nb_conf_matrix

#logistic regression for Adelie or not Adelie
glm.fit <- glm(species ~ ., data=train, family=binomial)
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- ifelse(glm.probs>0.5, "Not Adelie", "Adelie")
attach(test)

#plot the predictions for Adelie or not Adelie in a matrix for logistic regression
logreg_conf_matrix <- table(glm.pred, test$species)

logreg_conf_matrix

#calculate percent accuracy for Naive Bayes model - sum diagonal of matrix, then divide by sum of whole matrix
nb_accuracy <- sum(diag(nb_conf_matrix)) / sum(nb_conf_matrix)

#calculate percentage of incorrect predictions
nb_error <- 1 - nb_accuracy

#create data frame for pie chart - with correct and incorrect predictions
nb_pie_data <- data.frame(
  Model = "Naive Bayes",
  Type = c("Correct", "Incorrect"),
  Percentage = c(nb_accuracy, nb_error)
)

#filter the pie chart data to only have the percentages greater than 0
filtered_nb_pie_data <- nb_pie_data[nb_pie_data$Percentage > 0, ]

#plot pie chart for Naive Bayes model
ggplot(filtered_nb_pie_data, aes(x = "", y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Naive Bayes - Predictions", fill = "Prediction", y = "Percentage") +
  theme_void() + theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(100*Percentage, 2), "%")), position = position_stack(vjust = 0.5))



#sum of all predictions in logistic regression model
total_instances <- sum(logreg_conf_matrix)

#sum the correct predictions (these are in specific consistent cells in df)
correct_predictions <- sum(logreg_conf_matrix[1, 1] + logreg_conf_matrix[2, 2] +logreg_conf_matrix[2, 3])

#calculate percentage of incorrect predictions
logreg_error <- (1 - (correct_predictions / total_instances))

#calculate percentage of correct predictions
logreg_accuracy <- (1-logreg_error)

#create data frame for pie chart - with correct and incorrect predictions
logreg_pie_data <- data.frame(
  Model = "Logistic Regression",
  Type = c("Correct", "Incorrect"),
  Percentage = c(logreg_accuracy, logreg_error)
)

#plot pie chart for Logistic Regression model
ggplot(logreg_pie_data, aes(x = "", y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Logistic Regression - Predictions", fill = "Prediction", y = "Percentage") +
  theme_void() + theme(legend.position = "right")+ 
  geom_text(aes(label = paste0(round(100*Percentage, 2), "%")), position = position_stack(vjust = 0.5))




#combine the data frames for Naive Bayes and Logistic Regression
combined_pie_data <- rbind(nb_pie_data, logreg_pie_data)

#sum of all error percent
sumerr <- sum(combined_pie_data[4,3] +combined_pie_data[2,3])

#calculate bayes error percent
bayeserr <- ((combined_pie_data[2,3]/sumerr))

#calculate logistic regression error percent
logerr <- ((combined_pie_data[4,3]/sumerr))

#create data frame for pie chart - with bayes error and logreg error percentages
error_data <- data.frame(
  Model = c("Naive Bayes", "Logistic Regression"),
  Error_Percentage = c(bayeserr, logerr)
)

#plot pie chart for both models' error percentages
ggplot(error_data, aes(x = "", y = Error_Percentage, fill = Model)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Error Percentage Comparison", fill = "Model", y = "Error Percentage") +
  theme_void() + theme(legend.position = "right") +
  geom_text(aes(label = ifelse(Error_Percentage > 0, paste0(round(100 * Error_Percentage, 2), "%"), "")), position = position_stack(vjust = 0.5))
