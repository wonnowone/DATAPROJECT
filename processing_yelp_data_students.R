#Pre-Processing Yelp Academic Data for the Assignment
library(jsonlite)

#Clear
cat("\014")  
rm(list=ls())

#Set Directory as appropriate
#setwd("C/project/script/document")

#Load Different Data
business_data <- stream_in(file("yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
review_data  <- stream_in(file("yelp_academic_dataset_review.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
user_data <- stream_in(file("yelp_academic_dataset_user.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)

str(business_data)

# Specify columns to remove from the nested 'attributes' data frame
columns_to_remove_nested <- c("BikeParking", "CoatCheck","DriveThru", "BusinessAcceptsBitcoin", "GoodForDancing", "AcceptsInsurance", "BYOB", "Corkage", "BYOBCorkage", "HairSpecializesIn", "Open24Hours", "Caters", "HappyHour", "Dietaryrestrictions", "Smoking", "Bestnights","OutdoorSeating", "Restaurantscounterservice", "HasTV", "DogsAllowed", "AgesAllowed")

# Remove specified columns from the nested data frame
business_data$attributes <- business_data$attributes[, !(names(business_data$attributes) %in% columns_to_remove_nested)]

# Removing columns by name
columns_to_remove <- c("name", "address", "city", "state", "postal_code", "latitude", "longitude", "is_open")
business_data <- business_data[, !(names(business_data) %in% columns_to_remove)]

# Removing columns by name
columns_to_remove <- c("name", "yelping_since", "useful", "funny", "cool", "friends", "compliment_hot", "compliment_more", "compliment_profile", "compliment_cute", "compliment_list", "compliment_note", "compliment_plain", "compliment_cool", "compliment_funny", "compliment_writer", "compliment_photos")
user_data <- user_data[, !(names(user_data) %in% columns_to_remove)]

# blanks filling
business_data <- apply(business_data, 2, function(x) replace(x, is.na(x), 0))


library(dplyr)
business_data <- as_tibble(business_data)

# Specifing columns to transform
columns_to_transform <- c("attributes.ByAppointmentOnly", "attributes.BusinessAcceptsCreditCards", "attributes.RestaurantsTakeOut", "attributes.RestaurantsDelivery", "attributes.WheelchairAccessible", "attributes.RestaurantsReservations", "attributes.GoodForKids", "attributes.RestaurantsTableService", "attributes.RestaurantsGoodForGroups")

# Apply the transformation to the specified columns
business_data <- business_data %>%
  mutate_at(vars(columns_to_transform), ~ifelse(. == "True", 1, 0))

# Removing columns by name
columns_to_remove <- c("attributes.Alcohol", "attributes.BestNights", "attributes.RestaurantsCounterService", "attributes.DietaryRestrictions", "attributes.WiFi")
business_data <- business_data[, !(names(business_data) %in% columns_to_remove)]

# Data in business_data sorted(attributes)
library(dplyr)

business_data <- business_data %>%
  mutate(attributes.BusinessParking = ifelse(attributes.BusinessParking %in% c('None', 0), 0, 1))

business_data <- business_data %>%
  mutate(attributes.Ambience = ifelse(attributes.Ambience %in% c('None', 0), 0, 1))

business_data <- business_data %>%
  mutate(attributes.NoiseLevel = ifelse(attributes.NoiseLevel %in% c('quiet', 'average', "u'quiet'", "u'average'"), 1, 0))

business_data <- business_data %>%
  mutate(attributes.RestaurantsAttire = ifelse(attributes.RestaurantsAttire %in% c(0), 0, 1))

business_data <- business_data %>%
  mutate(attributes.GoodForMeal = ifelse(attributes.GoodForMeal %in% c(0), 0, 1))

business_data <- business_data %>%
  mutate(attributes.Music = ifelse(attributes.Music %in% c(0), 0, 1))

str(business_data)

library(dplyr)

business_data <- business_data %>%
  mutate(facilityrelated = rowMeans(cbind(attributes.BusinessParking, attributes.BusinessAcceptsCreditCards), na.rm = TRUE))

business_data <- business_data %>%
  mutate(food_qualityrelated = rowMeans(cbind(attributes.GoodForMeal, attributes.RestaurantsAttire), na.rm = TRUE))

business_data <- business_data %>%
  mutate(atmosphererelated = rowMeans(cbind(attributes.Ambience, attributes.NoiseLevel, attributes.Music), na.rm = TRUE))

business_data <- business_data %>%
  mutate(imagerelated = rowMeans(cbind(attributes.ByAppointmentOnly, attributes.RestaurantsReservations), na.rm = TRUE))

business_data <- business_data %>%
  mutate(servicerelated = rowMeans(cbind(attributes.RestaurantsTableService, attributes.GoodForKids, attributes.RestaurantsGoodForGroups), na.rm = TRUE))

business_data <- business_data %>%
  mutate(accesibilityrelated = rowMeans(cbind(attributes.WheelchairAccessible, attributes.RestaurantsDelivery, attributes.RestaurantsTakeOut), na.rm = TRUE))

business_data <- business_data %>%
  mutate(totalevaluation_attributes = facilityrelated + food_qualityrelated + atmosphererelated + imagerelated + servicerelated + accesibilityrelated)

business_data$attributes.RestaurantsPriceRange2 <- as.numeric(business_data$attributes.RestaurantsPriceRange2)

str(user_data)
# Convert 'review_count' to numeric
business_data$review_count <- as.numeric(business_data$review_count)

# Calculate quantiles
quantiles <- quantile(business_data$review_count, probs = seq(0, 1, 0.2), na.rm = TRUE)
business_data$quintile <- cut(business_data$review_count, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

library(caret)
set.seed(1)

test_index <- createDataPartition(review_data$stars, p = 0.2, list = FALSE)
training_data <- review_data[-test_index, ]
test_data <- review_data[test_index, ]

colnames(user_data)
old_name <- "stars"
new_name <- "business_stars"

names(business_data)[names(business_data) == old_name] <- new_name
colnames(business_data)[colnames(business_data) == old_name] <- new_name

old_name <- "quintile"
new_name <- "quintile_reviewcount"

names(business_data)[names(business_data) == old_name] <- new_name
colnames(business_data)[colnames(business_data) == old_name] <- new_name

# Training data change
# Select columns from business_data
selected_business_data <- business_data[c("business_id", "business_stars", "attributes.RestaurantsPriceRange2", "totalevaluation_attributes", "quintile_reviewcount")]

# Select columns from user_data
selected_user_data <- user_data[c("user_id", "average_stars")]

# Merge training_data, selected_business_data, and selected_user_data
trainingdata_actual <- merge(training_data, selected_business_data, by = "business_id", all.x = TRUE)
trainingdata_actual <- merge(trainingdata_actual, selected_user_data, by = "user_id", all.x = TRUE)

str(trainingdata_actual)
trainingdata_actual$business_stars <- as.numeric(trainingdata_actual$business_stars)

# test data change
# Select columns from business_data
selected_business_data <- business_data[c("business_id", "business_stars", "attributes.RestaurantsPriceRange2", "totalevaluation_attributes", "quintile_reviewcount")]

# Select columns from user_data
selected_user_data <- user_data[c("user_id", "average_stars")]

# Merge training_data, selected_business_data, and selected_user_data
testdata_actual <- merge(test_data, selected_business_data, by = "business_id", all.x = TRUE)
testdata_actual <- merge(testdata_actual, selected_user_data, by = "user_id", all.x = TRUE)

testdata_actual$business_stars <- as.numeric(testdata_actual$business_stars)


# Lasso_1 all the data
library(glmnet)

# names of the columns included in the LASSO model
selected_columns <- c("useful", "funny", "cool", "business_stars", "attributes.RestaurantsPriceRange2", "totalevaluation_attributes", "quintile_reviewcount", "average_stars")  

X_train_selected <- trainingdata_actual[, selected_columns]

X_train_selected_imputed <- apply(X_train_selected, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
y_train_imputed <- trainingdata_actual$stars
y_train_imputed <- ifelse(is.na(y_train_imputed), mean(y_train_imputed, na.rm = TRUE), y_train_imputed)

# Fit a LASSO model
lasso_model_selected <- cv.glmnet(x = as.matrix(X_train_selected_imputed), y = as.vector(y_train_imputed), alpha = 1, parallel = TRUE)

# Choose the lambda value
best_lambda_selected <- lasso_model_selected$lambda.min

# Fit the final LASSO model
final_lasso_model_selected <- glmnet(x = as.matrix(X_train_selected_imputed), y = as.vector(y_train_imputed), alpha = 1, lambda = best_lambda_selected, parallel = TRUE)

# Subset the test data to include only the selected columns
X_test_selected <- testdata_actual[, selected_columns]

X_test_selected_imputed <- apply(X_test_selected, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
y_test_imputed <- testdata_actual$stars
y_test_imputed <- ifelse(is.na(y_test_imputed), mean(y_test_imputed, na.rm = TRUE), y_test_imputed)

# Predict on the test data
lasso_predictions_selected <- predict(final_lasso_model_selected, newx = as.matrix(X_test_selected_imputed), s = best_lambda_selected, parallel = TRUE)

# Evaluate the model
mse_selected <- mean((lasso_predictions_selected - y_test_imputed)^2)

plot(lasso_predictions_selected)

# Get the coefficients
lasso_coefficients <- coef(final_lasso_model_selected, s = best_lambda_selected)
print(lasso_coefficients)


#Lasso_2 review data based
library(glmnet)

# Including specified columns
selected_columns <- c("useful", "funny", "cool")  

# Subset the training data to include only the selected columns
X_train_selected_two <- trainingdata_actual[, selected_columns]

X_train_selected_imputed_two <- apply(X_train_selected_two, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
y_train_imputed_two <- trainingdata_actual$stars
y_train_imputed_two <- ifelse(is.na(y_train_imputed_two), mean(y_train_imputed_two, na.rm = TRUE), y_train_imputed_two)

# Fit a LASSO model
lasso_model_selected_two <- cv.glmnet(x = as.matrix(X_train_selected_imputed_two), y = as.vector(y_train_imputed_two), alpha = 1, parallel = TRUE)

# Choose the lambda value
best_lambda_selected_two <- lasso_model_selected_two$lambda.min

# Fit the final LASSO model
final_lasso_model_selected_two <- glmnet(x = as.matrix(X_train_selected_imputed_two), y = as.vector(y_train_imputed_two), alpha = 1, lambda = best_lambda_selected_two, parallel = TRUE)

# Subset the test data to include only the selected columns
X_test_selected_two <- testdata_actual[, selected_columns]

X_test_selected_imputed_two <- apply(X_test_selected_two, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
y_test_imputed_two <- testdata_actual$stars
y_test_imputed_two <- ifelse(is.na(y_test_imputed_two), mean(y_test_imputed_two, na.rm = TRUE), y_test_imputed_two)

# Predict on the test data 
lasso_predictions_selected_two <- predict(final_lasso_model_selected_two, newx = as.matrix(X_test_selected_imputed_two), s = best_lambda_selected_two, parallel = TRUE)

# Evaluate the model
mse_selected_two <- mean((lasso_predictions_selected_two - y_test_imputed_two)^2)

plot(lasso_predictions_selected_two)

lasso_coefficients_two <- coef(final_lasso_model_selected_two, s = best_lambda_selected_two)
print(lasso_coefficients_two)

#Lasso 3 business and user data based
library(glmnet)

# Including specified columns
selected_columns <- c("business_stars", "attributes.RestaurantsPriceRange2", "totalevaluation_attributes", "quintile_reviewcount", "average_stars")  

# Subset the training data to include only the selected columns
X_train_selected_three <- trainingdata_actual[, selected_columns]

X_train_selected_imputed_three <- apply(X_train_selected_three, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
y_train_imputed_three <- trainingdata_actual$stars
y_train_imputed_three <- ifelse(is.na(y_train_imputed_three), mean(y_train_imputed_three, na.rm = TRUE), y_train_imputed_three)

# Fit a LASSO model
lasso_model_selected_three <- cv.glmnet(x = as.matrix(X_train_selected_imputed_three), y = as.vector(y_train_imputed_three), alpha = 1, parallel = TRUE)

# Choose the lambda value
best_lambda_selected_three <- lasso_model_selected_three$lambda.min

# Fit the final LASSO model
final_lasso_model_selected_three <- glmnet(x = as.matrix(X_train_selected_imputed_three), y = as.vector(y_train_imputed_three), alpha = 1, lambda = best_lambda_selected_three, parallel = TRUE)

# Subset the test data to include only the selected columns
X_test_selected_three <- testdata_actual[, selected_columns]

X_test_selected_imputed_three <- apply(X_test_selected_three, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
y_test_imputed_three <- testdata_actual$stars
y_test_imputed_three <- ifelse(is.na(y_test_imputed_three), mean(y_test_imputed_three, na.rm = TRUE), y_test_imputed_three)

# Predict on the test data 
lasso_predictions_selected_three <- predict(final_lasso_model_selected_three, newx = as.matrix(X_test_selected_imputed_three), s = best_lambda_selected_three, parallel = TRUE)

# Evaluate the model
mse_selected_three <- mean((lasso_predictions_selected_three - y_test_imputed_three)^2)

plot(lasso_predictions_selected_three)

# Get the coefficients for the final LASSO model
lasso_coefficients_three <- coef(final_lasso_model_selected_three, s = best_lambda_selected_three)
print(lasso_coefficients_three)

#Lasso 4 business data based
library(glmnet)

# including specified columns
selected_columns <- c("business_stars", "attributes.RestaurantsPriceRange2", "totalevaluation_attributes", "quintile_reviewcount")  

# Subset the training data to include only the selected columns
X_train_selected_four <- trainingdata_actual[, selected_columns]

X_train_selected_imputed_four <- apply(X_train_selected_four, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
y_train_imputed_four <- trainingdata_actual$stars
y_train_imputed_four <- ifelse(is.na(y_train_imputed_four), mean(y_train_imputed_four, na.rm = TRUE), y_train_imputed_four)

# Fit a LASSO model
lasso_model_selected_four <- cv.glmnet(x = as.matrix(X_train_selected_imputed_four), y = as.vector(y_train_imputed_four), alpha = 1, parallel = TRUE)

# Choose the lambda value
best_lambda_selected_four <- lasso_model_selected_four$lambda.min

# Fit the final LASSO model
final_lasso_model_selected_four <- glmnet(x = as.matrix(X_train_selected_imputed_four), y = as.vector(y_train_imputed_four), alpha = 1, lambda = best_lambda_selected_four, parallel = TRUE)

# To include only the selected columns
X_test_selected_four <- testdata_actual[, selected_columns]

X_test_selected_imputed_four <- apply(X_test_selected_four, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
y_test_imputed_four <- testdata_actual$stars
y_test_imputed_four <- ifelse(is.na(y_test_imputed_four), mean(y_test_imputed_four, na.rm = TRUE), y_test_imputed_four)

# Predict on the imputed test data
lasso_predictions_selected_four <- predict(final_lasso_model_selected_four, newx = as.matrix(X_test_selected_imputed_four), s = best_lambda_selected_four, parallel = TRUE)

# Evaluate the model
mse_selected_four <- mean((lasso_predictions_selected_four - y_test_imputed_four)^2)

plot(lasso_predictions_selected_four)

# Get the coefficients for the final LASSO model
lasso_coefficients_four <- coef(final_lasso_model_selected_four, s = best_lambda_selected_four)
print(lasso_coefficients_four)

# Decision tree 1

target_variable <- "stars"
library(tree)

# Construct the formula using paste
formula_str <- paste(target_variable, "~ .")

# Train a decision tree model on the training data
model <- tree(as.formula(formula_str), data = trainingdata_actual)

plot(model)
text(model, pretty = 1)
title(main = "Decision Tree for Training Data")

# Predict on the test data
predictions <- predict(model, newdata = testdata_actual, type = "vector")  

# Convert both vectors to factors
predictions <- factor(predictions)
testdata_actual$stars <- factor(testdata_actual$stars)

# Create a data frame with both vectors
df <- data.frame(predictions, testdata_actual$stars)

contingency_table <- table(df)
accuracy <- sum(diag(contingency_table)) / sum(contingency_table)
cat("Accuracy:", accuracy, "\n")

print(contingency_table)

plot(model)
text(model, pretty = 1)
title(main = "Decision Tree for Test Data")

# Decision Tree 2 review data based
target_variable <- "stars"
selected_columns <- c("useful", "funny", "cool")

# Train a decision tree model on the training data using specific columns
model_two <- tree(as.formula(paste(target_variable, "~", paste(selected_columns, collapse = " + "))), data = trainingdata_actual)

# Plot the decision tree
plot(model_two)
text(model_two, pretty = 1)
title(main = "Decision Tree for Training Data 2")

# Predict on the test data
predictions_two <- predict(model_two, newdata = testdata_actual, type = "vector")  

# Convert both vectors to factors
predictions_two <- factor(predictions_two)
testdata_actual$stars <- factor(testdata_actual$stars)

# Create a data frame with both vectors
df_two <- data.frame(predictions_two, testdata_actual$stars)

contingency_table_two <- table(df_two)
accuracy_two <- sum(diag(contingency_table_two)) / sum(contingency_table_two)
cat("Accuracy:", accuracy, "\n")

print(contingency_table_two)

plot(model_two)
text(model_two, pretty = 1)
title(main = "Decision Tree for Test Data 2")

# Decision Tree 3 business and user data based
target_variable <- "stars"
selected_columns <- c("business_stars", "attributes.RestaurantsPriceRange2", "totalevaluation_attributes", "quintile_reviewcount", "average_stars")

# Train a decision tree model on the training data using specific columns
model_three <- tree(as.formula(paste(target_variable, "~", paste(selected_columns, collapse = " + "))), data = trainingdata_actual)

# Plot the decision tree
plot(model_three)
text(model_three, pretty = 1)
title(main = "Decision Tree for Training Data 3")

# Predict on the test data
predictions_three <- predict(model_three, newdata = testdata_actual, type = "vector")  

# Convert both vectors to factors
predictions_three <- factor(predictions_three)
testdata_actual$stars <- factor(testdata_actual$stars)

# Create a data frame with both vectors
df_three <- data.frame(predictions_three, testdata_actual$stars)

contingency_table_three <- table(df_three)
accuracy_three <- sum(diag(contingency_table_three)) / sum(contingency_table_three)
cat("Accuracy:", accuracy, "\n")

print(contingency_table_three)

plot(model_three)
text(model_three, pretty = 1)
title(main = "Decision Tree for Test Data 3")

# Decision Tree 4 business data based
target_variable <- "stars"
selected_columns <- c("business_stars", "attributes.RestaurantsPriceRange2", "totalevaluation_attributes", "quintile_reviewcount")

# Train a decision tree model on the training data using specific columns
model_four <- tree(as.formula(paste(target_variable, "~", paste(selected_columns, collapse = " + "))), data = trainingdata_actual)

# Plot the decision tree
plot(model_four)
text(model_four, pretty = 1)
title(main = "Decision Tree for Training Data 4")

# Predict on the test data
predictions_four <- predict(model_four, newdata = testdata_actual, type = "vector")  

# Convert both vectors to factors
predictions_four <- factor(predictions_four)
testdata_actual$stars <- factor(testdata_actual$stars)

# Create a data frame with both vectors
df_four <- data.frame(predictions_four, testdata_actual$stars)

contingency_table_four <- table(df_four)
accuracy_four <- sum(diag(contingency_table_four)) / sum(contingency_table_four)
cat("Accuracy:", accuracy, "\n")

print(contingency_table_four)

plot(model_four)
text(model_four, pretty = 1)
title(main = "Decision Tree for Test Data 4")