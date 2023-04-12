library("dslabs")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("gtools")
library("lubridate")
library("Lahman")
library("broom")
library("caret")
library("gam")
library("rpart")
library("rpart.plot")
library("tinytex")
library("recommenderlab")
library("reshape2")
library("recosystem")
library("data.table")
library("readr")
library("httr")
library("readxl")
library("corrplot")
library("dendextend")
library("randomForest")
library("pROC")

# First Data Inspection - First Glance at the Data---------------------------

# Load the CSV files into data frames
training <- read_csv("Training.csv")
testing <- read_csv("Testing.csv")

#have a first look into the data.
head(training)

# First 5 column names
colnames(training)[1:5]

# Structure of the first 5 columns
str(training[, 1:5])

# Summary statistics of the first 5 variables
summary(training[, 1:5])

# Check for missing values
cat("Sum of Missing values in each column:\n")
sum(colSums(is.na(training)))

training[, 134]
training <- training %>% select(-134)

# Select first 300 values of prognosis column and create frequency table
table(training$prognosis[1:300])
length(unique(training$prognosis))

#Visualization------------------------------------------------------------

# Visualize the frequency distribution of diseases
#Note: Axis angle is adjusted to make it easier to read.
ggplot(training, aes(x=prognosis)) + 
  geom_bar(fill = "#0073C2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Frequency Distribution of Diseases", x="Disease", y="Frequency")

#Most prevalent symptoms for some diseases----------------------------------

# Get top 2 and last 2 diseases
top_diseases <- training %>%
  count(prognosis, sort = TRUE) %>%
  slice(c(head(row_number(), 2), tail(row_number(), 2))) %>%
  pull(prognosis)

# Filter the tibble for the top 2 and last 2 diseases
filtered_tibble <- training %>%
  filter(prognosis %in% top_diseases)

# Calculate the prevalence of each symptom for each disease
symptom_prevalence <- filtered_tibble %>%
  group_by(prognosis) %>%
  summarise(across(itching:blackheads, \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = -prognosis, names_to = "symptom", values_to = "prevalence")

# Get top 6 prevalent symptoms for each disease
top_symptoms_per_disease <- symptom_prevalence %>%
  group_by(prognosis) %>%
  arrange(prognosis, desc(prevalence)) %>%
  slice_head(n = 6)

# Visualize the top 6 prevalent symptoms 
#for each of the top 2 and last 2 diseases using facet_wrap
top_symptoms_per_disease %>%
  ggplot(aes(x = symptom, y = prevalence, fill = symptom)) +
  geom_col() +
  facet_wrap(~prognosis, scales = "free", ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), strip.text = element_text(size = 6)) +
  labs(title = "Top 6 Prevalent Symptoms for Each of the Top 2 and Last 2 Diseases",
       x = "Prevalence",
       y = "Symptom")

# Aggregate the data to calculate the frequency of each symptom for each disease
symptoms_vs_diseases <- training %>%
  group_by(prognosis) %>%
  summarise(across(itching:blackheads, sum)) %>%
  pivot_longer(cols = -prognosis, names_to = "Symptom", values_to = "Frequency") %>% 
  group_by(prognosis) %>% 
  mutate(Frequency = Frequency/sum(Frequency))

# Split the symptoms into two halves
symptoms <- unique(symptoms_vs_diseases$Symptom)
half <- floor(length(symptoms) / 2)
first_half_symptoms <- symptoms[1:half]
second_half_symptoms <- symptoms[(half + 1):length(symptoms)]

# Filter the data for the first half of the symptoms
first_half_data <- symptoms_vs_diseases %>% filter(Symptom %in% first_half_symptoms)

# Create the heatmap for the first half of the symptoms
heatmap_firsHalf <- ggplot(first_half_data, aes(x = Symptom,
                                                y = prognosis, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "firebrick") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Heatmap of Normalized Symptoms Frequencies vs Diseases (First Half)",
       fill = "Frequency")
heatmap_firsHalf

# Filter the data for the second half of the symptoms
second_half_data <- symptoms_vs_diseases %>% filter(Symptom %in% second_half_symptoms)

# Create the heatmap for the second half of the symptoms
heatmap_secondHalf <- ggplot(second_half_data, aes(x = Symptom, 
                                                   y = prognosis,
                                                   fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "firebrick") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Heatmap of Normalized Symptoms Frequencies vs Diseases (Second Half)",
       fill = "Frequency")

heatmap_secondHalf


#Correlation plot of the symptoms---------------------------------------------

# Select only the symptoms columns
symptoms_data <- training %>%
  select(itching:blackheads)

# Calculate the midpoint for splitting the symptoms data
midpoint <- ceiling(ncol(symptoms_data) / 2)

# Split the data into two halves
symptoms_data_half1 <- symptoms_data[, 1:midpoint]
symptoms_data_half2 <- symptoms_data[, (midpoint + 1):ncol(symptoms_data)]

# Calculate the correlation matrices for each half.
#We use the cor() function from the 
correlation_matrix_half1 <- cor(symptoms_data_half1)
correlation_matrix_half2 <- cor(symptoms_data_half2)

# Convert the correlation matrices to long format data frames
correlation_df_half1 <- correlation_matrix_half1 %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable1") %>%
  pivot_longer(cols = -Variable1, names_to = "Variable2", 
               values_to = "Correlation")

correlation_df_half2 <- correlation_matrix_half2 %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable1") %>%
  pivot_longer(cols = -Variable1, names_to = "Variable2", 
               values_to = "Correlation")

# Create heatmaps for each half
heatmap_plot_half1 <- ggplot(correlation_df_half1, aes(x = Variable1, 
                                                       y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_fixed(ratio = 1) +
  labs(title = "Multicollinearity Heatmap of Symptoms (Half 1)",
       fill = "Correlation")

heatmap_plot_half2 <- ggplot(correlation_df_half2, aes(x = Variable1,
                                                       y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_fixed(ratio = 1) +
  labs(title = "Multicollinearity Heatmap of Symptoms (Half 2)",
       fill = "Correlation")

# Display the heatmaps
heatmap_plot_half1
heatmap_plot_half2

# Remove the fluid_overload column from the training dataset
training <- training[, -46]



#New train data simplifying symptoms ------------------------------------------
#combining symptoms with more than 0.9 of correlation


symptoms_correlation <- cor(symptoms_data, use = "pairwise.complete.obs")


# Calculate the correlation matrix for the entire symptoms_data
correlation_matrix <- cor(symptoms_data)

# Find pairs of symptoms with a correlation higher than 0.9
high_correlation_pairs <- which(correlation_matrix > 0.9 & correlation_matrix < 1, arr.ind = TRUE)
high_correlation_pairs <- unique(t(apply(high_correlation_pairs, 1, sort)))

# Create an empty data frame to store the new combined features
new_features <- data.frame(matrix(ncol = 0, nrow = nrow(symptoms_data)))

# Perform PCA for each group of highly correlated symptoms and create new features
for (i in 1:nrow(high_correlation_pairs)) {
  symptoms_pair <- colnames(symptoms_data)[high_correlation_pairs[i, ]]
  symptoms_subset <- symptoms_data[, symptoms_pair]
  
  # Combine the correlated symptoms with an OR operation (presence of either symptom)
  combined_symptom <- symptoms_subset[, 1] | symptoms_subset[, 2]
  combined_symptom <- as.integer(combined_symptom)
  
  # Create new feature name based on the combined symptoms
  new_feature_name <- paste(symptoms_pair, collapse = "_")
  
  new_features <- cbind(new_features, setNames(list(combined_symptom), new_feature_name))
}

# Remove the original symptoms from the dataset
reduced_symptoms_data <- symptoms_data %>% 
  select(-colnames(symptoms_data)[unique(c(high_correlation_pairs))])

# Add the new combined features to the dataset
reduced_symptoms_data <- cbind(reduced_symptoms_data, new_features)

# Add the prognosis column to the reduced_symptoms_data
reduced_symptoms_data$prognosis <- training$prognosis

ncol(reduced_symptoms_data)

# Machine Learning Models - Analysis __________________________________________

set.seed(1)
# Split the data into training (80%) and testing (20%) sets
train_indices <- createDataPartition(training$prognosis, p = 0.8, 
                                     list = FALSE)
train_data <- training[train_indices, ]
test_data <- training[-train_indices, ]

# Split the data into training (80%) and testing (20%) sets
train_indices_reduced <- createDataPartition(reduced_symptoms_data$prognosis, p = 0.8, 
                                             list = FALSE)
train_data_reduced <- reduced_symptoms_data[train_indices_reduced, ]
test_data_reduced <- reduced_symptoms_data[-train_indices_reduced, ]

# Convert prognosis to factor
train_data$prognosis <- factor(train_data$prognosis)
test_data$prognosis <- factor(test_data$prognosis, levels = levels(train_data$prognosis))

# Fix variable names
names(train_data) <- make.names(names(train_data), unique = TRUE)
names(test_data) <- make.names(names(test_data), unique = TRUE)

# Decision Tree---------------------------------------------------------
dt_model <- rpart(prognosis ~ ., data = train_data)
dt_pred <- predict(dt_model, test_data, type = "class")
dt_cm <- confusionMatrix(dt_pred, test_data$prognosis)
dt_accuracy <- dt_cm$overall['Accuracy']

# Random Forest--------------------------------------------------------
rf_model <- randomForest(prognosis ~ ., data = train_data)
rf_pred <- predict(rf_model, test_data)
rf_cm <- confusionMatrix(rf_pred, test_data$prognosis)
rf_accuracy <- rf_cm$overall['Accuracy']

#Navie bayes method
# Set up the training control
trControl <- trainControl(method = "cv", number = 10)
# Train Naive Bayes model using train() function
nb_model <- train(prognosis ~ ., data = train_data, 
                  method = "naive_bayes", trControl = trControl)
# Predict on test set
nb_pred <- predict(nb_model, test_data)
# Compute confusion matrix
nb_cm <- confusionMatrix(nb_pred, test_data$prognosis)
nb_accuracy <- nb_cm$overall['Accuracy']

# kNN-----------------------------------------------------------------------
k_values <- seq(1, 40, 1)
accuracy_results <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  knn_model <- knn3(prognosis ~ ., data = train_data, k = k_values[i])
  knn_pred <- predict(knn_model, test_data, type = "class")
  knn_cm <- confusionMatrix(knn_pred, test_data$prognosis)
  accuracy_results[i] <- knn_cm$overall['Accuracy']
}
# Find the optimal k-value
optimal_k <- k_values[which.max(accuracy_results)]
print(paste("Optimal k-value:", optimal_k))

knn_pred <- predict(knn_model, test_data, type = "class")
knn_cm <- confusionMatrix(knn_pred, test_data$prognosis)
knn_accuracy <- knn_cm$overall['Accuracy']

# Function to calculate the mean of specificity and sensitivity
mean_specificity_sensitivity <- function(cm) {
  specificity <- mean(cm$byClass[,"Specificity"], na.rm = TRUE)
  sensitivity <- mean(cm$byClass[,"Sensitivity"], na.rm = TRUE)
  return(c(specificity, sensitivity))
}

# Function to calculate the mean F1 score
mean_f1_score <- function(cm) {
  precision <- cm$byClass[,"Pos Pred Value"]
  recall <- cm$byClass[,"Sensitivity"]
  f1 <- 2 * (precision * recall) / (precision + recall)
  return(mean(f1, na.rm = TRUE))
}
# Results are stored in the "results table" by calling elements of the
# Confusion Matrix.

results <- data.frame(
  Model = c("Decision Tree", "Random Forest", "Navie Bayies", "kNN"),
  Accuracy = c(dt_accuracy, rf_accuracy, nb_accuracy, knn_accuracy),
  F1_Score = c(
    mean_f1_score(dt_cm), mean_f1_score(rf_cm),
    mean_f1_score(nb_cm), mean_f1_score(knn_cm)
  ),
  Specificity = c(
    mean_specificity_sensitivity(dt_cm)[1],
    mean_specificity_sensitivity(rf_cm)[1],
    mean_specificity_sensitivity(nb_cm)[1],
    mean_specificity_sensitivity(knn_cm)[1]
  ),
  Sensitivity = c(
    mean_specificity_sensitivity(dt_cm)[2],
    mean_specificity_sensitivity(rf_cm)[2],
    mean_specificity_sensitivity(nb_cm)[2],
    mean_specificity_sensitivity(knn_cm)[2]
  )
)

print(results)

#Visualization - Inspection of the Generated Models ___________________________

# Visualize Random Forest (Variable Importance)
varImpPlot(rf_model)

#ROC Plot for the rf model

# Predict probabilities for the test data
pred_probs <- predict(rf_model, newdata = test_data, type = "prob")

# Extract the probabilities for the positive class
positive_probs <- pred_probs[, 2]

# Calculate the ROC curve and AUC
roc_obj <- roc(test_data$prognosis, positive_probs)
auc <- auc(roc_obj)

# Plot the ROC curve
ROC_rf <- plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc, 2), ")"))


#knn visualization
knn_values_accuracy <- plot(k_values, accuracy_results, type = "l", 
                            xlab = "k", ylab = "Accuracy", main = "kNN Accuracy vs k Value")
abline(v = optimal_k, col = "red", lty = 2)


# Final Validation - Results Calculation

# Predict on the testing dataset

# Convert prognosis to factor
testing$prognosis <- factor(testing$prognosis)
testing$prognosis <- factor(testing$prognosis, levels = levels(testing$prognosis))

# Fix variable names
names(testing) <- make.names(names(testing), unique = TRUE)
names(testing) <- make.names(names(testing), unique = TRUE)

# Calculation of predictions on "testing" data.

dt_pred_testing <- predict(dt_model, testing, type = "class")
dt_cm_testing <- confusionMatrix(dt_pred_testing, testing$prognosis)
dt_accuracy_testing <- dt_cm_testing$overall['Accuracy']

rf_pred_testing <- predict(rf_model, testing)
rf_cm_testing <- confusionMatrix(rf_pred_testing, testing$prognosis)
rf_accuracy_testing <- rf_cm_testing$overall['Accuracy']

nb_pred_testing <- predict(nb_model, testing)
nb_cm_testing <- confusionMatrix(nb_pred_testing, testing$prognosis)
nb_accuracy_testing <- nb_cm_testing$overall['Accuracy']

knn_pred_testing <- predict(knn_model, testing, type = "class")
knn_cm_testing <- confusionMatrix(knn_pred_testing, testing$prognosis)
knn_accuracy_testing <- knn_cm_testing$overall['Accuracy']

# Create the results table for the testing dataset
results_testing <- data.frame(
  Model = c("Decision Tree", "Random Forest", "Naive Bayes", "kNN"),
  Accuracy = c(
    dt_accuracy_testing, rf_accuracy_testing,
    nb_accuracy_testing, knn_accuracy_testing
  ),
  F1_Score = c(
    mean_f1_score(dt_cm_testing), mean_f1_score(rf_cm_testing),
    mean_f1_score(nb_cm_testing), mean_f1_score(knn_cm_testing)
  ),
  Specificity = c(
    mean_specificity_sensitivity(dt_cm_testing)[1],
    mean_specificity_sensitivity(rf_cm_testing)[1],
    mean_specificity_sensitivity(nb_cm_testing)[1],
    mean_specificity_sensitivity(knn_cm_testing)[1]
  ),
  Sensitivity = c(
    mean_specificity_sensitivity(dt_cm_testing)[2],
    mean_specificity_sensitivity(rf_cm_testing)[2],
    mean_specificity_sensitivity(nb_cm_testing)[2],
    mean_specificity_sensitivity(knn_cm_testing)[2]
  )
)
print(results_testing)


#top 5 diseases per observation using the nb_______________________________________

# Create an empty list with the same length as rand_obs to store the results
random_samples <- 5

# Randomly select observations from the training data
set.seed(123)
rand_obs <- sample(1:nrow(testing), random_samples)

# Create an empty list with the same length as random_samples to store the results
results <- vector("list", length(rand_obs))

# Loop through each random observation
for (i in 1:random_samples) {
  
  # Predict the probabilities of each disease given the symptoms using the Naive Bayes model
  probs <- predict(nb_model, newdata = testing[rand_obs[i],], type = "prob")
  
  # Sort the probabilities in descending order and select the top 5 diseases
  top_diseases <- names(sort(probs, decreasing = TRUE))[1:5]
  top_probs <- sort(probs, decreasing = TRUE)[1:5]
  
  # Store the results in a list
  results[[i]] <- list(top_diseases = top_diseases, top_probs = top_probs)
}

# Print the results
for (i in 1:length(results)) {
  cat("\nObservation", rand_obs[i], ":\n")
  cat("Top diseases:", results[[i]]$top_diseases, "\n")
  cat("Top probabilities:", paste(round(results[[i]]$top_probs, 14), collapse = ", "), "\n")
}


