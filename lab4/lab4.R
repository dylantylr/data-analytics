# Load data
wine <- read.csv("C:/Users/taylod9/Downloads/wine/wine.data", header = FALSE)
colnames(wine) <- c("Type", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash",
                    "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
                    "Proanthocyanins", "Color_Intensity", "Hue", "OD280_OD315", "Proline")


library(ggplot2)
library(caret)
library(class)
library(dplyr)
library(pROC)
# Standardize the data 
wine_scaled <- scale(wine[, -1])
# Perform PCA
pca_result <- prcomp(wine_scaled, center = TRUE, scale. = TRUE)

summary(pca_result)
# Plot the PCA results using the first two principal components
pca_data <- as.data.frame(pca_result$x)
pca_data$Type <- wine$Type
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Type))) +
  geom_point(size = 3) +
  labs(title = "PCA of Wine Dataset", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()
# Identify the variables that contribute the most to the 1st PC
loadings <- pca_result$rotation[, 1]
contributions <- abs(loadings)
contributions <- sort(contributions, decreasing = TRUE)
top_contributors <- names(contributions)[1:5]
top_contributors

# Drop the variables least contributing to the 1st PC
wine_reduced <- wine[, c("Type", top_contributors)]
wine_reduced_scaled <- scale(wine_reduced[, -1])
# Perform PCA again on the reduced dataset
pca_result_reduced <- prcomp(wine_reduced_scaled, center = TRUE, scale. = TRUE)
# Plot the PCA results using the first two principal components of the reduced dataset
pca_data_reduced <- as.data.frame(pca_result_reduced$x)
pca_data_reduced$Type <- wine$Type
ggplot(pca_data_reduced, aes(x = PC1, y = PC2, color = as.factor(Type))) +
  geom_point(size = 3) +
  labs(title = "PCA of Reduced Wine Dataset", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()


set.seed(123)
train_index <- createDataPartition(wine$Type, p = 0.8, list = FALSE)

# Original data: scaled features
train_data <- wine_scaled[train_index, ]
test_data <- wine_scaled[-train_index, ]
train_labels <- wine$Type[train_index]
test_labels <- wine$Type[-train_index]

# kNN on original dataset
knn_model <- knn(train = train_data, test = test_data, cl = train_labels, k = 5)

# Confusion matrix for original dataset
confusion_matrix_original <- confusionMatrix(factor(knn_model), factor(test_labels))
precision_original <- confusion_matrix_original$byClass[, "Pos Pred Value"]
recall_original <- confusion_matrix_original$byClass[, "Sensitivity"]
f1_score_original <- 2 * (precision_original * recall_original) / (precision_original + recall_original)

# PCA data: use first 3 PCs
train_data_pca <- as.data.frame(pca_result$x[train_index, 1:3])
test_data_pca <- as.data.frame(pca_result$x[-train_index, 1:3])
train_labels <- wine$Type[train_index]
test_labels <- wine$Type[-train_index]

# kNN on PCA-reduced dataset
knn_model_pca <- knn(train = train_data_pca, test = test_data_pca, cl = train_labels, k = 5)

# Confusion matrix for PCA dataset
confusion_matrix_pca <- confusionMatrix(factor(knn_model_pca), factor(test_labels))
precision_pca <- confusion_matrix_pca$byClass[, "Pos Pred Value"]
recall_pca <- confusion_matrix_pca$byClass[, "Sensitivity"]
f1_score_pca <- 2 * (precision_pca * recall_pca) / (precision_pca + recall_pca)

# Compare the two classification models using averaged metrics
comparison_table <- data.frame(
  Metric = c("Precision", "Recall", "F1 Score"),
  Original = c(mean(precision_original, na.rm = TRUE),
               mean(recall_original, na.rm = TRUE),
               mean(f1_score_original, na.rm = TRUE)),
  PCA = c(mean(precision_pca, na.rm = TRUE),
          mean(recall_pca, na.rm = TRUE),
          mean(f1_score_pca, na.rm = TRUE))
)

# Print the comparison table
print("Comparison of classification models:")
print(comparison_table)
