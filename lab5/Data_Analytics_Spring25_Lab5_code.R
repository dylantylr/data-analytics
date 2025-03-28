library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(class)

wine <- read.csv("C:/Users/taylod9/Downloads/lab5/wine/wine.data", header = FALSE)
colnames(wine) <- c("Type", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash",
                    "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
                    "Proanthocyanins", "Color_Intensity", "Hue", "OD280_OD315", "Proline")

wine$Type <- as.factor(wine$Type)

# Split the dataset into training and testing sets
set.seed(123)

wine_index <- sample(nrow(wine), 0.75 * nrow(wine))
wine_train <- wine[wine_index, ]
wine_test <- wine[-wine_index, ]

features <- c("Alcohol", "Flavanoids", "Hue")
wine_formula <- as.formula(paste("Type ~", paste(features, collapse = " + ")))

# ---- SVM with Linear Kernel ----
svm_linear <- svm(wine_formula, data = wine_train, kernel = "linear")
pred_linear <- predict(svm_linear, wine_test)
cat("SVM Linear Kernel Results:\n")
print(confusionMatrix(pred_linear, wine_test$Type))

# ---- SVM with Radial Kernel + tune ----
tune_result <- tune.svm(wine_formula, data = wine_train, kernel = "radial",
                        cost = 10^(-1:2), gamma = c(0.5, 1, 2))
svm_radial <- tune_result$best.model
pred_radial <- predict(svm_radial, wine_test)
cat("\nSVM Radial Kernel Results:\n")
print(confusionMatrix(pred_radial, wine_test$Type))

# ---- kNN Classifier ----
wine_train_knn <- wine_train[, features]
wine_test_knn <- wine_test[, features]
knn_pred <- knn(train = wine_train_knn, test = wine_test_knn,
                cl = wine_train$Type, k = 3)
cat("\nkNN Classifier Results:\n")
print(confusionMatrix(as.factor(knn_pred), wine_test$Type))
# ---- Performance Comparison ----
cat("\nPerformance Comparison:\n")
cat("SVM Linear Kernel:\n")
print(confusionMatrix(pred_linear, wine_test$Type)$byClass[, c("Precision", "Recall", "F1")])
cat("\nSVM Radial Kernel:\n")
print(confusionMatrix(pred_radial, wine_test$Type)$byClass[, c("Precision", "Recall", "F1")])
cat("\nkNN Classifier:\n")
print(confusionMatrix(as.factor(knn_pred), wine_test$Type)$byClass[, c("Precision", "Recall", "F1")])





#Part 2
# Load housing data
NY_House_Dataset <- read_csv("C:/Users/taylod9/Downloads/lab5/NY-House-Dataset.csv")
dataset <- NY_House_Dataset

# Split the dataset into training and testing sets
set.seed(123)
train.indexes <- sample(nrow(dataset), 0.75 * nrow(dataset))
train <- dataset[train.indexes, ]
test <- dataset[-train.indexes, ]


x <- dataset[,2:4] 
y <- as.factor(as.matrix(dataset[,1]))

## feature boxplots
boxplot(X, main="iris features")

## class label distributions
plot(Y)

## feature-class plots
featurePlot(x=x, y=y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

ggplot(dataset, aes(x = Alcohol, y = Ash, colour = Type)) +
  geom_point()

svr.mod0 <- svm(PRICE ~ PROPERTYSQFT, train)

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)

svr.pred <- cbind(test$PRICE,svr.pred)

ggplot(svr.pred, aes(x = V1, y = svr.pred)) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod0 <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset, kernel="radial")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, dataset)

svr.outs <- data.frame(real=log10(dataset$PRICE), pred=svr.pred)

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod0 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset)

summary(svr.mod0)

svr.pred <- predict(svr.mod0, dataset)

svr.outs <- data.frame(real=log10(dataset$PRICE), pred=svr.pred)

ggplot(svr.outs, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm")


# plot predicted price vs. real price.

ggplot(svr.outs, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm") +
  xlab("Real Price") +
  ylab("Predicted Price") +
  ggtitle("Predicted vs Real Price")



# Calculate residuals for both models
residuals_svr <- svr.outs$real - svr.outs$pred
residuals_lm <- dataset$PRICE - 10^predict(svr.mod0, dataset)

# Create data frames for plotting
residuals_data <- data.frame(
  Model = c(rep("SVR", length(residuals_svr)), rep("Linear Model", length(residuals_lm))),
  Residuals = c(residuals_svr, residuals_lm)
)

# Plot residuals
ggplot(residuals_data, aes(x = Residuals, fill = Model)) +
  geom_histogram(binwidth = 0.05, alpha = 0.6, position = "identity") +
  facet_wrap(~ Model, scales = "free") +
  xlab("Residuals") +
  ylab("Frequency") +
  ggtitle("Residual Plots for SVR and Linear Model") +
  theme_minimal()







