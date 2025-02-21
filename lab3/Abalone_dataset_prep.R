###################
##### Abalone #####
###################

# read dataset
abalone <- read.csv("C:/Users/taylod9/Downloads/abalone_dataset.csv")

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"

library(class)

#split data into training and testing sets
set.seed(123)
trainIndex <- sample(1:nrow(dataset), 0.7*nrow(dataset))
train <- dataset[trainIndex,]
test <- dataset[-trainIndex,]

#model 1
#kNN model using all features
model1 <- knn(train[,c("length", "diameter", "height")], test[,c("length", "diameter", "height")], train$age.group, k=5)
table(test$age.group, model1)

#model 2
#kNN model using only length and diameter
model2 <- knn(train[,c("whole_weight", "shucked_wieght", "shell_weight")], test[,c("whole_weight", "shucked_wieght", "shell_weight")], train$age.group, k=5)
table(test$age.group, model2)

#contigeny tables
model1_table <- table(test$age.group, model1)
model2_table <- table(test$age.group, model2)

print(model1_table)
print(model2_table)

#calculate accuracy for both
model1_accuracy <- sum(diag(model1_table))/sum(model1_table)
model2_accuracy <- sum(diag(model2_table))/sum(model2_table)

print(model1_accuracy)
print(model2_accuracy)

k_values <- 1:20
accuracies <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  model <- knn(train[,c("whole_weight", "shucked_wieght", "shell_weight")], test[,c("whole_weight", "shucked_wieght", "shell_weight")], train$age.group, k=k)
  model_table <- table(test$age.group, model)
  accuracies[i] <- sum(diag(model_table)) / sum(model_table)
}

optimal_k <- k_values[which.max(accuracies)]
print("Optimal k value for Model 2: ", optimal_k, "\n")

library(ggplot2)


# Best performing feature subset from Exercise 1
best_features <- c("whole_weight", "shucked_wieght", "shell_weight")

# Function to calculate total within-cluster sum of squares
wss <- function(data, k) {
  kmeans(data, k, nstart = 10)$tot.withinss
}

k_values <- 1:10
wss_values <- numeric(length(k_values))

for (k in k_values) {
  wss_values[k] <- wss(dataset[, best_features], k)
}

plot(k_values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")


optimal_k <- which.min(diff(diff(wss_values)))


set.seed(123)
kmeans_model <- kmeans(dataset[, best_features], centers = optimal_k, nstart = 10)

# Add cluster assignment to the dataset
dataset$cluster <- as.factor(kmeans_model$cluster)

# Plot the assigned clusters for 2 of the features
ggplot(dataset, aes(x = whole_weight, y = shucked_wieght, color = cluster)) +
  geom_point() +
  labs(title = paste("K-Means Clustering with K =", optimal_k),
       x = "Whole Weight",
       y = "Shucked Weight") +
  theme_minimal()