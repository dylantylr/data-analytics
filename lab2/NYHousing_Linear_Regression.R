library("ggplot2")

## read dataset
NY_House_Dataset <- read.csv("C:/Users/taylod9/Downloads/NY-House-Dataset.csv")

dataset <- NY_House_Dataset


# data cleaning
dataset <- dataset[complete.cases(dataset),]

# model 1
model1 <- lm(PRICE ~ PROPERTYSQFT + BEDS + BATH, data = dataset)
summary(model1)

# model 2
model2 <- lm(PRICE ~ PROPERTYSQFT + BEDS, data = dataset)
summary(model2)

# model 3
model3 <- lm(PRICE ~ PROPERTYSQFT + BATH, data = dataset)
summary(model3)

# plots
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# scatter plot of the residuals for the model
ggplot(dataset, aes(x = fitted(model1), y = residuals(model1))) +
  geom_point() +
  geom_hline(yintercept = 0)

ggplot(dataset, aes(x = fitted(model2), y = residuals(model2))) +
  geom_point() +
  geom_hline(yintercept = 0)

ggplot(dataset, aes(x = fitted(model3), y = residuals(model3))) +
  geom_point() +
  geom_hline(yintercept = 0)


