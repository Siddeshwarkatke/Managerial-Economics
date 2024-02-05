
rm(list = ls())

library(ISLR2)
library(tidyverse)
# install.packages("car")
library(car)


# 1. Collinearity

Credit <- Credit

model.1 <- lm(Balance ~ . , Credit)
summary(model.1)

# variance inflation factor
vif(model.1) # in car package

plot(Credit$Limit,Credit$Rating)

model.1.1 <- lm(Limit ~ Rating, Credit)
summary(model.1.1)

model.1.2 <- lm(Balance ~ . - Limit, Credit)
summary(model.1.2)

library(texreg)
screenreg(list(model.1,model.1.2))

# 2. Outliers

summary(Credit$Limit)
plot(Credit$Age,Credit$Limit)

model.2 <- lm(Limit ~ Age + Rating, Credit)
summary(model.2)

Credit2 <- Credit %>% filter(Limit <= 12000)
model.2.1 <- lm(Limit ~ Age + Rating, Credit2)
summary(model.2.1)

# 3. Subset selection

# install.packages("leaps")
library(leaps)

Hitters <- Hitters
 
sum(is.na(Hitters)) # count NAs

Hitters <- na.omit(Hitters) # remove NAs

# forward, backward, and exhaustive selection
model.bf.for <- regsubsets(Salary ~ ., data = Hitters, method = "forward")
summary(model.bf.for)

model.bf.back  <- regsubsets(Salary ~ ., data = Hitters, method = "backward")
summary(model.bf.back)

model.bf.ex  <- regsubsets(Salary ~ ., data = Hitters, method = "exhaustive")
summary(model.bf.ex)
bf.summary <- summary(model.bf.ex)

# get Rsq for each size model
bf.summary$rsq

# increase max number variables included
model.bf.ex  <- regsubsets(Salary ~ ., data = Hitters, method = "exhaustive", nvmax = 15)
summary(model.bf.ex)
bf.summary <- summary(model.bf.ex)
bf.summary$rsq

# look at coefficients
coef(model.bf.for,5)
coef(model.bf.back,5)
coef(model.bf.ex,5)

# plot results
par(mfrow = c(2, 2))
plot(bf.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(bf.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
max.adjrs <- which.max(bf.summary$adjr2)
points(max.adjrs, bf.summary$adjr2[max.adjrs], col = "red", cex = 2, pch = 20)

plot(bf.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp <- which.min(bf.summary$cp)
points(min.cp, bf.summary$cp[min.cp], col = "red", cex = 2, pch = 20)

plot(bf.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic <- which.min(bf.summary$bic)
points(min.bic, bf.summary$bic[min.bic], col = "red", cex = 2, pch = 20)

# Q1. Using the Credit data set, determine the 3 best covariates for estimating the Balance variable.
# Do this using backward, forward, and exhaustive selection methods. 
# What are their respective R-sq values?

library(leaps)

response_variable <- "Balance"

# Forward selection 
forward_model <- regsubsets(Balance ~ ., data = Credit, method = "forward")

# Backward selection 
backward_model <- regsubsets(Balance ~ ., data = Credit, method = "backward")

#Exhaustive Selection 
exhaustive_model <- regsubsets(Balance ~ ., data = Credit, method = "exhaustive")

get_best_variables <- function(model, data) {
  best_vars <- names(coef(model, id = which.max(summary(model)$rsq)))
  best_rsq <- summary(model)$rsq[which.max(summary(model)$rsq)]
  return(list(variables = best_vars, rsquared = best_rsq))
}

forward_result <- get_best_variables(forward_model, Credit)
backward_result <- get_best_variables(backward_model, Credit)
exhaustive_result <- get_best_variables(exhaustive_model, Credit)

cat("Forward Selection: \n")
cat("Selected Variables: ", forward_result$variables, "\n")
cat("R-squared: ", forward_result$rsquared, "\n\n")

cat("Backward Selection: \n")
cat("Selected Variables: ", backward_result$variables, "\n")
cat("R-squared: ", backward_result$rsquared, "\n\n")

cat("Exhaustive Selection: \n")
cat("Selected Variables: ", exhaustive_result$variables, "\n")
cat("R-squared: ", exhaustive_result$rsquared, "\n")


#Forward selection R-squared is 0.954888
#Backward selection R-squared is 0.954888 
#Exhaustive selection R-squared is  0.954888


# 4. Ridge Regression

library(glmnet)
grid <- 10^seq(10, -2, length = 100)
grid

x <- model.matrix(Salary ~ ., Hitters)[, -1] 
y <- Hitters$Salary

model.ridge <- glmnet(x, y, alpha = 0, lambda = grid) # standardized ridge model

coef(model.ridge)[,100]
coef(model.ridge)[,50]
coef(model.ridge)[,1]

plot(model.ridge, label = TRUE)

# Q2. Using the Credit data set, perform a ridge regression for each tuning parameter in grid.
# What covariate coefficient converges to 0 the slowest?

x <- as.matrix(Credit[, -1]) 

y <- Credit$Balance

ridge_results <- glmnet(x, y, alpha = 0, lambda = grid)

coefficients_for_largest_lambda <- coef(ridge_results)[, 100]
covariate_with_largest_coefficient <- names(which.max(coefficients_for_largest_lambda))
print(covariate_with_largest_coefficient)



# 5. Lasso Regression
model.lasso <- glmnet(x, y, alpha = 1, lambda = grid)
par(mfrow = c(1, 1))
plot(model.lasso)

for (i in 0:99) {
  n_coef <- sum(coef(model.lasso)[,100-i] > 0)
  print(sprintf("lambda = %f, num coef = %i",grid[100-i],n_coef))
  if (n_coef == 1){
    break
  }
}

# Q3. Using the Credit data set, perform a lasso regression for each tuning parameter in grid.
# At what magnitude of tuning parameter does only one covariate remain?

library(glmnet)
response_variable <- "Balance"

X <- model.matrix(Balance ~ ., data = Credit)[, -1]
y <- Credit$Balance

lasso_model <- cv.glmnet(X, y, alpha = 1)

magnitude_lambda_one_covariate <- min(lasso_model$lambda)

cat("Magnitude of tuning parameter where only one covariate remains:", magnitude_lambda_one_covariate, "\n")

# At 0.588883 the Magnitude of tuning parameter will contain only one covariate.

# The End