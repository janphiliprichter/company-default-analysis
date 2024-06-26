# Loading libraries
library(bestglm)
library(car)
library(glmnet)
library(MASS)
library(ggplot2)
library(GGally)
library(factoextra)
library(scales)
library(dplyr)
library(MLmetrics)
library(Gifi)


# Reading the data set
data <- read.csv("preprocessed_data.csv",
                 tryLogical = TRUE,
                 stringsAsFactors = TRUE)

# Setting random seed
set.seed(42)


### Calculating Principal Components for PCA Regression

# Numerical data for dimensionality reduction
data_num <- data[, c("log_age", "log_revenues", "log_profits", "sqrt_gmr", 
                     "last_statement_age",  "core_income_ratio", 
                     "cash_asset_ratio", "consolidated_liabilities_ratio", 
                     "sin_month", "cos_month", "month_day", "sin_dom", 
                     "cos_dom", "week_day", "sin_dow",  "cos_dow", "year")]

# Singular value decomposition
svd <- prcomp(data_num, 
              center = TRUE, 
              scale = TRUE)

## Save the first 4 PC's for further analysis
data$pc1 <- svd$x[,"PC1"]
data$pc2 <- svd$x[,"PC2"]
data$pc3 <- svd$x[,"PC3"]
data$pc4 <- svd$x[,"PC4"]


### Calculating Nonlinear Principal Component for NLPCA Regression

ord_data <- data[, c("score1", "score2", "score3")]

nlpc = princals(ord_data, ndim = 1, ordinal = TRUE)

data$nlpc <- nlpc$objectscores



### Train-Test-Split
sample <- sample(c(TRUE, FALSE), 
                 nrow(data), 
                 replace = TRUE, 
                 prob = c(0.7,0.3))

train <- data[sample, ]
test <- data[!sample, ]


## Logistic Regression with all Variables

# Model definition
mod_all <- glm(default ~ score1 + score2 + score3 + geo_area + sqrt_gmr + 
                 core_income_ratio + cash_asset_ratio + 
                 consolidated_liabilities_ratio + cr_available + 
                 last_statement_age + log_age + log_revenues + log_profits +
                 sin_month + cos_month + month_day + sin_dom + cos_dom + 
                 week_day + sin_dow + cos_dow + year, 
               data = train, 
               family = "binomial")

# Model summary
summary(mod_all)

# Variance inflation factors
vif(mod_all)



## Model Selection

# Variables for model selection
Xy <- train[, c("score1", "score2", "geo_area", "sqrt_gmr", 
                "core_income_ratio", "cash_asset_ratio", 
                "consolidated_liabilities_ratio", "cr_available",  
                "last_statement_age", "log_age", "log_revenues", "log_profits", 
                "sin_dow", "cos_dow", "year", "default")]

# Model selection by BIC
mod_best <- bestglm(Xy = Xy,
                    family = binomial, 
                    IC = "BIC", 
                    TopModels = 1,
                    method = "exhaustive")

mod_best <- mod_best$BestModel

# Summary of best model
summary(mod_best)



## PCA Regression

# Model definition
mod_pca <- glm(default ~ pc1 + pc2 + pc3 + pc4, 
               data = train, 
               family = "binomial")

# Model summary
summary(mod_pca)



### NLPCA Regression

mod_nlpca <- glm(default ~ core_income_ratio + log_revenues + nlpc, 
                 data = train, 
                 family = "binomial")

summary(mod_nlpca)



## Lasso regression

# Scaled data for model training and testing
X_train <- scale(as.matrix(train[, c("log_age", "log_revenues", "log_profits", 
                                     "sqrt_gmr",  "last_statement_age",  
                                     "core_income_ratio", "cash_asset_ratio", 
                                     "consolidated_liabilities_ratio", 
                                     "sin_month", "cos_month", "month_day", 
                                     "sin_dom", "cos_dom", "week_day", 
                                     "sin_dow",  "cos_dow", "year")]))


X_test <- scale(as.matrix(test[, c("log_age", "log_revenues", "log_profits", 
                                   "sqrt_gmr",  "last_statement_age",  
                                   "core_income_ratio", "cash_asset_ratio", 
                                   "consolidated_liabilities_ratio", 
                                   "sin_month", "cos_month", "month_day", 
                                   "sin_dom", "cos_dom", "week_day", 
                                   "sin_dow",  "cos_dow", "year")]))


# Visualisation of coefficients for multiple lambdas
lambda_array <- 10 ** seq(-1.2, -2, by = -0.01)

lasso <- glmnet(X_train,
                train$default,
                alpha = 1,
                lambda = lambda_array,
                family = "binomial")

plot(lasso, 
     xvar = "lambda", 
     label = TRUE, 
     lwd = 2,
     main = "Coefficients Lasso Regression",
     bty = "n")

axis(side = 3,
     at = seq(par("usr")[1], par("usr")[2], len = 1000), 
     tck = -0.5,
     lwd = 2, 
     col = "white", 
     labels = F)

# Cross validation for shrinkage penalty
cv_lasso <- cv.glmnet(X_train, 
                      train$default, 
                      alpha = 1,
                      family = "binomial",
                      nfold = 1077)

plot(cv_lasso,
     main = "CV - Shrinkage Penalty Lasso Regression",
     bty = "n")

axis(side = 3,
     at = seq(par("usr")[1], par("usr")[2], len = 1000), 
     tck = -0.5,
     lwd = 2, 
     col = "white", 
     labels = F)

# Finding the optimal lambda value that minimizes test MSE
lambda_lasso <- cv_lasso$lambda.min 

# Fitting the model
mod_lasso <- glmnet(X_train, 
                    train$default, 
                    alpha = 1, 
                    lambda = lambda_lasso,
                    thres = 1E-12,
                    family = "binomial")

coef(mod_lasso)



### Model evaluation and predictions

## Evaluation metrics

# AIC
AIC(mod_all)
AIC(mod_best)
AIC(mod_pca)
AIC(mod_nlpca)

# BIC
BIC(mod_all)
BIC(mod_best)
BIC(mod_pca)
BIC(mod_nlpca)


## Predictions

# All variables
pred_all <- predict(mod_all, newdata = test, type = "response")
mse_all <- MSE(pred_all, test$default)
mae_all <- MAE(pred_all, test$default)


# Best subset
pred_best <- predict(mod_best, newdata = test, type = "response")
mse_best <- MSE(pred_best, test$default)
mae_best <- MAE(pred_best, test$default)


# PCA
pred_pca <- predict(mod_pca, newdata = test, type = "response")
mse_pca <- MSE(pred_pca, test$default)
mae_pca <- MAE(pred_pca, test$default)


# NLPCA
pred_nlpca <- predict(mod_nlpca, newdata = test, type = "response")
mse_nlpca <- MSE(pred_nlpca, test$default)
mae_nlpca <- MAE(pred_nlpca, test$default)


# Lasso
pred_lasso <- predict(mod_lasso, newx = X_test, type = "response")
mse_lasso <- MSE(pred_lasso, test$default)
mae_lasso <- MAE(pred_lasso, test$default)


# Bar plot
evals <- data.frame(method = c("All Variables", "Best Subset", "PCA", "NLPCA", 
                               "Lasso",
                               "All Variables", "Best Subset", "PCA", "NLPCA",
                               "Lasso"),
                    eval = c(mse_all, mse_best, mse_pca, mse_nlpca, mse_lasso,
                             mae_all, mae_best, mae_pca, mae_nlpca, mae_lasso),
                    metric = c(rep("MSE", 5),
                               rep("MAE", 5)))


ggplot(data = evals,
       mapping = aes(x = factor(method, 
                                levels = c("All Variables", "Best Subset", 
                                           "PCA", "NLPCA", "Lasso")),
                     y = eval,
                     fill = metric)) + 
  geom_bar(stat = "identity",
           position = position_dodge(),
           alpha = 0.9) +
  scale_fill_manual(values = c("#023740", "#21918c")) +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = guide_legend(title = "Metric")) + 
  theme(axis.text.x = element_text(angle = 315))

