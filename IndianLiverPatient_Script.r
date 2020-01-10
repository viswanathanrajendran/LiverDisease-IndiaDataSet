if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")

# Read the data file from local folder
sourcedata <- read.csv("./indian_liver_patient.csv")

# Data exploratory analysis
head(sourcedata)
table(sourcedata$Dataset)
str(sourcedata)
summary(sourcedata)
sourcedata$Dataset<-factor(sourcedata$Dataset, levels = c(1,2), labels=c("No", "Yes"))

# Create test and train datasets
set.seed(1, sample.kind="Rounding")
samplesize <- floor(0.9 * nrow(sourcedata))
index <- sample(seq_len(nrow(sourcedata)), size = samplesize)
train_set <- sourcedata[index, ]
test_set = sourcedata[-index, ]


# Logistic Regression Model
set.seed(1, sample.kind="Rounding")
log_model <- train(Dataset ~ Total_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase + Total_Protiens + Albumin + Age, data = train_set, method = "glm")
log_preds <- predict(log_model, test_set)
log_accuracy <- mean(log_preds == test_set$Dataset)
summary(log_model)
model_results <- tibble(method = "Logistic Regression", Accuracy = log_accuracy)

# LDA Model
set.seed(1, sample.kind="Rounding")
lda_model <- train(Dataset ~ Total_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase + Total_Protiens + Albumin + Age, data = train_set, method = "lda")
lda_preds <- predict(lda_model, test_set)
lda_accuracy <- mean(lda_preds == test_set$Dataset)
summary(lda_model)
model_results<-add_row(model_results,method = "Linear Discriminant Analysis", Accuracy = lda_accuracy)

# QDA Model
set.seed(1, sample.kind="Rounding")
qda_model <- train(Dataset ~ Total_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase + Total_Protiens + Albumin + Age, data = train_set, method = "qda")
qda_preds <- predict(qda_model, test_set)
qda_accuracy <- mean(qda_preds == test_set$Dataset)
summary(qda_model)
model_results<-add_row(model_results,method = "Quadratic Discriminant Analysis", Accuracy = qda_accuracy)

# KNN Model
set.seed(1, sample.kind="Rounding")
knn_model <- train(Dataset ~ Total_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase + Total_Protiens + Albumin + Age, data = train_set, method = "knn",
                   tuneGrid = data.frame(k = seq(1, 50, 2)))
knn_preds <- predict(knn_model, test_set)
knn_accuracy <- mean(knn_preds == test_set$Dataset)
summary(knn_accuracy)
model_results<-add_row(model_results,method = "KNN", Accuracy = knn_accuracy)


# Classification Tree Model
set.seed(1, sample.kind="Rounding")
tree_model <- train(Dataset ~ Total_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase + Total_Protiens + Albumin + Age, data = train_set, method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
tree_preds <- predict(tree_model, test_set)
tree_accuracy <- mean(tree_preds == test_set$Dataset)
summary(tree_accuracy)
model_results<-add_row(model_results,method = "Classification Tree", Accuracy = tree_accuracy)

# Random Forest Model
set.seed(1, sample.kind="Rounding")
forest_model <- train(Dataset ~ Total_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase + Total_Protiens + Albumin + Age, data = train_set, method = "rf",
                      tuneGrid = data.frame(mtry = seq(1:7)))
forest_preds <- predict(forest_model, test_set)
forest_accuracy <- mean(forest_preds == test_set$Dataset)
summary(forest_accuracy)
model_results<-add_row(model_results,method = "Random Forest", Accuracy = forest_accuracy)

