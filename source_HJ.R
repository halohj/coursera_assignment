
library(lattice)
library(ggplot2)
library(caret)
library(corrplot)
library(rmarkdown)
library(knitr)

tr_data <- read.csv("./pml-training.csv", na.strings = c("", " ", NA))
te_data <- read.csv("./pml-testing.csv", na.strings = c("", " ", NA))
set.seed(1017)

tr_data <- tr_data[tr_data$new_window=='no',]
colSums(is.na(tr_data))

tr_data <- tr_data[!colSums(is.na(tr_data)|tr_data ==0) == nrow(tr_data)]

te_data <- te_data[te_data$new_window=='no',]
te_data <- te_data[!colSums(is.na(te_data)|te_data ==0) == nrow(te_data)]

train_idx <- sample(1:nrow(tr_data), size=0.7*nrow(tr_data), replace=F)
val_idx <- (-train_idx)

tr_data <- tr_data[train_idx,]
tr_data <- subset(tr_data, select=-c(X))
val_data <- tr_data[val_idx,]
te_data <- subset(te_data, select=-c(X,problem_id))

Y_train <- tr_data$classe
X_train <- subset(tr_data, select=-c(classe))
Y_val <- val_data$classe
X_val <- subset(val_data, select=-c(classe))

#model1
c_RF <- trainControl(method="cv", number=4, verboseIter=FALSE)
randomforest <- train(classe~., data=tr_data, method="rf", trControl=c_RF)
randomforest$finalModel

val_rf <- predict(randomforest, val_data)
confusion_matrix_rf <- confusionMatrix(val_rf, val_data$classe)
confusion_matrix_rf

#model2
c_GB <- trainControl(method="cv", number=4, verboseIter=F)
GB <- train(classe~., data=tr_data, method="gbm", trControl = c_GB, tuneLength = 5, verbose = F)
GB$finalModel

val_GB <- predict(GB, val_data)
confusion_matrix_GB <- confusionMatrix(val_GB, val_data$classe)
confusion_matrix_GB

plot(decision_tree)
plot(randomforest)
plot(GB)

## test-set predict

test_predict <- predict(randomforest, te_data)
test_predict
