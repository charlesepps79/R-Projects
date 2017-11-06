library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(caret)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

setwd("C:/Users/502689880/Desktop/FMI_Estimates")
requirements <- read.csv("requirements.csv", header = TRUE, 
                         stringsAsFactors = FALSE, na.strings = c(""," ",
                                                                  "NA"))
descriptions <- read.csv("descriptions.csv", header = TRUE, 
                         stringsAsFactors = FALSE, na.strings = c(""," ",
                                                                  "NA"))
descriptions[is.na(descriptions)] <- "MISC"
tasks1 <- read.csv("tasks1.csv", header = TRUE, stringsAsFactors = FALSE, 
                   na.strings = c(""," ","NA", "#N/A"))
tasks1[is.na(tasks1)] <- 0
tasks2 <- read.csv("tasks2mod.csv", header = TRUE, stringsAsFactors = FALSE, 
                   na.strings = c(""," ","NA"))

tasks <- merge(tasks1, tasks2, by=c("id_project"), allow.cartesian=TRUE)

tasks <- subset(tasks, select = c("id_project", "X0717"))

requirements <- merge(requirements, descriptions, by=c("id_req"), 
                      allow.cartesian=TRUE)


requirements <- subset(requirements, select = c("id_req", "desc_cat1", 
                                                "desc_cat2", "desc_cat3", 
                                                "desc_cat4", "id_frame", 
                                                "id_project"))

requirements <- unique(requirements[,1:7])

matchmaker <- merge(requirements, tasks, by=c("id_project"))

matchmaker <- unique(matchmaker[,1:8])

write.csv(requirements, file = "requirements_cln.csv")


num1 <- read.csv("num1.csv", header = TRUE, stringsAsFactors = FALSE)
num2 <- read.csv("num2.csv", header = TRUE, 
                      stringsAsFactors = FALSE)
num1 <- num1[which(num1$id_task == '0717'),]
spec2 <- merge(num1, num2, by="num_num")
spec2 <- merge(requirements, spec2, by=c("id_req", "id_frame"))
spec2 <- merge(tasks, spec2, by=c("id_project"))
spec2 <- unique(spec2[,1:10])

# Convert character variables and Level variable to factors
matchmaker$id_project <- as.factor(matchmaker$id_project)
matchmaker$id_req <- as.factor(matchmaker$id_req)
matchmaker$desc_cat1 <- as.factor(matchmaker$desc_cat1)
matchmaker$desc_cat2 <- as.factor(matchmaker$desc_cat2)
matchmaker$desc_cat3 <- as.factor(matchmaker$desc_cat3)
matchmaker$desc_cat4 <- as.factor(matchmaker$desc_cat4)
matchmaker$id_frame <- as.factor(matchmaker$id_frame)
matchmaker$X0717 <- as.factor(matchmaker$X0717)

# Create new columns for numeric representation
matchmaker["num_idProject"] <- NA
matchmaker["num_idReq"] <- NA
matchmaker["num_descCat1"] <- NA
matchmaker["num_descCat2"] <- NA
matchmaker["num_descCat3"] <- NA
matchmaker["num_descCat4"] <- NA
matchmaker["num_idFrame"] <- NA

# Convert character variables and Level variable to factors
matchmaker$num_idProject <- as.numeric(matchmaker$id_project)
matchmaker$num_idReq <- as.numeric(matchmaker$id_req)
matchmaker$num_descCat1 <- as.numeric(matchmaker$desc_cat1)
matchmaker$num_descCat2 <- as.numeric(matchmaker$desc_cat2)
matchmaker$num_descCat3 <- as.numeric(matchmaker$desc_cat3)
matchmaker$num_descCat4 <- as.numeric(matchmaker$desc_cat4)
matchmaker$num_idFrame <- as.numeric(matchmaker$id_frame)

# create a list of 80% of the rows in the original dataset we can use for training
matchmaker.index <- createDataPartition(matchmaker$X0717, p=0.80, 
                                        list=FALSE)
# select 20% of the data for validation
matchmaker.val <- matchmaker[-matchmaker.index,]
# use the remaining 80% of data to training and testing the models
matchmaker <- matchmaker[matchmaker.index,]

# dimensions of dataset
dim(matchmaker)

# list types for each attribute
sapply(matchmaker, class)

# take a peek at the first 5 rows of the data
head(matchmaker)

# list the levels for the class
levels(matchmaker$X0717)

# summarize the class distribution
percentage <- prop.table(table(matchmaker$X0717)) * 100
cbind(freq=table(matchmaker$X0717), percentage=percentage)

# summarize attribute distributions
summary(matchmaker)

matchmaker.datasub <- subset(matchmaker, select = c("num_idReq", 
                                                    "num_descCat1", 
                                                    "num_descCat2", 
                                                    "num_descCat3", 
                                                    "num_descCat4",
                                                    "num_idFrame",
                                                    "X0717"))
matchmaker.valsub <- subset(matchmaker.val, select = c("num_idReq", 
                                                       "num_descCat1", 
                                                       "num_descCat2", 
                                                       "num_descCat3", 
                                                       "num_descCat4",
                                                       "num_idFrame",
                                                       "X0717"))

# split input and output
x <- matchmaker.datasub[,1:6]
y <- matchmaker.datasub[,7]

# boxplot for each attribute on one image
par(mfrow=c(1,3))
for(i in 1:6) {
  boxplot(x[,i], main=names(matchmaker.datasub)[i])
}

# barplot for class breakdown
plot(y)

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
# Linear Discriminant Analysis
set.seed(7)
matchmaker.lda <- train(X0717~., data=matchmaker.datasub, method="lda", 
                        metric=metric, trControl=control)
# b) nonlinear algorithms
# Classification and Regression Trees
set.seed(7)
matchmaker.cart <- train(X0717~., data=matchmaker.datasub, method="rpart", 
                         metric=metric, trControl=control)
# k Nearest Neighbor
set.seed(7)
matchmaker.knn <- train(X0717~., data=matchmaker.datasub, method="knn", 
                        metric=metric, trControl=control)
# c) advanced algorithms
# Support Vector Machine
set.seed(7)
matchmaker.svm <- train(X0717~., data=matchmaker.datasub, method="svmRadial", 
                        metric=metric, trControl=control)
# Random Forest
set.seed(7)
matchmaker.rf <- train(X0717~., data=matchmaker.datasub, method="rf", 
                       metric=metric, trControl=control)

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(X0717) ~ num_idReq + num_descCat1 + 
                           num_descCat2 + num_descCat3 + num_descCat4 + 
                           num_idFrame, data = matchmaker.datasub)


# summarize accuracy of models
matchmaker.results <- resamples(list(lda=matchmaker.lda, 
                                     cart=matchmaker.cart, 
                                     knn=matchmaker.knn, 
                                     rf=matchmaker.rf, 
                                     svm=matchmaker.svm))
summary(matchmaker.results)

# compare accuracy of models
dotplot(matchmaker.results)

# Show model error
plot(rf_model, ylim=c(0,0.1))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# summarize Best Model
print(matchmaker.rf)

# estimate skill of model on the validation dataset
matchmaker.pred <- predict(rf_model, matchmaker.valsub)
confusionMatrix(matchmaker.pred, matchmaker.valsub$X0717)

matchmaker.estsub <- subset(matchmaker, select = c("id_req",
                                                      "id_frame",
                                                   "num_idReq", 
                                                   "num_descCat1", 
                                                   "num_descCat2", 
                                                   "num_descCat3", 
                                                   "num_descCat4",
                                                   "num_idFrame"))
est <- merge(spec2, matchmaker.estsub, by=c("id_req", "id_frame"))
est <- unique(est[,1:16])
summary(est)

est["Est"] <- NA
est$Est <- predict(matchmaker.rf, newdata = est)

write.csv(est, file = "C:/Users/502689880/Desktop/FMI_Estimates/MLI_Lists/X0717.csv")
###########################################################################
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

