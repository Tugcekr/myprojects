library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
data<- read.csv("C:/Users/Tugce/OneDrive/Masaüstü/WA_Fn-UseC_-HR-Employee-Attrition.csv")
head(data)
ncol(data)
View(data)
colSums(is.na(data))
glimpse(data)
table(data$EmployeeCount)
table(data$NumCompaniesWorked)
table(data$Attrition)
levels(data$Attrition)
str(data)


#EDA ANALYSİS-REQUIRED VISUALIZATIONS
ggplot(data, aes(x = EnvironmentSatisfaction)) +
  geom_bar() +
  labs(title = "Environment Satisfaction", x = "Environment Satisfaction")+
  theme_minimal()

ggplot(data, aes(x=JobInvolvement)) + ggtitle("JobInvolvement") + xlab("JobInvolvement")+ 
  geom_bar()+ylab("Percentage") + coord_flip() + theme_minimal()

ggplot(data, aes(x=JobLevel)) + ggtitle("JobLevel") + xlab("JobLevel")+ 
  geom_bar() + ylab("Percentage") + coord_flip() + theme_minimal()

ggplot(data, aes(x=JobRole)) + ggtitle("JobRole") + xlab("JobRole")+
geom_bar() + ylab("Percentage") + coord_flip() + theme_minimal()

ggplot(data, aes(x = JobRole, fill = EnvironmentSatisfaction)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Environment Satisfaction by Job Role", x = "Job Role", y = "Count") +
  theme_minimal()


ggplot(data, aes(x = Age, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Age vs Attrition", y = "Proportion", x = "Age") +
  theme_minimal()


ggplot(data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Monthly Income vs Attrition", y = "Monthly Income", x = "Attrition") +
  theme_minimal()

ggplot(data, aes(x = YearsAtCompany, fill = Attrition)) +
  geom_density(alpha = 0.7) +
  labs(title = "Years At Company vs Attrition", y = "Density", x = "Years At Company") +
  theme_minimal()

ggplot(data, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Overtime vs Attrition Proportion", x = "Overtime", y = "Proportion") +
  theme_minimal()


ggplot(data, aes(x = "", fill = Department)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(title = "Department-wise Salary Distribution") +
  theme_minimal() +
  theme(axis.text.x = element_blank())


ggplot(data, aes(x = EducationField, y = MonthlyIncome, fill = Department)) +
  geom_boxplot() +
  labs(title = "Education Field vs Salary by Department", x = "Education Field", y = "Monthly Income") +
  theme_minimal()





#removing unnecessary variables
data<-data[,-c(9,10,22,27)]
colnames(data)
ncol(data)
str(data)
data$Attrition <- as.factor(data$Attrition)
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$Department <- as.factor(data$Department)
data$EducationField <- as.factor(data$EducationField)
data$Gender <- as.factor(data$Gender)
data$JobRole <- as.factor(data$JobRole)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$OverTime <- as.factor(data$OverTime)


library(caret)

#Data partition
set.seed(123)
split <- createDataPartition(data$Attrition, p = 0.7, list = FALSE) 

train<- data[split, ]  
test<- data[-split, ]  

table(train$Attrition)
table(test$Attrition)

model<-glm(Attrition~., family="binomial",data=data)
summary(model)
colnames(data)

library("MASS")
model2<- stepAIC(model, direction="both")

summary(model2)



library(caret)   
library(pROC)    

############################
predmodel1 <- predict(model, newdata = test, type = "response")
predictclass_model1 <- ifelse(predmodel1 > 0.5, "Yes", "No")

predictclass_model1 <- as.factor(predictclass_model1)

# Confusion Matrix (model1)
library(caret)
cm_model1 <- confusionMatrix(predictclass_model1, reference = test$Attrition, positive = "Yes")
print(cm_model1)

#############################
predmodel2 <- predict(model2, newdata = test, type = "response")
roc_curve <- roc(test$Attrition, predmodel2)
best_threshold <- coords(roc_curve, x = "best", best.method = "youden")$threshold
print(best_threshold)
predictclass_model2 <- ifelse(predmodel2 > 0.2, "Yes", "No")

predictclass_model2 <- as.factor(predictclass_model2)


# Confusion Matrix 
cm_model2 <- confusionMatrix(predictclass_model2, reference = test$Attrition, positive = "Yes")
print(cm_model2)



library(pROC)

# ROC
roc_curve_model2 <- roc(test$Attrition, predmodel2) 

# AUC
auc_value_model2 <- auc(roc_curve_model2)

# ROC 
plot(roc_curve_model2, col = "blue", main = "ROC curve- Model 2")

cat("Model 2 - AUC :", auc_value_model2, "\n")




###################################################################################
#The overall accuracy of the model is 82.05%, which indicates that the model is making correct predictions in a high percentage of cases.

#The model performs well for the "Yes" class, with a relatively high Sensitivity (80.28%), meaning it 
#correctly predicts employee attrition in most cases.

#NPV shows how accurate the "No" predictions are. The NPV is 95.60%, indicating that the model is 
#making highly accurate predictions for the "No" class.

#Balanced Accuracy measures how well the model performs across both classes. The balanced accuracy is 81.33%, meaning
#the model is balancing its performance across both "Yes" and "No" classes reasonably well.

#Specificity measures how well the model is predicting the "No" class (non-attrition). The specificity is 82.38%,
#meaning the model is performing well at predicting "No" cases.

#AUC of 0.87 means that the model is able to correctly distinguish between positive and negative classes 87% of the time. 
#This is a strong indicator of good performance, particularly in terms of the model's overall discriminative power.
