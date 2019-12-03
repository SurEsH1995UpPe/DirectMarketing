##### *** LOGISTIC REGRESSION_DIRECTMARKETING *** ####
## * Direct Marketer wants to come up with a process to identify good customers * ##
## * According to the marketer customer who spends more than the average spend is considered as Good * ##

getwd()
setwd("E:/GitHub/Logistic Regression_R")
dm<- read.csv("DirectMarketing.csv")
head(dm)
# Load some packages
library(dplyr)
library(car)
library(gains)
library(irr)
library(caret)

# Target variable (AmountSpent)
dm %>% mutate(Target= ifelse(AmountSpent>mean(AmountSpent),1,0))-> dm
head(dm)
# Removing AmountSpent
dm %>% select(-AmountSpent)-> dm 

## * Data Preparation * ##
summary(dm)
str(dm)

#History - creating separate category for missing values
dm$History1<- ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<- as.factor(dm$History1)
summary(dm$History1)

# Convert Children & Catalogs in to factor variables
dm$Children<- as.factor(dm$Children)
dm$Catalogs<- as.factor(dm$Catalogs)
str(dm)

dm<- dm[,-8] # Removing History variable

# Split the data in to Train & Test samples
set.seed(200)
index<- sample(nrow(dm), 0.70*nrow(dm), replace = F)
train<- dm[index,]
test<- dm[-index,]

# Check Distributon
table(dm$Target)/nrow(dm)
table(train$Target)/nrow(train)
table(test$Target)/nrow(test)

### ** Logistic Regression Model ** ###
mod1<- glm(Target~., data = train, family = "binomial")
summary(mod1)

step(mod1, direction = "both")

mod2<- glm(formula = Target~ Age + Location + Salary + Children + Catalogs + 
            History1, family = "binomial", data = train)
summary(mod2)

#Creating dummy variables for significant levels

train$Age_Young<-ifelse(train$Age=="Young",1,0)
test$Age_Young<-ifelse(test$Age=="Young",1,0)

train$Children2<-ifelse(train$Children=="2",1,0)
test$Children2<-ifelse(test$Children=="2",1,0)

train$Children3<-ifelse(train$Children=="3",1,0)
test$Children3<-ifelse(test$Children=="3",1,0)

train$Hist1_Med<-ifelse(train$History1=="Medium",1,0)
test$Hist1_Med<-ifelse(test$History1=="Medium",1,0)

mod3<-glm(formula = Target~ Age_Young + Location + Salary + Children2 + Children3 + 
            Catalogs + Hist1_Med, family = "binomial", data = train)
summary(mod3)

#Check for Multicollinearity
vif(mod3) # (<5)

## * Model Performance * ##
pred<-predict(mod3, type = "response", newdata = test)
table(dm$Target)/nrow(dm)
# 0's-0.601 & 1's-0.399

# Converting probability values into 1s and 0s based on cutoff of 0.399
pred1<-ifelse(pred>0.399,1,0)
kappa2(data.frame(test$Target,pred1))
# Kappa=0.729

confusionMatrix(as.factor(test$Target),as.factor(pred1),positive = "1")
# Accuracy=0.8667

# ROCR curve
library(ROCR)
pred2<-prediction(pred,test$Target)
perf<-performance(pred2,"tpr","fpr") 
plot(perf,colorize=T)
abline(0,1)

auc<-performance(pred2,"auc")
auc
unlist(slot(auc,"y.values"))
# 0.9513122 It is close to 1, Indicates a good model.

# Identifying good customers using gains chart
gains(test$Target,predict(mod3,type="response",newdata = test),groups = 10)
#Top 30% contains 66.2% good customers(High probability scores)

test$prob<-predict(mod3,type = "response",newdata = test)
quantile(test$prob,p=seq(0.1,1,0.1))

#Top 30% probability scores lie between 0.801034385 and 0.999061671
targeted<-test[test$prob> 0.801034385 & test$prob< 0.999061671,"Cust_Id"]

#The customer IDs can be exported to a csv file
targeted<-as.data.frame(targeted)
write.csv(targeted,"Good_customers.csv",row.names = F)

#### *** ----------------- *** ####