##### *** LINEAR REGRESSION_DIRECTMARKETING *** ####
#### To predict the amount spent by the customers using a linear regression model ####

getwd()
setwd("E:/GitHub/Linear Regression_R")

# Read the file name
data<- read.csv("directmarketing.csv")
library(dplyr)

# Display the first few observations of the file
# Exploratory Data Analysis
head(data)
str(data)

# Age (Factor Varibale)
# Box Plot
plot(data$Age, data$AmountSpent,col="blue")

# We observed that Middle & Old Age are similar
# Combine them
data$Age1<- ifelse(data$Age!="Young", "Middle-Old", as.character(data$Age))
data$Age1<- as.factor(data$Age1)
summary(data$Age1)
plot(data$Age1, data$AmountSpent, col="blue")

# Gender
plot(data$Gender, data$AmountSpent, col="red")

# OwnHome
plot(data$OwnHome, data$AmountSpent, col="green")

# Married
plot(data$Married, data$AmountSpent, col="yellow")

# Location
plot(data$Location, data$AmountSpent, col="pink")

# Salary
# Scatter Plot(Both are Integer type)
plot(data$Salary, data$AmountSpent)

# Children
summary(data$Children)
unique(data$Children)
#Since there are 4 levels, It can be converted in to factors.
data$Children<- as.factor(data$Children)
summary(data$Children)
plot(data$Children, data$AmountSpent, col="red")

# Since Level 2 & 3 are similar, Combine them
data$Children1<- ifelse(data$Children=="2" | data$Children=="3", "2-3", as.character(data$Children))
data$Children1<- as.factor(data$Children1)
summary(data$Children1)
plot(data$Children1, data$AmountSpent, col="red")

# History
summary(data$History)
# Summarize AmountSpent based on History
tapply(data$AmountSpent, data$History, mean)

# check the mean AmountSpent for the Missing Value group (NA's)
ind<- which(is.na(data$History))
mean(data[ind, "AmountSpent"])

# Treat Missing Value's as a separate group
data$History1<- ifelse(is.na(data$History), "Missing", as.character(data$History))
data$History1<- as.factor(data$History1)
summary(data$History1)
plot(data$History1, data$AmountSpent, col="blue")

# Catalogs
summary(data$Catalogs)
unique(data$Catalogs)
data$Catalogs<- as.factor(data$Catalogs)

# Remove the columns of which you have derived new columns
data1<- data[, -c(1,7,8)]

# Split the data in to Train & Test samples.
set.seed(200)

# Before training the model, The available data is typically split up into two components: a training set and a test set in a 70-30 or 80-20 ratio.
index<- sample(nrow(data1), 0.70*nrow(data1), replace= F)
train<- data1[index,]
test<- data1[-index,]

### ** Linear Regression Model ** ###
mod1<- lm(AmountSpent~., data = train)
summary(mod1)

mod2<- lm(formula = AmountSpent~ Gender + Location + Salary + Catalogs + Children1
          + History1, data = train)
summary(mod2)

# Creating dummy variables
train$Low_d<- ifelse(train$History1=="Low",1,0)
test$Low_d<- ifelse(test$History1=="Low",1,0)

train$Med_d<- ifelse(train$History1=="Medium",1,0)
test$Med_d<- ifelse(test$History1=="Medium",1,0)

mod3<- lm(formula = AmountSpent~ Location + Salary + Catalogs + Children1 +
            Low_d + Med_d, data = train)
summary(mod3)

# Model assumptions check
hist(mod3$residuals) # +ve Skew
library(ggplot2)
library(car)
qqPlot(mod3$residuals) # Residuals are not normal

# Multicollinearity Check
vif(mod3) # < 10 , Ok

# Assumption of constant variation (Heteroscedasticity)
plot(mod3$fitted.values, mod3$residuals) # Funnel Shape

# Since data suffers from Heteroscedasticity, Apply log transform to dependent(y) variable
mod4<- lm(formula = log(AmountSpent)~ Location + Salary + Catalogs + Children1
         + Low_d + Med_d, data = train)
summary(mod4)

qqPlot(mod4$residuals) # 
vif(mod4) #OK
plot(mod4$fitted.values,mod4$residuals) #OK

## * Model Performance * ##
# Predicting on Test data
predicted<- predict(mod4, newdata = test)
actual<- log(test$AmountSpent)

# Prediction Accuracy
# A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, 
# i.e. when the actuals values increase the predicted values also increase and vice-versa.
dat<- data.frame(predicted,actual)
cor(dat) # 90.712%

# Mean Average Percentage Error (<5)
mape<- mean(abs((dat$actual-dat$predicted)/dat$actual))
mape # 4.71

# Fit-Chart 
p<- ggplot(dat, aes(x= row(dat)[,2],y= predicted))
p+geom_line(color="blue")+geom_line(data= dat, aes(y=actual),color="black")

#### *** ----------------- *** ####
