##Created by Jared Decker 
library(ggplot2)
library(GGally)
library(rpart)
library(rpart.plot)

Toyota.df <- read.csv(file="ToyotaCorolla.csv")

#Summarizing the Data
head(Toyota.df)
tail(Toyota.df)
summary(Toyota.df)

aggregate(Price~., data=Toyota.df, FUN=mean)

#Visualizing the Data
ggplot(Toyota.df) + 
  geom_point(aes(x=Toyota.df$Price, y=Toyota.df$Age_08_04)) +
  labs(title="Price vs Age", x="Price", y="Age")

ggplot(Toyota.df) + 
  geom_point(aes(x=Toyota.df$Price, y=Toyota.df$KM)) +
  labs(title="Price vs KM", x="Price", y="KM")

ggplot(Toyota.df) +
  geom_boxplot(aes(x=as.factor(Toyota.df$Fuel_Type), y=Toyota.df$Price)) +
  labs(title = "Price vs. Fuel Type", x="Fuel Type", y="Price")

ggplot(Toyota.df) +
geom_point(aes(x=Toyota.df$Price, y=Toyota.df$HP)) +
  labs(title="Price vs HP", x="Price", y="HP")

ggplot(Toyota.df) +
  geom_boxplot(aes(x=as.factor(Toyota.df$Doors), y=Toyota.df$Price)) +
  labs(title = "Price vs. Fuel Type", x="Doors", y="Price")

ggplot(Toyota.df) +
  geom_boxplot(aes(x=as.factor(Toyota.df$Automatic), y=Toyota.df$Price)) +
  labs(title = "Price vs. Automatic", x="Automatic (y/n)", y="Price")

ggplot(Toyota.df) + 
  geom_point(aes(x=Toyota.df$Price, y=Toyota.df$Sport_Model)) +
  labs(title="Price vs Age", x="Price", y="Sport")

ggplot(Toyota.df) +
  geom_boxplot(aes(x=as.factor(Toyota.df$Sport_Model), y=Toyota.df$Price)) +
  labs(title = "Price vs. Sport Model", x="Sport Model", y="Price")

ggplot(Toyota.df) +
  geom_boxplot(aes(x=as.factor(Toyota.df$Airco), y=Toyota.df$Price)) +
  labs(title = "Price vs. Air Conditioning", x="Air Conditioning", y="Price") 

ggplot(Toyota.df) +
  geom_boxplot(aes(x=as.factor(Toyota.df$CD_Player), y=Toyota.df$Price)) +
  labs(title = "Price vs. CD Player", x="CD Player", y="Price")

ggplot(Toyota.df) +
  geom_boxplot(aes(x=as.factor(Toyota.df$Powered_Windows), y=Toyota.df$Price)) +
  labs(title = "Price vs. Powered Windows", x="Powered Windows", y="Price")

#Dropping unessessary columns
Toyota3.df = Toyota.df[ , c("Price", "Age_08_04", "KM", "Fuel_Type", "HP", "Automatic", "Doors", "Quarterly_Tax", "Mfr_Guarantee", "Guarantee_Period", "Airco", "Automatic_airco", "CD_Player", "Powered_Windows", "Sport_Model", "Tow_Bar")] 

#Partition the Data
# partition
set.seed(1)
train.id = sample(c(1:dim(Toyota3.df)[1]), dim(Toyota3.df)[1]*0.6)
train.df = Toyota3.df[train.id, ]
valid.df= Toyota3.df[-train.id, ]

#Regression 

#CP set to 0.01
class.tree = rpart(Price~.,data=train.df, minsplit=1, maxdepth = 10, minbucket = 1, method = "anova", cp=0.01)

printcp(class.tree)

summary(class.tree)

prp(class.tree, type=1, extra=1, varlen = -10)

printcp(class.tree)

plotcp(class.tree)

#CP Set to 0.001
class.tree2 = rpart(Price~.,data=train.df, minsplit=1, maxdepth = 10, minbucket = 1, method = "anova", cp=0.001)

prp(class.tree2, type=1, extra=1, varlen = -10) 

printcp(class.tree2)

plotcp(class.tree2) 


