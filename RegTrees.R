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









