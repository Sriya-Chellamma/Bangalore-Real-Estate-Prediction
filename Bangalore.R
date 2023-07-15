setwd("C:/Users/Ramachandran/Desktop/Tableau Docs- BBL/Bangalore")
Blr=read.csv("Bangalore.csv")
str(Blr)
summary(Blr)
library(ggplot2)
library(tidymodels)
library(caTools)
library(baguette)
library(xgboost)
blr1=initial_split(Blr,prop = .70,strata = Price)
trainblr=training(blr1)
testblr=testing(blr1)
dim(trainblr)
dim(testblr)
table(trainblr$Price)

ggplot(trainblr,aes(trainblr$Price))+geom_histogram(bins=100)
ggplot(trainblr,aes(trainblr$Price,trainblr$Area))+geom_boxplot()
ggplot(trainblr,aes(trainblr$Price,trainblr$Location))+geom_point()
trainblr %>% filter(Price >="10585105") %>% 
  ggplot(aes(trainblr$Price,trainblr$Location))+geom_bar()
trainblr2 = trainblr %>% filter(Price >="10585105")
ggplot(aes(trainblr2$Price,trainblr2$Location))+geom_bar()
ggplot(trainblr,aes(trainblr$Price,trainblr$No..of.Bedrooms))+geom_point()
ggplot(trainblr,aes(trainblr$Price,trainblr$Resale))+geom_point()
ggplot(trainblr,aes(trainblr$Price,trainblr$MaintenanceStaff))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$Gymnasium))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$SwimmingPool))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$LandscapedGardens))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$JoggingTrack))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$RainWaterHarvesting))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$IndoorGames))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$ShoppingMall))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$Intercom))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$SportsFacility))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$ATM))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$ClubHouse))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$School))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$X24X7Security))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$Price,trainblr$PowerBackup))+geom_point()+
  facet_grid(trainblr$No..of.Bedrooms~.)
ggplot(trainblr,aes(trainblr$CarParking,trainblr$Price))+geom_point()
trainblr$MaintenanceStaff=as.factor()
ggplot(trainblr,aes(factor(trainblr$CarParking),trainblr$Price))+
  geom_bar(stat="identity")
ggplot(trainblr,aes(factor(trainblr$MaintenanceStaff),trainblr$Price))+
  geom_bar(stat="identity")
dim(Blr)

str(trainblr)
summary(trainblr)
##Linear Modelling
lm1=lm(Price~.,trainblr)
summary(lm1)
lm2=lm(Price~trainblr$Area+Location+Resale,trainblr)
summary(lm2)
p1=predict(lm2,testblr)


lmspec=linear_reg() %>% set_mode("regression") %>% set_engine("lm")
lmfit=lmspec %>% fit(Price~Area+Resale,trainblr)
lmfit

pricepred= predict(lmfit,testblr)
lmresults=testblr %>% bind_cols(pricepred)  
lmresults
  
lmresults %>% rmse(truth=Price,estimate=.pred)
lmresults %>%rsq(truth =Price, estimate = .pred)

