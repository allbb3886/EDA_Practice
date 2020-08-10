library(ggplot2)
library(ggthemes)
library(tidyverse)
library(corrplot)
library(reshape2)
library(dplyr)
library(randomForest)


# Loading in the dataset
redwine_data <- read.csv("winequality-red.csv")

## Creating our indicator variable to determine wine quality.

redwine_data$good.wine <- ifelse(redwine_data$quality>6,1,0) # If quality is > 6.5, "good".

#Examine the structure of the dataset

str(redwine_data)
summary(redwine_data)

## Creating a scatterplot matrix
plot(redwine_data)

# Since we want to see wine quality, the last two rows in order to see the best relationship for wine quality, the map shows that alcohol has the strongest correlation

## Correlation heatmap

corrplot(cor(redwine_data))

##Wine Quality

##Let us look at the distribution of red wine quality

ggplot(redwine_data,aes(quality)) +
 geom_bar(stat="count", position="dodge") +
  scale_x_continuous(breaks=seq(3,8,1)) +
  ggtitle("Distribution of Red Wine Quality") +
  theme_classic()

##Distribution of Good/Bad Wine quality
ggplot(redwine_data,aes(x=good.wine, fill=factor(good.wine))) + geom_bar(stat="count", position="dodge")+
  scale_x_continuous(breaks=seq(0,1,1)) +
  ggtitle("Distribution of Good/Bad Red Wine") 

# A large amount of bad wine outnumber the good wine, this is probably because of the threshold rating, suppose we lower it to 6 and above inclusively.

redwine_data$good.wine <- ifelse(redwine_data$quality>=6,1,0)

ggplot(redwine_data,aes(x=good.wine, fill=factor(good.wine))) + geom_bar(stat="count", position="dodge")+
  scale_x_continuous(breaks=seq(0,1,1)) +
  ggtitle("Distribution of Good/Bad Red Wine(Adjusted)") 

# As we can see there is a large increase of "good wine" once the score has been adjusted.

## Changing the threshold value back to 6.
redwine_data$good.wine <- ifelse(redwine_data$quality>6,1,0)

# Looking at the distribution of acidity levels and wine quality

ggplot(redwine_data,aes(fixed.acidity, fill=factor(good.wine))) + geom_density(alpha=0.3) +
  geom_vline(aes(xintercept=mean(fixed.acidity[good.wine==0],na.rm=T)),color="red", linetype = "dashed", lwd=1) +
  geom_vline(aes(xintercept=mean(fixed.acidity[good.wine==1],na.rm=T)), color="blue", linetype="dashed", lwd=1) +
  scale_x_continuous(breaks=seq(4,16,1)) +
  xlab(label="Fixed Acidity Level") +
  ggtitle("Distribution of Fixed Acidity Levels") +
  theme_classic()


## Let us look at the distribution of volatile acidity and wine quality

ggplot(redwine_data,aes(volatile.acidity, fill=factor(good.wine))) + geom_density(alpha=0.3) +
  geom_vline(aes(xintercept=mean(volatile.acidity[good.wine==0],na.rm=T)),color="red", linetype = "dashed", lwd=1) +
  geom_vline(aes(xintercept=mean(volatile.acidity[good.wine==1],na.rm=T)), color="blue", linetype="dashed", lwd=1) +
  scale_x_continuous(breaks=seq(0,1.6,0.1)) +
  xlab(label="Fixed Acidity Level") +
  ggtitle("Distribution of volatile Acidity Levels") +
  theme_classic()

#Citric Acid and Wine Quality
ggplot(redwine,aes(x=citric.acid,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(citric.acid[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(citric.acid[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  xlab(label = "Citric Acid Level")+
  ggtitle("Distribution of Citric Acid Levels")+
  theme_classic()

#Residual Sugar and Wine Quality
ggplot(redwine,aes(x=residual.sugar,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(residual.sugar[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(residual.sugar[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.5,15.5,1))+
  xlab(label = "Residual Sugar Level")+
  ggtitle("Distribution of Residual Sugar Levels")+
  theme_classic()

#Chlorides and Wine Quality
ggplot(redwine,aes(x=chlorides,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(chlorides[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(chlorides[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.01,0.62,0.1))+
  xlab(label = "Chlorides Level")+
  ggtitle("Distribution of Chlorides Levels")+
  theme_classic()

#Free Sulfur Dioxide and Wine Quality
ggplot(redwine,aes(x=free.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,72,8))+
  xlab(label = "Free Sulfur Dioxide Level")+
  ggtitle("Distribution of Free Sulfur Dioxide Levels")+
  theme_classic()

#Total Sulfur Dioxide and Wine Quality
ggplot(redwine,aes(x=total.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,300,20))+
  xlab(label = "Total Sulfur Dioxide Level")+
  ggtitle("Distribution of Total Sulfur Dioxide Levels")+
  theme_classic()

#Density and Wine Quality
ggplot(redwine,aes(x=density,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(density[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(density[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.9,1.1,0.05))+
  xlab(label = "Red Wine Density Level")+
  ggtitle("Distribution of Red Wine Density Levels")+
  theme_classic()

#PH and Wine Quality
ggplot(redwine,aes(x=pH,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(pH[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(pH[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(2.5,5,0.5))+
  xlab(label = "Red Wine PH Level")+
  ggtitle("Distribution of Red Wine PH Levels")+
  theme_classic()

#Sulphates and Wine Quality
ggplot(redwine,aes(x=sulphates,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(sulphates[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(sulphates[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,2,0.25))+
  xlab(label = "Sulphates Level")+
  ggtitle("Distribution of Sulphates Levels")+
  theme_classic()

#Alcohol and Wine Quality
ggplot(redwine,aes(x=alcohol,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(alcohol[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(alcohol[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(8,15,1))+
  xlab(label = "Alcohol Level")+
  ggtitle("Distribution of Alcohol Levels")+
  theme_classic()



#Predictive Modelling (Binary Classification)
# Utilizing a random forest as baseline model for predicting wine quality.

redwineRF <- randomForest(factor(good.wine)~. -quality, redwine_data,ntree=150)
redwineRF

# Predicts well, but the model predicts poor wine better than good ones.


# Variable Importance Plot

importance <- importance(redwineRF)
varImportance <- data.frame(Variables=row.names(importance),
                         Importance=round(importance[,'MeanDecreaseGini'],2))
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
  theme_classic()

# As expeceted, alcohol and sulphates the the main indicators for predicting wine quality.