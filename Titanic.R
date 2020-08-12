### Beginner EDA practice on Titanic Dataset




library(ggplot2)
library(ggthemes)
library(tidyverse)
library(corrplot)
library(reshape2)
library(dplyr)
library(randomForest)
library(mice)
library(scales)


### Loading in the test and training set.

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

## Binding the train and test 

full  <- bind_rows(train, test) # bind training & test data

# check data
str(full)
### Examining the training sets.

# Feature Engineering? This is from Megan L. Risdal's answer.
              
## Extracting title from passenger names

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)


# Examining the title counts by gender

table(full$Sex, full$Title)

## Examining the "titles" with low counts and combining them to be "rare"

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

### Re assigning mlle, ms, mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

## extracting last name from passengers
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

cat(paste('We have ', nlevels(factor(full$Surname)), ' unique surnames.')) 
##

#Following Megan's analysis, examining the incidence of families sinking or swimming together

## Create a family size variable that includes the passenger themselves.

full$FamilySize <- full$SibSp + full$Parch + 1

## The family variable

full$Family <- paste(full$Surname, full$FSize, sep='_')

### Using ggplot on the training porition (891)

ggplot(full[1:891,], aes(FamilySize, fill=factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Single individuals and larger families(bigger than 4) are more likely to not survive.


## Examine more variables?

#notice this one has a lot of missing values

full$Cabin[1:28]

## Example
strsplit(full$Cabin[2],NULL)[[1]]

#Creating a deck variable and a passenger deck?

full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


##########
# Notice that certain passengers are missing
 full[c(62,830),'Embarked']

## Megan makes the note that :

cat(paste('Megan notes that we will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))

## Removing the missing passenger IDS

embarked_full <- full %>%
    filter(PassengerId!= 62 & PassengerId != 830)

## Plotting the embarkment, class, median fare

ggplot(embarked_full, aes(Embarked,Fare,fill=factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=90), colour= 'blue', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) + 
  theme_few()

## Median fare for first class passenger departing from Charbourg coincides the the $80 paid by the embarkment-deficient passengers.

## assigning the missing values for 1st class as 'C'
full$Embarked[c(62,830)] <- 'C'

##Other passengers also have a NA Fare Value

full[1044,]

## Visualize the fares amongst their class

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

## Replacing the missing fare with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

## Imputing Age
# number of missing age values??

sum(is.na(full$Age))
## could use rpart to predict ages, but Megan used mice package.

## Factorize the variables

factor_variables <- c('PassengerId','Pclass','Sex','Embarked',
                      'Title','Surname','Family','FsizeD')
full[factor_variables] <- lapply(full[factor_variables],function(x) as.factor(x))


#Setting a random seed
set.seed (129)


#Performing the mice imputation as Megan said.

mice_model <- mice(full[, !names(full) %in% c('PassengerId','Pclass','Sex','Embarked',
                                                'Title','Surname','Family','FsizeD')], method='rf')

## Saving the outpuit

mice_output <- complete(mice_model)


## Looking at the comparison of the original distribution of passenger ages.

par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

### Replacing age vector with the output in the mice model.

full$Age <- mice_output$Age

## Show the new number of missing age values (should be 0)

sum(is.na(full$Age))

# Megan performs a second set of 'feature engineering', to create age-dependent variables, child and mother.

## Examining the relationship between age and survival.

ggplot(full[1:891,], aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  # including sex since it is consider a significant predictor
  facet_grid(.~Sex) +
  theme_few()

## Creating the children column 

full$Children[full$Age<18] <- 'Child'
full$Children[full$Age >= 18] <- 'Adult'

## Showing the counts
table(full$Children, full$Survived)


# Creating the mother variable

full$Mom <- 'Not Mom'
full$Mom[full$Sex=='female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss']<- 'Mom'

## show the counts
table(full$Mom, full$Survived)
# Factorizing the two
full$Children <- factor(full$Children)
full$Mom <- factor(full$Mom)

### Checking for missing data

md.pattern(full)


## Predictions !
## Will look to use a randomForest algorithm

## Let us split the data back into test and training sets.

train <- full[1:891,]
test <- full[892:1309,]

## Let us build the model and set the seed.

set.seed(613)

random_forest_model <- randomForest(factor(Survived) ~ Pclass + Age + Sex+ SibSp + Parch +
                                      Fare + Embarked + Title + FamilySize + Children + Mom, data=train)

## Showing the model error
plot(random_forest_model,ylim=c(0,0.36))
legend('topright', colnames(random_forest_model$err.rate), col = 1:3, fill=1:3)

## Black line shows the overall error rate to fall below 20%, more sucessfull predicting death than surival (lower error)


## Variable Importance Plot

Importance <- importance(random_forest_model)
VariableImportance <- data.frame(Variables = row.names(Importance),
                                Importance=round(Importance[,'MeanDecreaseGini'],2))

## Creating ranks on the variables based off of importance.
RankImportance <- VariableImportance %>%
    mutate(Rank=paste0('#', dense_rank(desc(Importance))))

## Using ggplot to visualize the importance


  

ggplot(RankImportance, aes(reorder(x= Variables,Importance), y= Importance, fill = Importance)) +
    geom_bar(stat='identity') + 
    geom_text(aes(Variables, y=0.85, label = Rank), 
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
    labs(x='Variables') +
    coord_flip() +
    theme_few()

## Making the final prediction

predict_set <- predict(random_forest_model, test)

final_result <- data.frame(PassengerID = test$PassengerId, Survived= predict_set)

# Solution to file
write.csv(final_result,file='rf_model_titanic.csv', row.names=F)

