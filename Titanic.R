### Kaggle Titanic competition
## Fabien Millet - Nov 2015

Lib <- "Y:/Work/R/win-library/3.2"
require(data.table)
library("stringr", lib.loc=Lib)
library("sqldf", lib.loc=Lib)
library("date", lib.loc=Lib)
library("rpart", lib.loc=Lib)
library("rattle", lib.loc=Lib)
library("randomForest", lib.loc=Lib)
library("rpart.plot", lib.loc=Lib)
library("RColorBrewer", lib.loc=Lib)
library("ggplot2", lib.loc=Lib)

Dir <- "Y:/Work/Training/Claims Analytics group training/Titanic"
setwd(Dir)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train_0 <- train
test_0 <- test

# VARIABLE DESCRIPTIONS:
#   survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# SibSp           Number of Siblings/Spouses Aboard (Sibling: Brother, Sister, Stepbrother, or Stepsister; Spouse: Husband or Wife)
# Parch           Number of Parents/Children Aboard (Parent: Mother or Father; Child: Son, Daughter, Stepson, or Stepdaughter
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# 
# SPECIAL NOTES:
#   Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# 
# Age is in Years; Fractional if Age less than One (1)
# If the Age is Estimated, it is in the form xx.5


### Data cleaning
test_0$Survived <- NA
Full <- rbind(train_0, test_0)
rm(train_0, test_0)

Full <- data.table(Full)
Full[, ':=' (Survived = as.factor(Survived), Sex = as.factor(Sex), Pclass = as.factor(Pclass), Embarked = as.factor(Embarked))]

# Embarked
summary(Full$Embarked) # 2 ""
TEMP <- Full[Full$Embarked == "",] # English titles: replace by Southampton
Full$Embarked[Full$Embarked == ""] <- "S"

# Fare
Full[, .(AvgFare = sum(Fare) / .N), by = "Pclass"]
dim(Full[is.na(Full$Fare),])
Full$Pclass[is.na(Full$Fare)] # 3
Full$Fare[is.na(Full$Fare)] <- mean(Full$Fare[Full$Pclass == 3], na.rm = TRUE)

dim(Full[Full$Fare == 0,]) # 17 zeros => replace by average Fare

Full$Fare[Full$Fare == 0 & Full$Pclass == 1] <- mean(Full$Fare[Full$Pclass == 1], na.rm = TRUE)
Full$Fare[Full$Fare == 0 & Full$Pclass == 2] <- mean(Full$Fare[Full$Pclass == 2], na.rm = TRUE)
Full$Fare[Full$Fare == 0 & Full$Pclass == 3] <- mean(Full$Fare[Full$Pclass == 3], na.rm = TRUE)

# Age
dim(Full[is.na(Full$Age),]) # 263 NA - Replace by mean on Pclass and gender

Pas_Age <- Full[,.(MedAge = median(Age, na.rm = TRUE)), by = c("Pclass", "Sex")]
Pas_Age

Full$Age2[is.na(Full$Age) & Full$Pclass == 1 & Full$Sex == "male"] <- Pas_Age$MedAge[Pas_Age$Pclass == 1 & Pas_Age$Sex == "male"]
Full$Age2[is.na(Full$Age) & Full$Pclass == 1 & Full$Sex == "female"] <- Pas_Age$MedAge[Pas_Age$Pclass == 1 & Pas_Age$Sex == "female"]
Full$Age2[is.na(Full$Age) & Full$Pclass == 2 & Full$Sex == "male"] <- Pas_Age$MedAge[Pas_Age$Pclass == 2 & Pas_Age$Sex == "male"]
Full$Age2[is.na(Full$Age) & Full$Pclass == 2 & Full$Sex == "female"] <- Pas_Age$MedAge[Pas_Age$Pclass == 2 & Pas_Age$Sex == "female"]
Full$Age2[is.na(Full$Age) & Full$Pclass == 3 & Full$Sex == "male"] <- Pas_Age$MedAge[Pas_Age$Pclass == 3 & Pas_Age$Sex == "male"]
Full$Age2[is.na(Full$Age) & Full$Pclass == 3 & Full$Sex == "female"] <- Pas_Age$MedAge[Pas_Age$Pclass == 3 & Pas_Age$Sex == "female"]
Full$Age2[!is.na(Full$Age)] <- Full$Age[!is.na(Full$Age)]
dim(Full[is.na(Full$Age2),]) # 



### Create additional data
Full$Parch2 <- rep("No", nrow(Full))
Full$Parch2[Full$Parch > 0] <- "Yes"

Full$SibSp2 <- rep("No", nrow(Full))
Full$SibSp2[Full$SibSp > 0] <- "Yes"

Full$NRelatives <- Full$SibSp + Full$Parch
summary(Full$NRelatives)
summary(as.factor(Full$NRelatives))

Full[, Title := ifelse(grepl("Miss.", Name, ignore.case = TRUE) | grepl("Mlle.", Name, ignore.case = TRUE), "Miss", 
                        ifelse(grepl("Mrs.", Name, ignore.case  = TRUE) | grepl("Mme.", Name, ignore.case = TRUE), "Mrs", 
                               ifelse(grepl("Mr.", Name, ignore.case = TRUE), "Mr", 
                                      ifelse(grepl("Master", Name, ignore.case = TRUE), "Master", "Other"))))]
Full[, .N, by = Title]

# Recreate train and test
train <- Full[!is.na(Full$Survived),]
test <- Full[is.na(Full$Survived),]
test$Survived <- NULL

rm(Full)

###

train[, .N, by = Survived]
train[, .N, by = c("Sex", "Survived")]

train[, .(SurvivalRate = sum(ifelse(Survived == 1, 1, 0)) / .N), by = c("Sex")]             # 74% female, 19% male
train[, .(SurvivalRate = sum(ifelse(Survived == 1, 1, 0)) / .N), by = c("Pclass")]          # 1st class 63%, 2nd 47%, 3rd 24%
train[, .(SurvivalRate = sum(ifelse(Survived == 1, 1, 0)) / .N), by = c("Embarked")]        # Cherboug 55%, Southampton and QueensTown less than 40% - see class
train[, .(SurvivalRate = sum(ifelse(Survived == 1, 1, 0)) / .N), by = c("Sex", "Pclass")]
train[, .(SurvivalRate = sum(ifelse(Survived == 1, 1, 0)) / .N), by = c("Parch")]           # Greater survival rate with 1 or 2 parents - could be driven by children not travelling alone or lower proportion of single women
train[, .(SurvivalRate = sum(ifelse(Survived == 1, 1, 0)) / .N), by = c("Parch2")]           # Greater survival rate with 1 or 2 parents - could be driven by children not travelling alone or lower proportion of single women
train[, .N, by = c("Parch", "Survived")]
train[, .N, by = c("Parch", "Sex")] # female represent 35% of passengers but 56% of passengers with Parents or Children
train[, .(SurvivalRate = sum(ifelse(Survived == 1, 1, 0)) / .N), by = c("SibSp")]           # Greater survival rate with one sibling or spouse
train[, .N, by = c("SibSp", "Survived")]
train[, .N, by = c("SibSp", "Sex")] # female represent 35% of passengers but 50% of passengers with siblings or spouse: fewer single women


p <- ggplot(data = train, aes(x=Fare, fill = Survived))
p + geom_density(alpha = 0.2)

p <- ggplot(data = train, aes(x=Age, fill = Survived))
p + geom_density(alpha = 0.2) # Peak of survival below 10 yo

p <- ggplot(data = train, aes(x=Age, fill = Parch2))
p + geom_density(alpha = 0.2) # Passengers without parents nor children are mostly 20-40 yo, whereas of course passengers with parents are mostly children





### Decision trees

Tree_1 <- rpart(Survived ~ Sex + Age,
                data = train, 
                method ="class")
fancyRpartPlot(Tree_1)

Tree_2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                data = train, 
                method ="class")
fancyRpartPlot(Tree_2)

jpeg("Tree 2.jpeg", width = 10, height = 10, units = 'in', res = 300)
fancyRpartPlot(Tree_2)
dev.off()


Tree_3 <- rpart(Survived ~ Pclass + Sex + Age,
                data = train, 
                method ="class")
fancyRpartPlot(Tree_3)


Tree_4 <- rpart(Survived ~ Pclass + Sex + Age + Title,
                data = train, 
                method ="class")
fancyRpartPlot(Tree_4)


Tree_5 <- rpart(Survived ~ Pclass + Sex + Age + NRelatives + Fare,
                data = train, 
                method ="class")
fancyRpartPlot(Tree_5)

jpeg("Tree 5.jpeg", width = 10, height = 10, units = 'in', res = 300)
fancyRpartPlot(Tree_5)
dev.off()

### Predictions 

Pred_1 <- predict(Tree_1, test, type = "class")
Sol_1 <- data.frame(PassengerId = test$PassengerId, Survived = Pred_1)
write.csv(Sol_1, "test_Sol1.csv", row.names = FALSE) # Score = 0.75598

Pred_2 <- predict(Tree_2, test, type = "class")
Sol_2 <- data.frame(PassengerId = test$PassengerId, Survived = Pred_2)
write.csv(Sol_2, "test_Sol2.csv", row.names = FALSE) # Score = 0.79426

Pred_3 <- predict(Tree_3, test, type = "class")
Sol_3 <- data.frame(PassengerId = test$PassengerId, Survived = Pred_3)
write.csv(Sol_3, "test_Sol3.csv", row.names = FALSE) # Score = 0.73684

Pred_4 <- predict(Tree_4, test, type = "class")
Sol_4 <- data.frame(PassengerId = test$PassengerId, Survived = Pred_4)
write.csv(Sol_4, "test_Sol4.csv", row.names = FALSE) # Score = 0.75598

Pred_5 <- predict(Tree_5, test, type = "class")
Sol_5 <- data.frame(PassengerId = test$PassengerId, Survived = Pred_5)
write.csv(Sol_5, "test_Sol5.csv", row.names = FALSE) # Score = 0.77512

rm(Sol_1, Pred_1, Tree_1, Sol_2, Pred_2, Tree_2, Sol_3, Pred_3, Tree_3, Sol_4, Pred_4, Tree_4, Sol_5, Pred_5, Tree_5)


### Random Forest
set.seed(28)

RandF_1 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age2 + Fare + Embarked + NRelatives, 
                        data=train, importance=TRUE, ntree=2000)

varImpPlot(RandF_1)

PredRandF_1 <- predict(RandF_1, test, type = "class")
SolRandF_1 <- data.frame(PassengerId = test$PassengerId, Survived = PredRandF_1)
write.csv(SolRandF_1, "SolRandF_1.csv", row.names = FALSE) # Score = 0.78469
