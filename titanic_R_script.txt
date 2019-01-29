setwd("C:/KAGGLE_COMPETITION_DATA_SETS/")
titanic_train<-read.csv("train.csv",stringsAsFactors = FALSE,header = TRUE)
titanic_test<-read.csv("test.csv",stringsAsFactors = FALSE,header = TRUE)
str(titanic_train)
str(titanic_test)
median(titanic_train$Age,na.rm = T)->train_median
median(titanic_test$Age,na.rm=T)
titanic_train$IsTrainSet<-TRUE
titanic_test$IsTrainSet<-FALSE
titanic_test$Survived<-NA
titanic.full<-rbind(titanic_train,titanic_test)
table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked=='', "Embarked"]<-'S'
##Categorical Casting##
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)
titanic.full$Sex<-as.factor(titanic.full$Sex)
str(titanic.full)
#cleaning missing values#
titanic.full[(is.na(titanic.full$Age)),"Age"]<-train_median
table(is.na(titanic.full$Fare))
median(titanic.full$Fare,na.rm=T)->Fare_median
table(is.na(titanic.full))

## Spliting back to Train and Test##
titanic_test<-titanic.full[!titanic.full$IsTrainSet==TRUE,]
titanic_train<-titanic.full[titanic.full$IsTrainSet==TRUE,]

str(titanic_test)
str(titanic_train)

titanic_train$Survived<-as.factor(titanic_train$Survived)
##predictive model##
survived.equation<-"Survived~Pclass + Age + SibSp + Fare + Embarked + Parch"
survived.formula<-as.formula(survived.equation)

model<-randomForest(formula=survived.formula,data=titanic_train,ntree=700, mtry=5, nodesize=0.01*nrow(titanic_test))
survived.features<-"Pclass + Age + SibSp + Fare + Embarked + Parch"
predict(model,newdata = titanic_test)->Survived
Survived
plot(Survived)
plot(model)

PassengerId<-titanic_test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived


write.csv(output.df,file="Titanic_submission.csv",row.names = FALSE)
