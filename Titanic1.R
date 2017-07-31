#Kaggle Competition : Titanic Survival
#By Samtha Reddy
setwd("~/R/Kaggle/Titanic")
file <- file.path("train.csv") #loading training data
mydata <- read.csv(file)
mydata

file <- file.path("test.csv") #loading test data
mytestdata <- read.csv(file)
mytestdata

str(mydata)
mydata$Survived #isolating one column and check its values...
table(mydata$Survived) # passing the vector to function table. most basic summary statistics functions in R, 
#it runs through the vector you gave it and simply counts the occurrence of each value in it. 
prop.table(table(mydata$Survived))

str(mytestdata)
mytestdata$Survived <- rep(0,418)# We are assuming all died in test data. Since there was no "Survived" column in the dataframe, 
#it will create one for us and repeat our "0" prediction 418 times, the number of rows we have.
#If this column already existed, it would overwrite it with the new values.
mytestdata #checking if our new column got added or not

submit <- data.frame(PassengerId <- mytestdata$PassengerId, Survived <- mytestdata$Survived)
submit
write.csv(submit,file = "theyallperish.csv",row.names=FALSE) # writing the dataframe to a file
#CONCLUSION 1: train data had data.frame':	891 obs. where approx 66% died, so we predectiong 491 data in test also died..so based on
#survived column we did our prediction

table(mydata$Sex)
table(mydata$Survived) 
# by default takes each entry in the table and divides by the total number of passengers. 

#What we want to see is the row-wise proportion, ie, the proportion of each sex that survived, as separate groups.
prop.table(table(mydata$Sex, mydata$Survived),1) # ,1 means give row wise proportion ,2 means give columnwisw proportion

#See we now see majority of females survived compared to male , so changing my earlier prediction.
mytestdata$Survived <- 0 # same result as rep command
mytestdata$Survived[mytestdata$Sex== 'female'] <- 1 # []used to susbset the data of dataframe
mytestdata

submit <- data.frame(PassengerId <- mytestdata$PassengerId, Sex <- mytestdata$Sex,Survived <- mytestdata$Survived)
write.csv(submit,file = "femalesurvive.csv" , row.names=FALSE)

#CONCLUSION 2: All females survive on test data

#Now based in Age

summary(mydata$Age) # 5 point summary of column age
# we see 177 NA...i.e. blanks..we assume their age to be around mean 29.70

mydata$child <- 0 
mydata$child[mydata$Age<18] <- 1 # wherever age missing , those are replaced by 0 as R treats NA<18 as false
table(mydata$Sex,mydata$child,mydata$Survived) # But we want see as how many female child survived or how many male child survived
# so use aggregrate function:

aggregate(Survived~Sex + child, data=mydata, FUN=sum )# adds the survived col values and female child /female adult..since survived values
# are 0 +1..and 1 being survived, we get total survived..
# so it shows as if more female adult survived compared more male adults and same is the case for child

#suppose i wanna know among the total females who survived ...how many where child..being child added to their adv?..but maybe 
# but maybe there were more peopele in the group which shows as more survivors [grp being male adult , male child etc]  so we need to what % of each grp survived..so first we need to know
# how many are there in each grp.


aggregate(Survived~Sex + child, data=mydata, FUN=length) #length simply counts the records in each grp
aggregate(Survived~Sex + child, data=mydata, function(x){sum(x)/length(x)})
#CONCLUSION 3: Among  each grp we see female had adv  so no change from the previous prediction

# Now look at ticket price
#as fares continous variables...we need change to factors by creating bins
mydata$Fare2 <- '30+' # New col created
mydata$Fare2[mydata$Fare < 30 & mydata$Fare >= 20] <- '20-30'
mydata$Fare2[mydata$Fare < 20 & mydata$Fare >= 10] <- '10-20'
mydata$Fare2[mydata$Fare < 10] <- '<10'


aggregate(Survived~Fare2 + Pclass + Sex , data=mydata, function(x){sum(x)/length(x)} )

# While the majority of males, regardless of class or fare still don't do so well, we notice that
#most of the class 3 women who paid more than $20 for their ticket actually also miss out on a lifeboat, ..there are suvival is less
#than 50%...so lets apply this knowledge to test data

mytestdata$Survived <- 0 # Assuming none survived
mytestdata$Survived[mytestdata$Sex == 'female'] <- 1 #Females does survive
mytestdata$Survived[mytestdata$Sex == 'female' & mytestdata$Pclass==3 & mytestdata$Fare >= 20] <- 0 # Few exceptions among femal 
#what we got from training data

submit <- data.frame(mytestdata)

write.csv(submit,"Prediction3.csv")


#
setwd("~/R/Kaggle/Titanic")
file <- file.path("train.csv")
mydata <- read.csv(file)
mydata

file <- file.path("test.csv")
mytestdata <- (read.csv(file))
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = mydata,
             method = "class") # building the decision tree...here method class means classification tree

plot(fit)
text(fit)

install.packages(RGtk2)
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library('rattle')
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, mytestdata, type = "class")
submit <- data.frame( PassengerId= mytestdata$PassengerId , Name = mytestdata$Name, Survived = Prediction )
write.csv(submit, file = "myfirstdecisiontree.csv", row.names = FALSE )

file1 <- file.path("train.csv")
my.data <- read.csv(file1)
my.data
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = my.data,
             method = "class",
             control = rpart.control(cp=0, minsplit = 2))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


  mydata$Name[1]
  mytestdata$Survived <- NA
  combdata <- rbind(mydata, mytestdata)
  combdata$Name <- as.character(combdata$Name)# chaging text back to string from factor
  combdata$Name[1]
  
  --------------------------------6/3/2017---------------------------------------------
    
    setwd("~/R/Kaggle/Titanic")
  file <- file.path("train.csv")
  my.data <- read.csv(file)
str(my.data)    

file1 <- file.path("test.csv")
my.testdata <- read.csv(file1)
str(my.testdata)

my.data$Name[1]

my.testdata$Survived <- NA
combi.data <- rbind(my.data,my.testdata)
str(combi.data)

combi.data$Name <- as.character(combi.data$Name)
combi.data$Name[1]

strsplit(combi.data$Name, split='[,.]')
strsplit(combi.data$Name, split='[,.]')[[1]][2]

combi.data$Title <- strsplit(combi.data$Name, split='[,.]')[[1]][2]# this would just apply first rows's split to all rows..which is incorrect

combi.data$Title <- sapply(combi.data$Name , FUN=function(x){strsplit(x, split='[,.]')[[1]][2]})

combi.data$Title <- sub(" ","",combi.data$Title)
table(combi.data$Title)

combi.data$Title[combi.data$Title %in% c('Mme','Mlle')] <- 'Mlle'
--------------------------------------------------------------------------------------------------------
  
  6/4/2017

setwd("~/R/Kaggle/Titanic")

file <- file.path("train.csv")
my.data <- read.csv(file)
str(my.data)

file <- file.path("test.csv")
mytest.data <- read.csv(file)
str(mytest.data)

mytest.data$Survived <- NA

my.data$Name

# combining test and train data is imp to undersatnd titles , surnames, family size across entire data set..we will later split the datasets again to test and train

combi.data <- rbind(my.data , mytest.data)

strsplit(combi.data$Name[[1]], split='[,.]')
combi.data$Name <- as.character(combi.data$Name)
str(combi.data$Name)

strsplit(combi.data$Name, split='[,.]')[[1]][2]

combi.data$Title <- sapply (combi.data$Name ,FUN = function(x){ strsplit(x, split='[,.]')[[1]][2]})


combi.data$Title <- sub(" ","",combi.data$Title)

# now try and see what all titles can you combine

table(combi.data$Title)


combi.data$Title[combi.data$Title %in% c("Mme","Mlle")] <- "Mlle"
combi.data$Title [combi.data$Title %in% c('Capt','Col','Major','Don','Sir','JonKheer')] <- 'Military'
combi.data$Title [combi.data$Title %in% c('Lady','the Countess','Dona')] <- 'Lady'


# as these title are to be used by decision trees as categories , we need to convert them back to factors

combi.data$Title <- factor(combi.data$Title)
#now split



my.data <- combi.data[1:891,]
mytest.data <- combi.data[892:1309,]

library('rpart')
install.packages('rattle')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title + Embarked , 
             data= my.data,
             method ="class"    )


fancyRpartPlot(newfit)
Prediction <- predict(fit, mytest.data, type = "class")

submit <- data.frame( PassengerId = mytest.data$PassengerId , Survived = Prediction)
write.csv(submit,"Dec_tree_title_added.csv", row.names = FALSE)


#-------------now using family size logic large families have difficulty to survive-----------------------
  
  combi.data$FamilySize <- combi.data$Parch + combi.data$SibSp + 1

combi.data$Surname <- sapply( combi.data$Name , FUN = function(x){strsplit(x, split='[,.]')[[1]][1]})

combi.data$FamilyId <- paste(as.character(combi.data$FamilySize) ,combi.data$Surname, sep="")

table(combi.data$FamilyId)
combi.data$FamilyId[combi.data$FamilySize <= 2] <- 'Small' # Observe added as factor at last small=1025

famId <- data.frame(table(combi.data$FamilyId))
famId <- famId[famId$Freq <= 2,]

combi.data$FamilyId[combi.data$FamilyId %in% famId$Var1] <- 'Small' # now we small increased to 1074
combi.data$FamilyId <- factor(combi.data$FamilyId)

my.data <- combi.data[1:891,]
mytest.data <- combi.data[892:1309,]



fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize + FamilyId,
             data = my.data,
             method ="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, mytest.data, type ="class")
submit <- data.frame(PassengerId = mytest.data$PassengerId , Survived = Prediction)
write.csv(submit ,"dec_tree_familysize_add.csv", row.names = FALSE)

#======================================6/5/2017==========================================================

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data = combi.data[!is.na(combi.data$Age),],
                method ="anova")

combi.data$Age[is.na(combi.data$Age)]<- predict(Agefit, combi.data[is.na(combi.data$Age),])
summary(combi.data$Age)

summary(combi.data)


which(combi.data$Embarked =='')
combi.data$Embarked[c(62,830)] = "S"
combi.data$Embarked <- factor(combi.data$Embarked)

which(is.na(combi.data$Fare))
combi.data$Fare[1044] <- median(combi.data$Age , na.rm=TRUE)

combi.data$FamilyId2 <- combi.data$FamilyId
table(combi.data$FamilyId2)

combi.data$FamilyId2 <- as.character(combi.data$FamilyId2)
combi.data$FamilyId2[combi.data$FamilySize <= 3] <- 'Small'
combi.data$FamilyId2 <- factor(combi.data$FamilyId2)

install.packages('randomForest')
library(randomForest)

set.seed(145)

#make sure to spilt data

my.data <- combi.data[1:891,]
mytest.data <- combi.data[892:1309,]


fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title + Embarked + FamilySize + FamilyId2,
                    data = my.data,
                    importance=TRUE,
                    ntree = 2000)

varImpPlot(fit)

Prediction <- predict(fit, mytest.data)

submit <- data.frame(PassengerID = mytest.data$PassengerId , Survived = Prediction)

write.csv(submit , file = "myfirstRF.csv" , row.names = FALSE)



  install.packages('party')
  library('party')
  
  set.seed(145)
  
  fit <- cforest(as.factor(Survived) ~ Pclass + Age  + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId,
                 data = my.data,
                 controls = cforest_unbiased(ntree = 2000, mtry =3))
  
  Prediction <- predict(fit,mytest.data,00B = TRUE , type = "response" )
  
  
  submit <- data.frame(PassengerID = mytest.data$PassengerId , Survived = Prediction)
  
  write.csv(submit , file = "myfirstcf.csv" , row.names = FALSE)
  
  
  
  
                 )
  