---
title: "Titanic_survival_analysis_report"
author: "YutongLIU"
date: "2016年12月19日"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This is the first time I tried to do a data analysis practice by myself after data science course. Any comments and recommendation about this practice is welcome :)

Here is the agenda of this report

#### 1 Introduction  
#### 2 Defining the Question
  + 2.1 What is the outcome we want?
  + 2.2 What data can we use to answer the question?  

#### 3 Feature Engineering  
  + 3.1 Missing data imputation
  + 3.2 Which varialbes may be relevant to the outcome?
    * 3.2.1 Higher Passenger class, more possible to survive?
    * 3.2.2 Children and women first?
    * 3.2.3 Families can help each other?
    * 3.2.4 Is cabin location relevant to the passenger survival?
    * 3.2.5 Is there any more information we can grab from passengers name?

#### 4 Prediction  
#### 5 Conclusions


> #### 1 Introduction

With this dataset, let's find out if it's possible to predict Titanic passengers' survival based on their profile.

Firstly I downloaded the dataset here [kaggle](https://www.kaggle.com/c/titanic)

##### Set working directory
```{r preparation,results = 'hide',warning=FALSE,message=FALSE}
# Set working directory
if(!dir.exists("D:/kaggle/kaggle_Titanic"))
        dir.create("D:/kaggle/kaggle_Titanic")
setwd("D:/kaggle/kaggle_Titanic")

# Load packages
library(dplyr)   # cleaning dataset
library(ggplot2) # drawing graphics for exploratory analysis
library(rpart)   # missing data imputation
library(caret)   # prediction
library(randomForest) #prediction
```

##### Loading training dataset

Here are the reasons why I save integer variables in different formats
  + **Survived**: I think it's nomial variable, so I save it as factor.
  + **Pclass**  : I think it's ordinal variable, so I save it as integer.
  + **Sibsp** and **Parch**: maybe I will try to calculate some new variables, so keep them as integer.
  
```{r download_read_dataset,results = 'hide'}
# Download and read dataset
colclass <- c("integer","factor","integer","character",
            "factor","numeric","integer","integer",
            "character","numeric","character","factor")
train_raw <- read.csv(file="./train.csv", header = TRUE,
                      colClasses = colclass,
                       na.strings = c("","NA"),stringsAsFactors = FALSE)  

# Do the same process to the test set
# There is no "survived" variable, so change the column class
colclass_test<-c("integer","integer","character",
            "factor","numeric","integer","integer",
            "character","numeric","character","factor")
test_raw <- read.csv(file="./test.csv", header=TRUE, 
                     colClasses=colclass_test,
                      na.strings = c("","NA"),stringsAsFactors = FALSE)
```

Now let's have a glance at the dataset 
```{r data_structure,results = 'hide'}
str(train_raw)
```

There are 11 variables(**PassengerID**　excluded), here is the definition of each variables.

Variable|Description
--------|-------------------------------------------------------------------
survival|Survival(0 = No; 1 = Yes)
pclass  |Passenger Class(1 = 1st; 2 = 2nd; 3 = 3rd)
name    |Passenger name
sex     |Sex
age     |Age
sibsp   |Number of siblings/spouses aboard
parch   |Number of parents/children aboard
ticket  |Ticket number
fare    |Passenger fare
cabin   |Cabin
embarked|Port of Embarkation(C = Cherbourg; Q = Queenstown; S = Southampton)

> #### 2 Defining the Question

   + 2.1 What is the outcome we want?
Clearly it is important to set the target of data analysis before anything gets start.
We want to predict the survival of Titanic passengers. In the dataset, **pclass** is the outcome we will predict in testset.  

   + 2.2 What data can we use to answer the question?
There are 10 variables, two of them, **age** and **sex**, are demographic information, there is also passengers' name. I think another 7 varibales can be categorized to information about their trip: **embarked**, where did they get on the boat; **sibsp** and **parch**, were they alone or accompanied; **pclass**, which class are they, etc.


> #### 3 Feature Engineering

  + 3.1 Dataset cleaning and Missing data imputation

### 3.1.1 Dataset cleaning
#### Combination
To ensure the preprocess is the same to train and also test set, now we combine them together.
```{r combination,results = 'hide'}
total_raw <- bind_rows(train_raw,test_raw)

# Save dataset in a new data frame
total_clean <- total_raw
```

#### Text Editing  
Firstly, let's check the variable names, are they easy to use? Fortunately there is no space among the variable names. Now I just standardize them in lower case.
```{r text_edit,results = 'hide'}
# Standardize all variable names in lower case and check
names(total_clean) <- tolower(names(total_raw))
colnames(total_clean)
```

### 3.1.2 Missing value inputation
#### a) Counting missing values in each variable
```{r counting_missing_values,results = 'hide'}
na_count<-summary(apply(total_clean,2,is.na))
```

From the table below, we learned that there are NA values in several variables
Because the dataset contains the test set, there should be missing values in **survived** so it won't be shown in the table, and **passengerid** won't be used as predictor, therefore it won't be shown in the table neither.

Variables                        |NA values #
---------------------------------|------------------
`r dimnames(na_count)[[2]][[3]]` |`r na_count[,3]`
`r dimnames(na_count)[[2]][[4]]` |`r na_count[,4]`
`r dimnames(na_count)[[2]][[5]]` |`r na_count[,5]`
`r dimnames(na_count)[[2]][[6]]` |`r na_count[,6]`
`r dimnames(na_count)[[2]][[7]]` |`r na_count[,7]`
`r dimnames(na_count)[[2]][[8]]` |`r na_count[,8]`
`r dimnames(na_count)[[2]][[9]]` |`r na_count[,9]`
`r dimnames(na_count)[[2]][[10]]`|`r na_count[,10]`
`r dimnames(na_count)[[2]][[11]]`|`r na_count[,11]`
`r dimnames(na_count)[[2]][[12]]`|`r na_count[,12]`

#### b) Finding observations that contain missing value and process

##### 1 Age
There are 263 NA values in variable **age**.
There is a peak in the age of about 20 but I think the shape of age distribution approximate to the previous one.
```{r age_imputation}
set.seed(123)
factor_name<-c("survived","sex")
total_clean[factor_name]<-lapply(total_clean[factor_name], function(x) as.factor(x))
# Age missing value imputation with rpart
fit_age <- rpart(age ~ pclass+sex+sibsp+parch,
               data=total_clean[!is.na(total_clean$age),3:12],
               na.action=na.omit,method = "anova")
age_result <- predict(fit_age,total_clean[,3:12])
age_result <- round(age_result)
imp_age<-total_clean[,6]
imp_age[is.na(imp_age)]<-age_result[is.na(imp_age)]

# plot age distribution
par(mfrow=c(1,2))
hist(total_raw$Age, freq=F, main='Age: Original Data',col='blue', 
     ylim=c(0,0.07))
hist(imp_age$age, freq=F,main='Age: Imputed Data',col='lightblue', 
     ylim=c(0,0.07))

#impute missing value in age
total_clean$age<-imp_age$age
sum(is.na(total_clean$age))
```

##### 2 Fare
After checking the observation with NA in fare, we find that this passenger is third class and embarked from Southampton. Based on the distribution of third class ticket fare from Southampton, we use the median to impute the NA value.
```{r fare_imputation}
# Check the observation with NA in fare
total_clean[is.na(total_clean$fare),1:12]

# distribution of fare
g_fare<-ggplot(data =total_clean[which(total_clean$pclass==3),], 
               aes(embarked,fare))
g_fare+geom_boxplot()
total_clean[is.na(total_clean$fare),"fare"]<-
        median(total_clean[total_clean$pclass=="3"&total_clean$embarked =="S",]$fare,na.rm = TRUE)
```

##### 3 Embarked
Two observations with missing value in embarked have the same ticket fare: 80
From the plot, this fare appears more likely among passengers emarked from Cherbourg. So these two observations embarked missing value are assigned to 'C'.
```{r embarked}
# Find the missing value in which observation
total_clean[is.na(total_clean$embarked),]
# Fare vs Embarked
gfare <- ggplot(total_clean[total_clean$pclass==1,],aes(x=embarked,y=fare))
gfare+geom_boxplot()

total_clean$embarked[c(62, 830)] <- 'C'
```

#### 3.2 Which varialbes may be relevant to the outcome?
##### 3.2.1 Higher Passenger class, more possible to survive?
From the barchart, it seems that higher passenger class is, more possible to survive.

```{r pclass_survival}
gpclass<-ggplot(total_clean[1:891,],aes(pclass,fill = survived))
gpclass+geom_bar()
```
##### 3.2.2 Children and women first?
Obviously female passengers are more likely to survive.
```{r gender_survive}
ggender<-ggplot(total_clean[1:891,],aes(sex,fill = survived))
ggender+geom_bar()
```

At first I drew an area plot to find insights but the plot was not easy to read.  
So I divide variable **age** into several intervals, plot it with survival status and calculate the proportion of survival in each age group. It seems clear that child is more possible to survive.
```{r age_survive}
#gage <- ggplot(total_clean[1:891,],aes(age,fill = survived))
#gage+geom_area(stat = "bin")

gage_interval <- ggplot(total_clean[1:891,],aes(cut(age,10),fill = survived))
gage_interval + geom_bar() + labs(title = "Age vs Survival", x = "Passenger age interval", y = "Count")

# Survival Proportion of each age group
age_survival <- table(cut(head(total_clean$age,891),10),head(total_clean$survived,891))
age_survival[1:10,2]/apply(age_survival,1,sum)
```

From the table, I found that there is a little peak in 32-40 age group. Maybe because young child couldn't leave alone so they were accompanied by their mother and most of these 32~40-year-old passengers are children mother. From the plot below, it seems like what I thought, this group of female passengers are more possible to survive than the male group in the same age and also other age group of women :)
```{r children_mother}
gage_interval + geom_bar()+facet_grid(.~sex)+
        labs(title = "Age vs Survival", x = "Passenger age interval", y = "Count")
total_clean<-mutate(total_clean,agegroup=cut(age,10))
```

##### 3.2.3 Families can help each other?
We assume that **parch+sibsp** means the family size of passengers and family can help each other during disaster.  
From the plot, we can see that passengers who traveled with family were more possible to survive, however it works well just for family size 1-3.
```{r family_size}
# Create a new feature called familynumber
family_size<-total_clean$sibsp+total_clean$parch
total_clean<-mutate(total_clean,familynumber=family_size)

gfamily<-ggplot(total_clean[1:891,],aes(familynumber,fill=survived))
gfamily+geom_bar()

family_size[family_size > 3]<-"large"
family_size[family_size > 0 & family_size < 4]<-"small"
family_size[family_size == 0]<-"single"
total_clean<-mutate(total_clean,familysize=as.factor(family_size))
```

##### 3.2.4 Is cabin location relevant to the passenger survival?  
I found some information on the internet, it indicates that the initials in cabin number depends on cabin location, class and price. So I tried to grab the initials of cabin.
```{r cabin_capital}
cabin_survived <-  total_clean[1:891,c("survived","cabin")]

# Grab the beginning letter in cabin number
cabin_survived$cabin<-gsub("[0-9]","",cabin_survived$cabin)

# Coerce into a factor vector
unique(cabin_survived$cabin)
cabin_survived$cabin<-as.factor(cabin_survived$cabin)

# Combine factor levels(there are some observation with multiple letter in cabin, like 'B20 B10 B3'
cabin_level<-c("C","E","G","D","A","C","B","F","F","D","B","E","C","B","T","B","E")
levels(cabin_survived$cabin)<-cabin_level
```

From this plot, we can see that almost all passengers with cabin information have a high possibility to survive. But there are too many missing value(about 77%) in cabin, and especially we don't know the reason why there are missing value, e.g. they were just not recorded by mistake, so this variable won't be used as a feature in prediction.
```{r cabin}
# Cabin vs survival 
g_cabin <- ggplot(data = cabin_survived[!is.na(cabin_survived$cabin),],
                aes(cabin,fill=survived))
g_cabin+geom_bar()
```

##### 3.2.5 Is there any more information we can grab from passengers name?
After observing the name variable, I grab the title from passenger name.  
Those people with special title had more possiblities to survive.
```{r title}
# Variable "Name" contains two types of information: 
# name and also sex&marital status
# Extract sexmarital status

nametitle<-gsub("(.*, )|(\\..*)","",x=total_clean$name)

#As the factor levels in the train dataset may be different from the test set
#So we don't coerse this variable into factor.

#there are some repeated title so we combined those titles.
special_male<-c("Capt","Col","Don","Dr","Jonkheer",
                "Major","Master","Rev","Sir","the Countess")
special_female<-c("Lady","Dona","Mlle","Mme","Ms")
nametitle[nametitle %in% special_male]<- "Special_male"
nametitle[nametitle %in% special_female]<- "Special_female"

total_clean<-mutate(total_clean,title=nametitle)
total_clean$title<-as.factor(total_clean$title)
```

### Feature & Model selection 
Based on exploratory analysis, pclass, sex, age, fare, title and familysize are selected to train model.
For this classification question, I train the model using random forest.
```{r split_data}
# Split back into train and test sets
trainset<-total_clean[1:891,]
testset<-total_clean[892:1309,]
```

```{r model}
model_rf1<-randomForest(survived ~ pclass + sex + age + fare +
                          title + familysize,
                  data = trainset)
```

### Evaluation
In sample error, accuracy 0.89, and title, sex, fare and age are top 4 important variables.
```{r prediction}
rf1_result<-predict(model_rf1,trainset)
confusionMatrix(trainset$survived,rf1_result)

importance(model_rf1)
```


### Predict with test data
```{r predict_test_data}
# pay attention to the type of every predictor!
testset<-total_clean[892:1309,]

test_result<-predict(model_rf1,testset)
titanic_rf_submission<-
        data.frame(Passengerid = testset$passengerid,
                   Survived = test_result)
write.csv(x = titanic_rf_submission,
          file = "./titanic_rf_submission7.csv",row.names = FALSE)
```

### Conclusions
Actually I iterated this project for many time, thought about the case, tried to find out the factor that impacts the survival status, did data exploration, processed variables in a different way(e.g. missing value imputation), changed features to train the prediction model. The greatest score I got was about 0.79. I really enjoyed the process.  
From this project, what surprises me is that **title** is the most important feature, not passenger class or gender.  
I think there are other ways to improve the prediction accuracy, any comment and advice is welcome :) 
