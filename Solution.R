## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rio)
library(ggcorrplot)
library(rpart.plot)
library(ISLR)
library(rpart)
library(C50)
library(cowplot)


## --------------------------------------------------------------------------------------------------------------------------------------------
df <- import("German Credit.xls")%>%
  as_tibble()


## --------------------------------------------------------------------------------------------------------------------------------------------
str(df)


## --------------------------------------------------------------------------------------------------------------------------------------------
summary(df)


## --------------------------------------------------------------------------------------------------------------------------------------------
na <- is.na(df)%>%
  head(5)
na


## --------------------------------------------------------------------------------------------------------------------------------------------
summary(as.factor(df$RESPONSE))


## --------------------------------------------------------------------------------------------------------------------------------------------
#For HISTORY#
#For instance, '1: all credits at this bank paid back duly' has been renamed as 'ALL DUES PAID' for shorter names and to have a readable format while plotting graphs.

data_new<-df %>%
  mutate(HISTORY=replace(HISTORY, HISTORY==0,"NO CREDITS TAKEN"))%>%
  mutate(HISTORY=replace(HISTORY, HISTORY==1,"ALL DUES PAID")) %>%
  mutate(HISTORY=replace(HISTORY, HISTORY==2,"EXISTING DUES PAID")) %>%
  mutate(HISTORY=replace(HISTORY, HISTORY==3,"DUES DELAYED")) %>%
  mutate(HISTORY=replace(HISTORY, HISTORY==4,"CRITICAL ACCOUNT"))


## --------------------------------------------------------------------------------------------------------------------------------------------
#For CHK_ACCT#

data_new<-data_new %>%
  mutate(CHK_ACCT=replace(CHK_ACCT, CHK_ACCT==0,"LESS THAN 0"), CHK_ACCT=replace(CHK_ACCT, CHK_ACCT==1,"BETWEEN 0 AND 200"),CHK_ACCT=replace(CHK_ACCT, CHK_ACCT==2,"GREATER THAN EQUAL TO 200"),CHK_ACCT=replace(CHK_ACCT, CHK_ACCT==3,"NO CHECKING ACCOUNT"))


## --------------------------------------------------------------------------------------------------------------------------------------------
#For SAV_ACCT#

data_new<-data_new %>%
  mutate(SAV_ACCT=replace(SAV_ACCT, SAV_ACCT==0,"LESS THAN 100"),SAV_ACCT=replace(SAV_ACCT, SAV_ACCT==1,"BETWEEN 100 and 500"),SAV_ACCT=replace(SAV_ACCT, SAV_ACCT==2,"BETWEEN 500 AND 1000"),SAV_ACCT=replace(SAV_ACCT, SAV_ACCT==3,"GREATER THAN EQUAL TO 1000"),SAV_ACCT=replace(SAV_ACCT, SAV_ACCT==4,"NO SAVINGS"))


## --------------------------------------------------------------------------------------------------------------------------------------------
#For EMPLOYMENT#

data_new<-data_new %>%
  mutate(EMPLOYMENT=replace(EMPLOYMENT, EMPLOYMENT==0,"UNEMPLOYED"),EMPLOYMENT=replace(EMPLOYMENT, EMPLOYMENT==1,"LESS THAN 1 YR"),EMPLOYMENT=replace(EMPLOYMENT, EMPLOYMENT==2,"BETWEEN 1 AND 4 YRS"),EMPLOYMENT=replace(EMPLOYMENT, EMPLOYMENT==3,"EMPLOYED BETWEEN 4 AND 7 YRS"),EMPLOYMENT=replace(EMPLOYMENT, EMPLOYMENT==4,"EMPLOYED MORE THAN 7 YRS"))


## --------------------------------------------------------------------------------------------------------------------------------------------
#For PRESENT_RESIDENT#

data_new<-data_new %>%
  mutate(PRESENT_RESIDENT=replace(PRESENT_RESIDENT, PRESENT_RESIDENT==1,"LESS THAN 1 YEAR"),PRESENT_RESIDENT=replace(PRESENT_RESIDENT, PRESENT_RESIDENT==2,"GREATER THAN EQUAL TO 2 YEARS"),PRESENT_RESIDENT=replace(PRESENT_RESIDENT, PRESENT_RESIDENT==3,"BETWEEN 2 AND 3 YEARS"),PRESENT_RESIDENT=replace(PRESENT_RESIDENT, PRESENT_RESIDENT==4,"MORE THAN 4 YEARS"))


## --------------------------------------------------------------------------------------------------------------------------------------------
#For JOB#

data_new<-data_new %>%
  mutate(JOB=replace(JOB, JOB==0,"UNEMP/UNSKILLED/NON-RES"), JOB=replace(JOB, JOB==1,"UNSKILLED RES"),JOB=replace(JOB, JOB==2,"SKILLED EMP/ OFFICIAL"),JOB=replace(JOB, JOB==3,"SELF/HIGHLY QUALIFIED EMP"))


## --------------------------------------------------------------------------------------------------------------------------------------------
#For TELEPHONE,FOREIGN, RESPONSE, OTHER_INSTALL#

data_new<-data_new%>%
  mutate(TELEPHONE = ifelse(TELEPHONE == 0, "NO","YES"), FOREIGN = ifelse(FOREIGN == 0,"NO","YES"), RESPONSE = ifelse(RESPONSE == 0, "NO","YES"), OTHER_INSTALL = ifelse(OTHER_INSTALL == 0,"NO","YES"))


## --------------------------------------------------------------------------------------------------------------------------------------------
sapply( data_new[ sapply(data_new, is.character)], table)


## --------------------------------------------------------------------------------------------------------------------------------------------
summary(data_new)


## --------------------------------------------------------------------------------------------------------------------------------------------
#PURPOSE

data_new<-unite(data_new, "PURPOSE",NEW_CAR:RETRAINING, remove = T,sep = "")
data_new<-data_new%>%
  mutate(PURPOSE = ifelse(PURPOSE == "000100","RADIO/TV",PURPOSE), PURPOSE = ifelse(PURPOSE == "000010","EDUCATION",PURPOSE), PURPOSE = ifelse(PURPOSE == "100000","NEW CAR",PURPOSE), PURPOSE = ifelse(PURPOSE == "010000","USED CAR",PURPOSE), PURPOSE = ifelse(PURPOSE == "001000","FURNITURE",PURPOSE), PURPOSE = ifelse(PURPOSE == "000001","RETRAINING",PURPOSE), PURPOSE = ifelse(PURPOSE == "000000","OTHER",PURPOSE))


## --------------------------------------------------------------------------------------------------------------------------------------------
#STATUS

data_new<-unite(data_new, "STATUS", MALE_DIV:MALE_MAR_or_WID, remove = T, sep = "")
data_new<-data_new%>%
  mutate(STATUS = ifelse(STATUS == "000", "OTHER(FEMALE)", STATUS), STATUS = ifelse(STATUS == "100", "MALE DIVORCED", STATUS), STATUS = ifelse(STATUS == "010", "MALE SINGLE", STATUS), STATUS = ifelse(STATUS == "001", "MALE MARRIED OR WIDOWED", STATUS))


## --------------------------------------------------------------------------------------------------------------------------------------------
#OTHER PARTIES

data_new<-unite(data_new, "OTHER_PARTIES", 'CO-APPLICANT':GUARANTOR, remove = T, sep = "")
data_new<-data_new%>%
  mutate(`OTHER_PARTIES` = ifelse(`OTHER_PARTIES` == "00", "NONE", `OTHER_PARTIES`), `OTHER_PARTIES` = ifelse(`OTHER_PARTIES` == "01", "GUARANTOR", `OTHER_PARTIES`), `OTHER_PARTIES` = ifelse(`OTHER_PARTIES` == "10", "CO-APPLICANT", `OTHER_PARTIES`))


## --------------------------------------------------------------------------------------------------------------------------------------------
#ASSETS

data_new<-unite(data_new, "ASSETS", REAL_ESTATE:PROP_UNKN_NONE, remove = T, sep = "")
data_new<-data_new%>%
  mutate(ASSETS = ifelse(ASSETS == "00", "NONE", ASSETS), ASSETS = ifelse(ASSETS == "01", "NO PROPERTY", ASSETS), ASSETS = ifelse(ASSETS == "10", "REAL ESTATE", ASSETS))


## --------------------------------------------------------------------------------------------------------------------------------------------
#HOUSING

data_new <- unite(data_new, "HOUSING", RENT:OWN_RES, remove = T, sep = "")
data_new<-data_new%>%
  mutate(HOUSING = ifelse(HOUSING == "00","NONE",HOUSING), HOUSING = ifelse(HOUSING == "01","OWN RESIDENCE",HOUSING), HOUSING = ifelse(HOUSING == "10","RENTAL",HOUSING) )


## --------------------------------------------------------------------------------------------------------------------------------------------
summary(df)
summary(data_new)


## --------------------------------------------------------------------------------------------------------------------------------------------
#AGE
data_new %>%
  ggplot()+
  geom_boxplot(aes(y = AGE, fill = RESPONSE))+
  labs(title = "Age vs Response")


## --------------------------------------------------------------------------------------------------------------------------------------------
#AMOUNT
ggplot(data = data_new)+
  geom_density(aes(x = AMOUNT, fill = RESPONSE),alpha = 0.7)+
  scale_fill_brewer(palette = "Pastel1")+
  scale_y_continuous(breaks = 1)+
  labs(title = "Graph for Amount Credited")


## --------------------------------------------------------------------------------------------------------------------------------------------
#AGE vs AMOUNT
ggplot(data = data_new)+
  geom_point(aes(x = AMOUNT, y = AGE, col = RESPONSE))+
  facet_grid(~RESPONSE)+
  labs(title = "Age, Amount and Response")


## --------------------------------------------------------------------------------------------------------------------------------------------
#NUM_DEPENDENTS
ggplot(data_new, aes(x = NUM_DEPENDENTS, fill = RESPONSE)) + 
  geom_bar(width = 0.9) +
  labs(title = "Number of Dependents vs Response")


## --------------------------------------------------------------------------------------------------------------------------------------------
#EMPLOYMENT
ggplot(data_new, aes(y = EMPLOYMENT, fill = RESPONSE)) + 
  geom_bar() + facet_grid(RESPONSE~.)+
  labs(title = "Graph for Employment Tenure")


## --------------------------------------------------------------------------------------------------------------------------------------------
#PURPOSE
ggplot(data_new) + 
  geom_bar(aes(y = PURPOSE, fill = RESPONSE)) + facet_grid(RESPONSE~.)+
  labs(title = "Graph for Employment Tenure")+
  scale_fill_manual(values = c("steelblue1", "steelblue4"))


## --------------------------------------------------------------------------------------------------------------------------------------------
#ASSETS
ggplot(data_new) + 
  geom_bar(aes(x = ASSETS, fill = RESPONSE)) + facet_grid(RESPONSE~.)+
  labs(title = "Graph for Assest Type")+
  scale_fill_manual(values = c("burlywood", "gray46"))


## --------------------------------------------------------------------------------------------------------------------------------------------
#HISTORY
ggplot(data = data_new)+
  geom_bar(aes(x="",y = RESPONSE, fill = HISTORY),width = 1, stat = "identity", position = "stack")+
  coord_polar("y")+
  facet_grid(~RESPONSE)+
  labs(title = "Pie Chart of History vs Response")


## --------------------------------------------------------------------------------------------------------------------------------------------
#HOUSING
ggplot(data_new) +
  geom_bar(aes(x= HOUSING, fill=RESPONSE), width = 0.20, position = "dodge") +
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Housing vs Response")



## --------------------------------------------------------------------------------------------------------------------------------------------
#OTHER_INSTALL
ggplot(data = data_new)+
  geom_bar(aes(x = OTHER_INSTALL, fill = RESPONSE), position = "dodge")+
  scale_fill_brewer(palette = "Set3")+
  labs(title = "Other Install vs Response")


## --------------------------------------------------------------------------------------------------------------------------------------------
#JOB
ggplot(data = data_new)+
  geom_bar(aes(y = JOB, fill = RESPONSE), position = "dodge")+
  labs(title = "Job vs Response")


## --------------------------------------------------------------------------------------------------------------------------------------------
#PRESENT_RESIDENT
ggplot(data = data_new)+
  geom_bar(aes(x="",y = RESPONSE, fill = PRESENT_RESIDENT),width = 1, stat = "identity", position = "stack")+
  coord_polar("y")+
  facet_grid(~RESPONSE)+
  labs(title = "Present Resident vs Response")


## --------------------------------------------------------------------------------------------------------------------------------------------
#STATUS
p1 <- ggplot(data_new, aes(x = STATUS)) + geom_bar() + 
  labs(title = 'Bar Chart indicating status', x = "Gender/Status") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2 <- ggplot(data_new, aes(x = STATUS, fill = RESPONSE)) + 
  geom_bar(width=0.35) + facet_grid(RESPONSE~.) +
  labs(title = '', x = "Gender/Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(p1, p2, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#OTHER_PARTIES
p3 <- ggplot(data_new, aes(x = OTHER_PARTIES)) + geom_bar() + 
  labs(title = 'Bar Chart of Other Parties Involved') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p4 <- ggplot(data_new, aes(x = OTHER_PARTIES, fill = RESPONSE)) + 
  geom_bar() + facet_grid(RESPONSE~.) +
  labs(title = '') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(p3, p4, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#FOREIGN
a1 <- ggplot(data_new, aes(x = FOREIGN)) + geom_bar() + 
  labs(title = 'Bar Chart of Foreign Worker') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

a2 <- ggplot(data_new, aes(x = FOREIGN, fill = RESPONSE)) + 
  geom_bar() + facet_grid(RESPONSE~.) +
  labs(title = '') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(a1, a2, ncol = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#Multivariate (STATUS, HISTORY, NUM_DEPENDENTS)

ggplot(data_new) + 
  geom_bar(aes(x = HISTORY, fill = RESPONSE), position = 'dodge') + facet_grid(factor(NUM_DEPENDENTS)~STATUS) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Bar Chart: Credit History, Status and Number of dependants', x = "HISTORY", fill = "RESPONSE")


## --------------------------------------------------------------------------------------------------------------------------------------------
#Multivariate (PURPOSE, HOUSING, PRESENT_RESIDENT)

ggplot(data_new) + 
  geom_bar(aes(x = PURPOSE, fill = RESPONSE), position = 'dodge') + facet_grid(HOUSING~PRESENT_RESIDENT) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Bar Chart', x = "PURPOSE", fill = "RESPONSE")


## --------------------------------------------------------------------------------------------------------------------------------------------
#Multivariate (JOB, STATUS, ASSET)

ggplot(data_new) + 
  geom_bar(aes(x = JOB, fill = RESPONSE), position = 'stack') + facet_grid(STATUS~ASSETS) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Bar Chart', x = "JOB", fill = "RESPONSE")



## --------------------------------------------------------------------------------------------------------------------------------------------
table(data_new$PRESENT_RESIDENT, data_new$RESPONSE)
chisq.test(data_new$PRESENT_RESIDENT, data_new$RESPONSE, correct = F)

table(data_new$TELEPHONE, data_new$RESPONSE)
chisq.test(data_new$TELEPHONE, data_new$RESPONSE, correct = F)

table(data_new$NUM_DEPENDENTS, data_new$RESPONSE)
chisq.test(data_new$NUM_DEPENDENTS, data_new$RESPONSE, correct = F)

table(data_new$NUM_CREDITS, data_new$RESPONSE)
chisq.test(data_new$NUM_CREDITS, data_new$RESPONSE, correct = F)

table(data_new$INSTALL_RATE, data_new$RESPONSE)
chisq.test(data_new$INSTALL_RATE, data_new$RESPONSE, correct = F)


## --------------------------------------------------------------------------------------------------------------------------------------------
# Useful
table(data_new$HISTORY, data_new$RESPONSE)
chisq.test(data_new$HISTORY, data_new$RESPONSE, correct = F)

table(data_new$EMPLOYMENT, data_new$RESPONSE)
chisq.test(data_new$EMPLOYMENT, data_new$RESPONSE, correct = F)

table(data_new$STATUS, data_new$RESPONSE)
chisq.test(data_new$STATUS, data_new$RESPONSE, correct = F)

table(data_new$OTHER_INSTALL, data_new$RESPONSE)
chisq.test(data_new$OTHER_INSTALL, data_new$RESPONSE, correct = F)

table(data_new$FOREIGN, data_new$RESPONSE)
chisq.test(data_new$FOREIGN, data_new$RESPONSE, correct = F)

table(data_new$HOUSING, data_new$RESPONSE)
chisq.test(data_new$HOUSING, data_new$RESPONSE, correct = F)

table(data_new$PURPOSE, data_new$RESPONSE)
chisq.test(data_new$PURPOSE, data_new$RESPONSE, correct = F)

table(data_new$CHK_ACCT,data_new$RESPONSE)
chisq.test(data_new$CHK_ACCT,data_new$RESPONSE, correct = F)

table(data_new$ASSETS, data_new$RESPONSE)
chisq.test(data_new$ASSETS, data_new$RESPONSE, correct = F)

table(data_new$SAV_ACCT, data_new$RESPONSE)
chisq.test(data_new$SAV_ACCT, data_new$RESPONSE, correct = F)



## --------------------------------------------------------------------------------------------------------------------------------------------
#Gini for 0.5 and 0.5 (Default Tree without pruning)

data_new$RESPONSE<- as.factor(data_new$RESPONSE)
#The random seed is set to a fixed value below to make the results reproducible.
set.seed(125)
#Splitting criteria
ind<-sample(2, nrow(data_new), replace =T, prob = c(0.5, 0.5))
train<-data_new[ind==1,]
test<-data_new[ind==2,]

#myFormula specifies that the target RESPONSE is dependent variable while all others (used as ~.) are independent variables. We have excluded the OBS# variable.
myFormula = RESPONSE ~. -`OBS#`
# Default decision tree without using pruning parameters
mytree <- rpart(myFormula, data = train)

#Check Train error
pred_train<-predict(mytree,data=train,type="class")
mean(train$RESPONSE!=pred_train)

#Check Test error
pred_test<-predict(mytree, newdata = test,type="class")
mean(test$RESPONSE!=pred_test)

#See Decision Tree
mytree
#Plot Decision Tree
rpart.plot(mytree)



## --------------------------------------------------------------------------------------------------------------------------------------------
#Parameters to check reliability of the model
cf<-table(actual = test$RESPONSE, pred = pred_test)
cf
accuracy<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
precision<-(cf[2,2]/(cf[2,2]+cf[1,2]))
recall<-(cf[2,2]/(cf[2,2]+cf[2,1]))
accuracy
precision
recall


## --------------------------------------------------------------------------------------------------------------------------------------------
plotcp(mytree)


## --------------------------------------------------------------------------------------------------------------------------------------------
#Gini for 0.5 and 0.5 (Default Tree with pruning)
#for loop
x<-c(1:10)
msplit <- 3
mbucket <- 1

for (val in x) {

myFormula = RESPONSE ~. -`OBS#`
myTree2 <- rpart(myFormula, data = train, control = rpart.control(minsplit = msplit, minbucket = mbucket, cp = 0.01))
cat("Min Split :",msplit, "Min Bucket:", mbucket,"\n") 
  
pred_train<-predict(myTree2,data=train,type="class")
print(mean(train$RESPONSE != pred_train))

pred_test<-predict(myTree2, newdata = test,type="class")
print(mean(test$RESPONSE != pred_test))
print(table(actual = test$RESPONSE, pred = pred_test))

msplit<-msplit + 10
mbucket<-mbucket + 10  
}


## --------------------------------------------------------------------------------------------------------------------------------------------
#Choosing the best prune tree with lowest test error

myTreeprun <- rpart(myFormula, data = train, control = rpart.control(minsplit = 33, minbucket = 31, cp = 0.01))

#Train error
pred_train<-predict(myTreeprun,data=train,type="class")
mean(train$RESPONSE != pred_train)

#Test error
pred_test<-predict(myTreeprun, newdata = test,type="class")
mean(test$RESPONSE != pred_test)

myTreeprun
rpart.plot(myTreeprun)



## --------------------------------------------------------------------------------------------------------------------------------------------
#Parameters to check reliability of the model
cf<-table(actual = test$RESPONSE, pred = pred_test)
cf
accuracy55<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
precision55<-(cf[2,2]/(cf[2,2]+cf[1,2]))
recall55<-(cf[2,2]/(cf[2,2]+cf[2,1]))
accuracy55
precision55
recall55


## --------------------------------------------------------------------------------------------------------------------------------------------
# Gini for 0.8 and 0.2 (Default Tree without prunning)

set.seed(999)
ind<-sample(2, nrow(data_new), replace =T, prob = c(0.8, 0.2))
train<-data_new[ind==1,]
test<-data_new[ind==2,]

myFormula = RESPONSE ~. -`OBS#`

mytree2 <- rpart(myFormula, data = train,parms = list(split = "gini"))

# Train Error 
pred_train<-predict(mytree2,data=train,type="class")
mean(train$RESPONSE!=pred_train)

#Test Error
pred_test<-predict(mytree2, newdata = test,type="class")
mean(test$RESPONSE!=pred_test)

print(mytree2)
rpart.plot(mytree2)

## --------------------------------------------------------------------------------------------------------------------------------------------
# How reliable is the model

cf<-table(actual = test$RESPONSE, pred = pred_test)
cf
accuracy<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
precision<-(cf[2,2]/(cf[2,2]+cf[1,2]))
recall<-(cf[2,2]/(cf[2,2]+cf[2,1]))
accuracy
precision
recall


## --------------------------------------------------------------------------------------------------------------------------------------------
plotcp(mytree2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#Gini for 0.8 and 0.2 (Default Tree with prunning)
x<-c(1:10)
msplit <- 3
mbucket <- 1

for (val in x) {

myFormula = RESPONSE~. -`OBS#`
myTree <- rpart(myFormula, data = train, control = rpart.control(minsplit = msplit, minbucket = mbucket, cp = 0.01))

cat("Min Split :",msplit, "Min Bucket:", mbucket,"\n") 
pred_train<-predict(myTree, data = train, type = "class")
print(mean(train$RESPONSE != pred_train))
pred_test<-predict(myTree, newdata = test, type = "class")
print(mean(test$RESPONSE != pred_test))
print(table(actual = test$RESPONSE, pred = pred_test))

msplit<-msplit + 10
mbucket<-mbucket + 10  
}



## --------------------------------------------------------------------------------------------------------------------------------------------
#choosing the best prunned tree with lowest test error

myTreeprun <- rpart(myFormula, data = train, control = rpart.control(minsplit = 23, minbucket = 21, cp = 0.01))

# Train Error 
pred_train<-predict(myTreeprun,data=train,type="class")
mean(train$RESPONSE!=pred_train)

#Test Error
pred_test<-predict(myTreeprun, newdata = test,type="class")
mean(test$RESPONSE!=pred_test)

print(myTreeprun)
rpart.plot(myTreeprun)


## --------------------------------------------------------------------------------------------------------------------------------------------
# How reliable is the prunned model


cf<-table(actual = test$RESPONSE, pred = pred_test)
cf
accuracy82<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
precision82<-(cf[2,2]/(cf[2,2]+cf[1,2]))
recall82<-(cf[2,2]/(cf[2,2]+cf[2,1]))
accuracy82
precision82
recall82


## --------------------------------------------------------------------------------------------------------------------------------------------
# Gini for 0.75 and 0.25 (Default Tree without prunning)

data_new$RESPONSE<- as.factor(data_new$RESPONSE)

set.seed(777)
ind<-sample(2, nrow(data_new), replace =T, prob = c(0.75, 0.25))
train<-data_new[ind==1,]
test<-data_new[ind==2,]

myFormula = RESPONSE ~. -`OBS#`

mytree2 <- rpart(myFormula, data = train,parms = list(split = "gini"))

# Train Error 
pred_train<-predict(mytree2,data=train,type="class")
mean(train$RESPONSE!=pred_train)

#Test Error
pred_test<-predict(mytree2, newdata = test,type="class")
mean(test$RESPONSE!=pred_test)


print(mytree2)
rpart.plot(mytree2)


## --------------------------------------------------------------------------------------------------------------------------------------------
# How reliable is the model

cf<-table(actual = test$RESPONSE, pred = pred_test)
cf
accuracy<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
precision<-(cf[2,2]/(cf[2,2]+cf[1,2]))
recall<-(cf[2,2]/(cf[2,2]+cf[2,1]))
accuracy
precision
recall



## --------------------------------------------------------------------------------------------------------------------------------------------
plotcp(mytree2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#Gini for 0.75 and 0.25 (Default Tree with prunning)
x<-c(1:10)
msplit <- 3
mbucket <- 1

for (val in x) {

myFormula = RESPONSE~. -`OBS#`
myTree <- rpart(myFormula, data = train, control = rpart.control(minsplit = msplit, minbucket = mbucket, cp = 0.01))

cat("Min Split :",msplit, "Min Bucket:", mbucket,"\n") 
pred_train<-predict(myTree, data = train, type = "class")
print(mean(train$RESPONSE != pred_train))
pred_test<-predict(myTree, newdata = test, type = "class")
print(mean(test$RESPONSE != pred_test))
print(table(actual = test$RESPONSE, pred = pred_test))

msplit<-msplit + 10
mbucket<-mbucket + 10  
}


## --------------------------------------------------------------------------------------------------------------------------------------------
#choosing the best prunned tree with lowest test error

myTreeprun <- rpart(myFormula, data = train, control = rpart.control(minsplit = 43, minbucket = 41, cp = 0.01))

# Train Error 
pred_train<-predict(myTreeprun,data=train,type="class")
mean(train$RESPONSE!=pred_train)

#Test Error
pred_test<-predict(myTreeprun, newdata = test,type="class")
mean(test$RESPONSE!=pred_test)

print(myTreeprun)
rpart.plot(myTreeprun)


## --------------------------------------------------------------------------------------------------------------------------------------------
# How reliable is the prunned model

cf<-table(actual = test$RESPONSE, pred = pred_test)
cf
accuracy72<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
precision72<-(cf[2,2]/(cf[2,2]+cf[1,2]))
recall72<-(cf[2,2]/(cf[2,2]+cf[2,1]))
accuracy72
precision72
recall72


## --------------------------------------------------------------------------------------------------------------------------------------------
#Gini for 0.7 and 0.3 (Default Tree without pruning)

data_new$RESPONSE<- as.factor(data_new$RESPONSE)
#The random seed is set to a fixed value below to make the results reproducible.
set.seed(1123)
#Splitting criteria
ind<-sample(2, nrow(data_new), replace =T, prob = c(0.7, 0.3))
train<-data_new[ind==1,]
test<-data_new[ind==2,]

#myFormula specifies that the target RESPONSE is dependent variable while all others (used as ~.) are independent variables. We have excluded the OBS# variable.
myFormula = RESPONSE ~. -`OBS#`
# Default decision tree without using pruning parameters
mytree <- rpart(myFormula, data = train)

#Check Train error
pred_train<-predict(mytree,data=train,type="class")
mean(train$RESPONSE!=pred_train)

#Check Test error
pred_test<-predict(mytree, newdata = test,type="class")
mean(test$RESPONSE!=pred_test)

#See Decision Tree
mytree
#Plot Decision Tree
rpart.plot(mytree)



## --------------------------------------------------------------------------------------------------------------------------------------------
#Parameters to check reliability of the model
cf<-table(actual = test$RESPONSE, pred = pred_test)
cf
accuracy<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
precision<-(cf[2,2]/(cf[2,2]+cf[1,2]))
recall<-(cf[2,2]/(cf[2,2]+cf[2,1]))
accuracy
precision
recall


## --------------------------------------------------------------------------------------------------------------------------------------------
plotcp(mytree)


## --------------------------------------------------------------------------------------------------------------------------------------------
#Gini for 0.7 and 0.3 (Default Tree without pruning)
#for loop
x<-c(1:10)
msplit <- 0
mbucket <- 0

for (val in x) {

myFormula = RESPONSE ~. -`OBS#`
myTree2 <- rpart(myFormula, data = train, control = rpart.control(minsplit = msplit, minbucket = mbucket, cp = 0.01))
cat("Min Split :",msplit, "Min Bucket:", mbucket,"\n") 
  
pred_train<-predict(myTree2,data=train,type="class")
print(mean(train$RESPONSE != pred_train))

pred_test<-predict(myTree2, newdata = test,type="class")
print(mean(test$RESPONSE != pred_test))
print(table(actual = test$RESPONSE, pred = pred_test))

msplit<-msplit + 10
mbucket<-mbucket + 8  
}


## --------------------------------------------------------------------------------------------------------------------------------------------
#Choosing the best prune tree with lowest test error

myTreeprun <- rpart(myFormula, data = train, control = rpart.control(minsplit = 10, minbucket = 8, cp = 0.01))

#Train error
pred_train<-predict(myTreeprun,data=train,type="class")
mean(train$RESPONSE != pred_train)

#Test error
pred_test<-predict(myTreeprun, newdata = test,type="class")
mean(test$RESPONSE != pred_test)

myTreeprun
rpart.plot(myTreeprun)



## --------------------------------------------------------------------------------------------------------------------------------------------
#Parameters to check reliability of the model
cf<-table(actual = test$RESPONSE, pred = pred_test)
cf
accuracy73<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
precision73<-(cf[2,2]/(cf[2,2]+cf[1,2]))
recall73<-(cf[2,2]/(cf[2,2]+cf[2,1]))
accuracy73
precision73
recall73


## --------------------------------------------------------------------------------------------------------------------------------------------
#C5.0 on prob = c(0.5,0.5)
set.seed(7276)
ind<-sample(2, nrow(data_new), replace = T, prob = c(0.5,0.5))
ctrain<-data_new[ind==1,]
ctest<-data_new[ind==2,]

train.fit <- C5.0(ctrain[c(-1,-22)], factor(ctrain$RESPONSE))
train.fit

cpred.train <- predict(train.fit, ctrain)
mean(ctrain$RESPONSE!=cpred.train)

cpred.test <- predict(train.fit, ctest)
print("Error Rate")
mean(ctest$RESPONSE!=cpred.test)

cf<-table(actual = ctest$RESPONSE, pred = cpred.test)
cf
c5accuracy55<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
c5precision55<-(cf[2,2]/(cf[2,2]+cf[1,2]))
c5recall55<-(cf[2,2]/(cf[2,2]+cf[2,1]))
print("Accuracy")
c5accuracy55
print("Precision")
c5precision55
print("Recall")
c5recall55



## --------------------------------------------------------------------------------------------------------------------------------------------
#c5.0 on prob = c(0.7,0.3)
set.seed(7576)
ind<-sample(2, nrow(data_new), replace = T, prob = c(0.7,0.3))
ctrain<-data_new[ind==1,]
ctest<-data_new[ind==2,]

train.fit <- C5.0(ctrain[c(-1,-22)], factor(ctrain$RESPONSE))
train.fit

cpred.train <- predict(train.fit, ctrain)
mean(ctrain$RESPONSE!=cpred.train)

cpred.test <- predict(train.fit, ctest)
print("Error Rate")
mean(ctest$RESPONSE!=cpred.test)

cf<-table(actual = ctest$RESPONSE, pred = cpred.test)
cf
c5accuracy73<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
c5precision73<-(cf[2,2]/(cf[2,2]+cf[1,2]))
c5recall73<-(cf[2,2]/(cf[2,2]+cf[2,1]))
print("Accuracy")
c5accuracy73
print("Precision")
c5precision73
print("Recall")
c5recall73


## --------------------------------------------------------------------------------------------------------------------------------------------
#c5.0 on prob = c(0.8,0.2)
set.seed(78876)
ind<-sample(2, nrow(data_new), replace = T, prob = c(0.8,0.2))
ctrain<-data_new[ind==1,]
ctest<-data_new[ind==2,]

train.fit <- C5.0(ctrain[c(-1,-22)], factor(ctrain$RESPONSE))
train.fit

cpred.train <- predict(train.fit, ctrain)
mean(ctrain$RESPONSE!=cpred.train)

cpred.test <- predict(train.fit, ctest)
print("Error Rate")
mean(ctest$RESPONSE!=cpred.test)

cf<-table(actual = ctest$RESPONSE, pred = cpred.test)
cf
c5accuracy82<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
c5precision82<-(cf[2,2]/(cf[2,2]+cf[1,2]))
c5recall82<-(cf[2,2]/(cf[2,2]+cf[2,1]))
print("Accuracy")
c5accuracy82
print("Precision")
c5precision82
print("Recall")
c5recall82



## --------------------------------------------------------------------------------------------------------------------------------------------
#c5.0 on prob = c(0.75,0.25)
set.seed(7134)
ind<-sample(2, nrow(data_new), replace = T, prob = c(0.75,0.25))
ctrain<-data_new[ind==1,]
ctest<-data_new[ind==2,]

train.fit <- C5.0(ctrain[c(-1,-22)], factor(ctrain$RESPONSE))
train.fit

cpred.train <- predict(train.fit, ctrain)
print("Error Rate on Training")
mean(ctrain$RESPONSE!=cpred.train)

cpred.test <- predict(train.fit, ctest)
print("Error Rate on test")
mean(ctest$RESPONSE!=cpred.test)
cf<-table(actual = ctest$RESPONSE, pred = cpred.test)
cf
c5accuracy72<-(cf[2,2]+cf[1,1])/(cf[1,1]+cf[1,2]+cf[2,1]+cf[2,2])
c5precision72<-(cf[2,2]/(cf[2,2]+cf[1,2]))
c5recall72<-(cf[2,2]/(cf[2,2]+cf[2,1]))
print("Accuracy")
c5accuracy72
print("Precision")
c5precision72
print("Recall")
c5recall72




## --------------------------------------------------------------------------------------------------------------------------------------------
cat("C&R Trees\n","50:50 Split\n","Accuracy","\t","Precision", "\t","Recall","\n",accuracy55,"\t",precision55,"\t",recall55,"\n")
print("70:30 Split")
cat("Accuracy","\t","Precision", "\t","Recall","\n",accuracy73,"\t",precision73,"\t",recall73,"\n")
print("80:20 Split")
cat("Accuracy","\t","Precision", "\t","Recall","\n",accuracy82,"\t",precision82,"\t",recall82,"\n")
print("75:25 Split")
cat("Accuracy","\t","Precision", "\t","Recall","\n",accuracy72,"\t",precision72,"\t",recall72,"\n\n")
cat("c5.0 Trees","\n","50:50 Split","\n","Accuracy","\t","Precision","\t","Recall","\n",c5accuracy55,"\t",c5precision55,"\t",c5recall55,"\n")
print("70:30 Split")
cat("Accuracy","\t","Precision", "\t","Recall","\n",c5accuracy73,"\t",c5precision73,"\t",c5recall73,"\n")
print("80:20 Split")
cat("Accuracy","\t","Precision", "\t","Recall","\n",c5accuracy82,"\t",c5precision82,"\t",c5recall82,"\n")
print("75:25 Split")
cat("Accuracy","\t","Precision", "\t","Recall","\n",c5accuracy72,"\t",c5precision72,"\t",c5recall72,"\n")


## --------------------------------------------------------------------------------------------------------------------------------------------
# Training a model with best minsplit and minbucket values obtained from above analysis
myTree <- rpart(myFormula, data = train, control = rpart.control(minsplit = 10, minbucket = 8, cp = 0.01))

# Performing prediction on the test data.
pred_test<-predict(myTree, newdata = test, type = "class")

# Bulding a confusion matrix for the actual response vs the predicted response.
pred_table <- table(actual = test$RESPONSE, pred = pred_test)
pred_table

# Plotting the tree which does not consider the misclassificaiton costs.
rpart.plot::rpart.plot(myTree)



## --------------------------------------------------------------------------------------------------------------------------------------------
# Calculating the accuracy, precision and recall of the model.
accuracy<-(pred_table[2,2]+pred_table[1,1])/(pred_table[1,1]+pred_table[1,2]+pred_table[2,1]+pred_table[2,2])
precision<-(pred_table[2,2]/(pred_table[2,2]+pred_table[1,2]))
recall<-(pred_table[2,2]/(pred_table[2,2]+pred_table[2,1]))

print("Without Loss function")
accuracy
precision
recall


## --------------------------------------------------------------------------------------------------------------------------------------------
loss<-matrix(c(0,5,1,0), byrow = T, ncol = 2)
# Training a model with the same parameters as myTree but adding loss function to it.
misclassTree <- rpart(myFormula, data = train,method = "class", control = rpart.control(minsplit = 10, minbucket = 8, cp = 0.01), parms = list(loss = loss))

misclas_pred_train<-predict(misclassTree, train, type = "class")
mean(misclas_pred_train!=train$RESPONSE)
# Performing prediction using the tree using the loss function
misclas_pred_test <- predict(misclassTree, test, type ="class" )
mean(misclas_pred_test!=test$RESPONSE)
# Building a confusion matrix for the actual response vs the predicted response.
misclas_table <- table(actual = test$RESPONSE, pred = misclas_pred_test)
misclas_table

# Plotting the tree which considers the misclassification costs.
rpart.plot::rpart.plot(misclassTree)


## --------------------------------------------------------------------------------------------------------------------------------------------
# Calculating the accuracy, precision and recall of the model
accuracy<-(misclas_table[2,2]+misclas_table[1,1])/(misclas_table[1,1]+misclas_table[1,2]+misclas_table[2,1]+misclas_table[2,2])
precision<-(misclas_table[2,2]/(misclas_table[2,2]+misclas_table[1,2]))
recall<-(misclas_table[2,2]/(misclas_table[2,2]+misclas_table[2,1]))

print("With loss function")
accuracy
precision
recall

