setwd("S://R Studio Study//Projects//Banking Project")

Train=read.csv("bank-full_train.csv",stringsAsFactors = F)
Test=read.csv("bank-full_test.csv",stringsAsFactors = F)

#########################################################################################

library(dplyr)

Test1=Test
Train1=Train

Test1$y=NA
Test1$Data='Test'

Train1$Data='Train'

Bank=rbind(Train1,Test1)

glimpse(Bank)

###############################################################################

#Age:

unique(Bank$age)

sum(is.na(Bank$age))

boxplot(Bank$age,horizontal = T) # outlier is found but not removed as tree methods doe not get affected

 # Mean age is 40.9 ~ 41 yrs

#Job

unique(Bank$job)

round(prop.table(table(Bank$job,Bank$y),1),2)

Bank=Bank%>%
  mutate(Job_Admin_SE_UN=as.numeric(job%in%c("admin.","self-employed","unknown")),
         
          Job_Enterpreneur_Services=as.numeric(job%in%c("entrepreneur","services")),  
         
         Job_Mgmt=as.numeric(job=='management'),
         
         Job_Technician=as.numeric(job=='technician'),
         
         Job_BlueCollar=as.numeric(job=='blue-collar'),
         
         Job_housemaid=as.numeric(job=='housemaid'),
         
         Job_retired=as.numeric(job=='retired'))

Bank=Bank%>%select(-job)

#Marital

unique(Bank$marital)

round(prop.table(table(Bank$marital,Bank$y),1),2)

Bank=Bank%>%
  mutate(Marital_div_single=as.numeric(marital%in%c('divorced','single')),
         Marital_married=as.numeric(marital=='married'))

Bank=Bank%>%select(-marital)

glimpse(Bank)

#Education:

unique(Bank$education)

round(prop.table(table(Bank$education,Bank$y),1),2)

Bank=Bank%>%
  mutate(Education_Prim=as.numeric(education=='primary'),
         Education_secondary_Tertiary_Unknown=as.numeric(education%in%c('secondary','tertiary','unknown')))
        

Bank=Bank%>%select(-education)

#Default

unique(Bank$default)

round(prop.table(table(Bank$default,Bank$y),1),2)

Bank=Bank%>%
  mutate(Default.Credit_Yes=as.numeric(default=='yes'))

Bank=Bank%>%select(-default)

glimpse(Bank)

#Balance:

options(max.print=100000)
unique(Bank$balance)

sum(is.na(Bank$balance))

#Housing:

unique(Bank$housing)

round(prop.table(table(Bank$housing,Bank$y),1),2)

Bank=Bank%>%
  mutate(Housing.Loan_Yes=as.numeric(housing=='yes'))

Bank=Bank%>%select(-housing)

#Loan

unique(Bank$loan)

round(prop.table(table(Bank$loan,Bank$y),1),2)

Bank=Bank%>%
  mutate(Persona.Loan_Yes=as.numeric(loan=='yes'))

Bank=Bank%>%select(-loan)

glimpse(Bank)

#contact:

unique(Bank$contact)

round(prop.table(table(Bank$contact,Bank$y),1),2)

Bank=Bank%>%
  mutate(Contact.Cellular.telephone=as.numeric(contact%in%c('cellular','telephone')),
         Contact.Unknown=as.numeric(contact=='unknown'))

Bank=Bank%>%select(-contact)

# Dropping ID

Bank=Bank%>%select(-ID)

#Dropping Day: ## DO not Drop

unique(Bank$day)

round(prop.table(table(Bank$day,Bank$y),1),1)


Bank=Bank%>%
  mutate(Day_0.9_0.1=as.numeric(day%in%c("2","5","6","7","8","9","11","14","15","16","17","18",
                                         "19","20","21","23","24","26","27","28","29","31")),
         
         Day_0.8_0.2=as.numeric(day%in%c("3","4","10","12","13","22","25","30")),
         
         Day_0.7._0.3=as.numeric(day=="1"))

Bank=Bank%>%select(-day)

#Month:

unique(Bank$month)

round(prop.table(table(Bank$month,Bank$y),1),2)

Bank=Bank%>%
  mutate(Month_Apr.Feb=as.numeric(month%in%c("apr","feb")), # 0.80, 0.83 together
         Month_Aug.Jan.nov=as.numeric(month%in%c("aug","jan","nov")), ## 0.89 0.11
         Month_dec.sep=as.numeric(month%in%c("dec","sep")), # 0.58 0.42
         Month_Jun_Jul=as.numeric(month%in%c("jul","jun","may")), # 0.91, 0.90, 0.93 together
         Month_Mar=as.numeric(month=="mar"), # 0.47
         Month_Mar=as.numeric(month=="oct")) #0.55


Bank=Bank%>%select(-month)

#Duration

unique(Bank$duration)

boxplot(Bank$duration,horizontal = T) # outlier is found but not removed

#Campaign:

unique(Bank$campaign)

boxplot(Bank$campaign,horizontal = T) # outlier is found but not removed

unique(Bank$campaign)

#Pdays:

unique(Bank$pdays)

boxplot(Bank$pdays,horizontal = T) # outlier is found but not removed

# Previous

unique(Bank$previous)

boxplot(Bank$previous,horizontal = T) # outlier is found but not removed

# Poutcome # Needs to be dropped
Bank=Bank%>%select(-poutcome)

# y

unique(Bank$y)
sum(is.na(Bank$y))

Bank=Bank%>%
  mutate(y=as.numeric(y=="yes"))

glimpse(Bank)

#Separating Test and Train Data:

Test2=Bank%>%filter(Data=='Test')
Test2=Test2%>%select(-Data)
Test2=Test2%>%select(-y)


Train2=Bank%>%filter(Data=='Train')
Train2=Train2%>%select(-Data) 

#Checking Nas on both Train2 and Test2:

sum(is.na(Train2))
sum(is.na(Test2))

# Breaking Train2 into Train3 and Test3:

set.seed(2)
s=sample(1:nrow(Train2),0.8*nrow(Train2))

Train3=Train2[s,]
Test3=Train2[-s,]

############################################################################################

library(car)

for_vif=lm(y~.,data = Train3) 

vif(for_vif)

alias(for_vif) 

for_vif=lm(y~.-Marital_married-Education_secondary_Tertiary_Unknown-Contact.Unknown
           -Day_0.7._0.3 ,data = Train3)

sort(vif(for_vif),decreasing = T)

for_vif=lm(y~.-Marital_married-Education_secondary_Tertiary_Unknown-Contact.Unknown-Day_0.9_0.1
           -Day_0.7._0.3-Month_Jun_Jul ,data = Train3)

sort(vif(for_vif),decreasing = T)

##############################################################################

#Trying random forest:

library(randomForest)
library(cvTools)

# Converting to factors

glimpse(Train2)

Train2=Train2%>%
  mutate(y=as.factor(y))

glimpse(Train3)
Train3=Train3%>%
  mutate(y=as.factor(y))

glimpse(Test3)
Test3=Test3%>%
  mutate(y=as.factor(y))

# Parameters Tuning:

param=list(mtry=c(10,15,6),
           ntree=c(1000,500,700,200),
           maxnodes=c(70,20,30,50),
           nodesize=c(5,10,15))

Expanded.Param=expand.grid(param)

s=sample(1:nrow(Expanded.Param),29)

subset2=Expanded.Param[s,]

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  
  return(score)}

# Parameter Tuning for Random Forest:

myauc=0

for(i in 1:29){
  
  print(paste0('iteration',i))
  
  parameters=subset2[i,]
  
  k=cvTuning(randomForest,y~.-Marital_married-Education_secondary_Tertiary_Unknown-Contact.Unknown-Day_0.9_0.1
             -Day_0.7._0.3-Month_Jun_Jul ,data = Train3,
             
             tuning = parameters,
             
             folds = cvFolds(nrow(Train3),K=10,type = 'random'),
             
             cost = mycost_auc, seed = 2,
             
             predictArgs = list(type='prob'))
  
  score.this=k$cv[,2]
  if(score.this>myauc)
    
  {
    myauc=score.this
    
    best_params=parameters
    print(best_params)
  }}

# Best parmas auc=0.8699, mtry=6, ntree=1000, maxnodes=50, nodesize=10

rfmodel=randomForest(y~.-Marital_married-Education_secondary_Tertiary_Unknown-Contact.Unknown-Day_0.9_0.1
                     -Day_0.7._0.3-Month_Jun_Jul ,data = Train3,ntree=1000,mtry=6,maxnodes=50,nodesize=10)

pre.Insample=predict(rfmodel,newdata = Train3,type = 'prob')[,2]

pROC::roc(Train3$y,pre.Insample) # In sample auc=87.78

pre.Outsample=predict(rfmodel,newdata = Test3,type = 'prob')[,2]

pROC::roc(Test3$y,pre.Outsample) # Out Sample= 0.8652

# Building model on Train2:

RFModel.Train2=randomForest(y~.-Marital_married-Education_secondary_Tertiary_Unknown-Contact.Unknown-Day_0.9_0.1
                            -Day_0.7._0.3-Month_Jun_Jul ,data = Train2,ntree=1000,mtry=6,maxnodes=50,nodesize=10)

Pred.Train2=predict(RFModel.Train2,newdata = Train2,type = 'prob')[,2]

pROC::roc(Train2$y,Pred.Train2) # AUC on Train2 is 87.81

#Creating Cutoffs:

Cutoffs=round(seq(0,1,length=100),3)

Cutoff_Data=data.frame(Cutoff=0,TP=0,TN=0,FP=0,FN=0)

for(i in Cutoffs){
  
  Predicted=as.numeric(Pred.Train2>i)
  
  TP=sum(Train2$y==1&Predicted==1)
  TN=sum(Train2$y==0&Predicted==0)
  FP=sum(Train2$y==0&Predicted==1)
  FN=sum(Train2$y==1&Predicted==0)
  
  Cutoff_Data=rbind(Cutoff_Data,c(i,TP,TN,FP,FN))
}

Cutoff_Data=Cutoff_Data[-1,]

Cutoff_Data=Cutoff_Data%>%
  mutate(P=TP+FN,
         N=TN+FP,
         KS=abs((TP/P)-(FP/N)))

W=which.max(Cutoff_Data$KS) #Maximum KS is 0.646 at a cut off of 0.010, needed KS is 0.47

C=Cutoff_Data$KS[W]

Pred.Test2.RF=predict(RFModel.Train2,newdata = Test2,type = 'prob')[,2]

length(Pred.Test2.RF)

sum(is.na(Pred.Test2.RF))

Final.predictions_RF=ifelse(Pred.Test2.RF>0.01,"yes","no")

write.csv(Final.predictions_RF,"Mohana_Bhowmick_P5_part2.csv",row.names = F) 

# Submission Attempt 3 done on Random Forest




