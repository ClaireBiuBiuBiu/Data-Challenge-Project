setwd("D://UMD life//Spring Study//Data challenge//Maryland Small Business Development Center")
mydata=read.csv("3.1_Q1.csv")
library(ggplot2)
library(tidyverse)
library(pastecs)
library(lubridate)
library(randomForest)
library(ROCR)
library(mlr)
library(ggplot2)
library(dplyr)
library(car)
options(scipen=999)
table(mydata$Own_Gender)

 my_Q3 %>% group_by(Own_Hisp) %>% summarise(sum(count_jobs,na.rm=T))
table(my_Q3$Own_Hisp)
mydata$Own_Gender
mydata = mydata %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(
    NAICS_code= relevel(NAICS_code, ref = 'Undefined'),
    Own_Gender = relevel(Own_Gender, ref = 'Choose not to respond'),
    Service_Center=relevel(Service_Center, ref = "Prince George's County"),
    Own_Hisp = relevel(Own_Hisp, ref = '0'),
    Own_Race = relevel(Own_Race, ref = 'White/Caucasian' )
  )
mydata$Own_Race
mydata %>% group_by(Own_Hisp) %>% summarise(sum(Ipct_StartedBusi))
mydata = mydata %>% mutate(log_Total_ConTime=log(Total_ConTime))
write.csv(mydata,"3.1_Q1.csv")
#model 1 
library(broom)
logistic= glm(Ipct_StartedBusi~ log_Total_ConTime + Attended_GRPT + NAICS_code+
                Own_Gender + Service_Center + Initial_Services+
                #County+
                Own_Hisp + Own_Race + initial_employee+
                initial_revenue, data = mydata, family = binomial(link="logit"))
summary(logistic)
tidy_logistic_Q1=tidy(logistic)
pred.Q1=predict.lm(logistic,mydata,type="response")
write.csv(tidy_logistic_Q1,"logistic_Q1.csv")

y.pred.Q1=pred.Q1
y.pred.Q1

y.test.Q1=mydata$Ipct_StartedBusi
y.test.Q1

table(y.test.Q1,y.pred.Q1>0.3)

y.pred.Q1=ifelse(y.pred.Q1>0.3,1,0)

library(caret)
confusionMatrix(as.factor(y.test.Q1),as.factor(y.pred.Q1))
###########confusion matrix

#Q2 whether capital invest

my_Q2= read.csv("second_clean.csv")
my_Q2= my_Q2 %>%  mutate_if(is.character,as.factor) %>% 
  mutate(
    NAICS_code= relevel(NAICS_code, ref = 'Undefined'),
    Own_Gender = relevel(Own_Gender, ref = 'Choose not to respond'),
    Service_Center=relevel(Service_Center, ref = "Prince George's County"),
    Own_Hisp = relevel(Own_Hisp, ref = '0'),
    Own_Race = relevel(Own_Race, ref = 'White/Caucasian' )
  )

logistic_Q2 = glm(Is.CptlIvest ~  Attended_GRPT +log_Total_ConTime
                  #+NAICS_code 
                  +Initial_Services+Own_Gender+ Service_Center + NAICS_code
                  +Own_Hisp+Own_Race+initial_employee
                  +initial_revenue, data = my_Q2, family = binomial(link="logit"))
summary(logistic_Q2)

pred.logit_Q2=predict.glm(logistic_Q2,my_Q2,type="response")
pred.logit_Q2

tidy_logistic_Q2=tidy(logistic_Q2)
write.csv(tidy_logistic_Q2,"logistic_Q2.csv")

#Q2-2 
my_Q2$
subset_Q2= my_Q2 %>% filter(Ipct_CapitalIvest>0)
log((my_Q2$Ipct_CapitalIvest)+1)

test_exp=as.data.frame(exppred_Q2)
test_exp
dist_ipct_invet=ggplot(subset_Q2, aes(x=log(Ipct_CapitalIvest))) + geom_density() 
dist_ipct_invet

linear_Q2 = lm(log(Ipct_CapitalIvest+1)~Attended_GRPT +log_Total_ConTime
               #+NAICS_code 
               +Initial_Services+Own_Gender+ Service_Center + NAICS_code
               +Own_Hisp+Own_Race+initial_employee
               +initial_revenue, data = my_Q2)
summary(linear_Q2)
pred_linear_Q2=predict.lm(linear_Q2,my_Q2)
my_Q2= my_Q2 %>% mutate(exppred_Q2= test_exp)

my_Q2$exppred_Q2
exp(0.344)
exp(0.938)
#testing model
library(heuristica)
library(Matrix)
library(tidyverse)
library(caret)
#try confusion matrix for Q2_logit

y.pred=pred.logit_Q2
y.pred

y.test=my_Q2$Is.CptlIvest
y.test

table(y.test,y.pred>0.3)

y.pred=ifelse(y.pred>0.3,1,0)

confusionMatrix(as.factor(y.test),as.factor(y.pred))



#Q3
my_Q3_subset$Ipct_StartedBusi
#my_Q3 = my_Q3 %>%  mutate_at(vars(Ipct_StartedBusi), funs(replace(., is.na(.), "exist")))
my_Q3= read.csv("data_Q3.csv")
table(my_Q3$Own_Gender)
my_Q3_subset=my_Q3 %>% filter(Ipct_StartedBusi=="exist")
write.csv(my_Q3_subset,"data_Q3.csv")


#employee new jobs
my_Q3= my_Q3 %>%  mutate_if(is.character,as.factor) %>% 
  mutate(
    NAICS_code= relevel(NAICS_code, ref = 'Undefined'),
    Own_Gender = relevel(Own_Gender, ref = 'Choose not to respond'),
    Service_Center=relevel(Service_Center, ref = "Prince George's County"),
    Own_Hisp = relevel(Own_Hisp, ref = '0'),
    Own_Race = relevel(Own_Race, ref = 'White/Caucasian' )
  )
class(my_Q3$Ipct_Create_Newjobs)
my_Q3$Ipct_Create_Newjobs
my_Q3$Ipct_RevenueIncrease

dist_ipct_reve=ggplot(my_Q3, aes(x=log(Ipct_RevenueIncrease))) + geom_density() 
dist_ipct_reve

my_Q3$count_jobs = ifelse(my_Q3$Ipct_Create_Newjobs>0,1,0)
sum(my_Q3$count_jobs==1)
employee_gamma = my_Q3 %>% filter(count_jobs>0)
my_Q3= my_Q3 %>% mutate(log_newjobs=log(Ipct_Create_Newjobs+1))

logit_Q3_job = glm(count_jobs ~ Attended_GRPT + log_Total_ConTime
                   #+NAICS_code 
                   #+Initial_Services
                   + Own_Gender + Service_Center + NAICS_code
                   +Own_Hisp + Own_Race + initial_employee
                   +initial_revenue, data =my_Q3,family=binomial(link="logit"))
summary(logit_Q3_job)

pred.logit_Q3job=predict.glm(logit_Q3_job,my_Q3,type="response")

y.pred.Q3job=pred.logit_Q3job

y.pred.Q3job

y.test.Q3job=my_Q3$count_jobs
y.test.Q3job

table(y.test.Q3job,y.pred.Q3job>0.3)

y.pred.Q3job=ifelse(y.pred.Q3job>0.3,1,0)

confusionMatrix(as.factor(y.test.Q3job),as.factor(y.pred.Q3job))





logit_Q3_job =tidy(logit_Q3_job )
write.csv(logit_Q3_job ,"logit_Q3_job.csv")

gamma_Q3 = glm(log_newjobs ~ Attended_GRPT + log_Total_ConTime
               #+NAICS_code 
               #+Initial_Services
               + Own_Gender + Service_Center + NAICS_code
               +Own_Hisp + Own_Race + initial_employee
               +initial_revenue, data = employee_gamma, family = Gamma (link="identity"))
summary(gamma_Q3)
tidy_gamma_Q3=tidy(gamma_Q3)
write.csv(tidy_gamma_Q3,"jobinc_gamma_Q3.csv")
exp(0.01209297140)
exp(0.00000016740)














my_Q3 = my_Q3 %>% mutate(is.increvenue=ifelse(Ipct_RevenueIncrease>0,1,0))
sum(my_Q3$is.increvenue==1)
sub_revenue_Q32= my_Q3 %>% filter(is.increvenue==1)




logit_Q3_revenue = glm(is.increvenue ~ Attended_GRPT + log_Total_ConTime
                   #+NAICS_code 
                   #+Initial_Services
                   + Own_Gender + Service_Center + NAICS_code
                   +Own_Hisp + Own_Race + initial_employee
                   +initial_revenue, data =my_Q3,family=binomial(link="logit"))
summary(logit_Q3_revenue)
logit_Q3_revenue =tidy(logit_Q3_revenue )
write.csv(logit_Q3_revenue,"logit_Q3_revenue.csv")

#revenue confusion matrix

pred.logit_Q3rev= predict.glm(logit_Q3_revenue,my_Q3,type="response")

pred.logit_Q3rev

y.pred.Q3rev=pred.logit_Q3rev
y.pred.Q3rev

y.test.Q3rev=my_Q3$is.increvenue
y.test.Q3rev

table(y.test.Q3rev,y.pred.Q3rev>0.25)

y.pred.Q3rev=ifelse(y.pred.Q3rev>0.25,1,0)

confusionMatrix(as.factor(y.test.Q3rev),as.factor(y.pred.Q3rev))

revenue_Q32 = lm(log(Ipct_RevenueIncrease+1) ~ Attended_GRPT +log_Total_ConTime
                #+NAICS_code 
                +Initial_Services+Own_Gender+ Service_Center + NAICS_code
                +Own_Hisp+Own_Race+initial_employee
                +initial_revenue, data = my_Q3)
summary(revenue_Q32)
tidy_linear_Q3=tidy(revenue_Q32)
write.csv(tidy_linear_Q3,"linear_revenue_Q3.csv")

exp()
#confusion matrix for Q3
y.pred=pred.logit_Q2
y.pred

y.test=my_Q2$Is.CptlIvest
y.test

table(y.test,y.pred>0.3)

y.pred=ifelse(y.pred>0.3,1,0)

confusionMatrix(as.factor(y.test),as.factor(y.pred))