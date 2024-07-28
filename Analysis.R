# RT Project

#Libraries
library(survival)
?cox.zph
#dataset
primarydata <- read.csv("D:/College/NMIMS/PRACTICALS/RT/Churn/Churn_Edited.csv", header = T)
View(primarydata)
data <- read.csv("D:/College/NMIMS/PRACTICALS/RT/Churn/Churn_final.csv", header = T)
View(data)
str(data)
#####################################run all these codes before plotting
hist(data$Tenure)
data$Gender <- as.factor(data$Gender)
data$Geography <- as.factor(data$Geography)
data$Complain <- as.factor(data$Complain)
data$Satisfaction.Score <- as.factor(data$Satisfaction.Score)
data$NumOfProducts <- as.factor(data$NumOfProducts)
data$HasCrCard <- as.factor(data$HasCrCard)
data$IsActiveMember <- as.factor(data$IsActiveMember)
data$Card.Type <- as.factor(data$Card.Type)
data$Age_group <- as.factor(data$Age_group)
data$CrScore_group <- as.factor(data$CrScore_group)
data$Salary_group <- as.factor(data$Salary_group)
data$points_group <- as.factor(data$points_group)
data$balance_group <- as.factor(data$balance_group)


####

#Parametric Model fitting ####



library(MASS)
#histogram for churned data
datax <- data %>% 
  group_by(Exited) %>% 
  filter(Exited == 0) %>%
  select(Tenure)
datax <- data.frame(datax)
datax <- unlist(datax)
hist(datax)
hist(data$Balance)
hist(data$Tenure)







library(flexsurv)
#weibull fit
?flexsurvreg
fit_weibull <- flexsurvreg(Surv(Tenure, Exited) ~ 1, data = data, dist = "weibull"); fit_weibull
fit_exp <- flexsurvreg(Surv(Tenure, Exited) ~ 1, data = data, dist = "exp"); fit_exp
fit_llogis <- flexsurvreg(Surv(Tenure, Exited) ~ 1, data = data, dist = "llogis"); fit_llogis
fit_lnorm <- flexsurvreg(Surv(Tenure, Exited)~1, data = data, dist = "lnorm"); fit_lnorm
plot(fit_weibull, xlab = "Tenure", ylab = "Survival Probablity")
plot(fit_exp)
plot(fit_llogis)
plot(fit_lnorm)


#Hazard function
time_points <- seq(0, max(data$Tenure), 1)
shape <- 1.9181
scale <- 13.4745

pdf <- dweibull(time_points, shape = shape, scale = scale)

cdf <- pweibull(time_points, shape = shape, scale = scale)

hazard <- pdf/(1-cdf)
plot(time_points, hazard, xlab = "Tenure", ylab = "hazard", type = 'l')

#Non Parametric - Kaplan Meier Survival Functions ####

data1 <- data
fit_main <- survfit(Surv(tenure, status)~1, data = d1)
d1 <-data.frame(tenure,status)
p=ggsurvplot(fit_main,data = d1,conf.int = T);p1


str(data1)
tenure=data1$Tenure;tenure
status=data1$Exited;status
gender=data1$Gender;gender
d=data.frame(tenure,status,gender);d
summary(d)
library(survival)
library(survminer)
library(ggplot2)
#GENDER
fit=survfit(Surv(tenure,status) ~1);fit
summary(fit)
fit1=survfit(Surv(tenure,status)~gender,data=d)
p1=ggsurvplot(fit1,data = d,conf.int = T);p1

#IF WANT SEPRATELY
fit_male = survfit(Surv(tenure, status) ~ gender, data = subset(d, gender == "1"))
ggsurvplot(fit_male,data=d,palette = "blue")

fit_female= survfit(Surv(tenure,status)~ gender,data = subset(d,gender== "2"))
ggsurvplot(fit_female,data=d,palette  = "red")


#AGE GROUP
ag=data1$Age_group;ag
d_ag=data.frame(tenure,status,ag);d_ag
fit_ag=survfit(Surv(tenure,status)~ag,data=d_ag);fit_ag
y1=ggsurvplot(fit_ag,data = d_ag,conf.int = T)+labs(title = "Age Group");y1

#BALANCE_GROUP
bg=data1$balance_group;bg
d_bg=data.frame(tenure,status,bg);d_bg
fit_bg=survfit(Surv(tenure,status)~bg,data=d_bg);fit_bg
y2=ggsurvplot(fit_bg,data = d_bg)+labs(title = "BALANCE GROUP");y2

#CREDIT SCORE
cs=data1$CrScore_group;cs
d_cs=data.frame(tenure,status,cs);d_cs
fit_cs=survfit(Surv(tenure,status)~cs,data=d_cs);fit_cs
p2=ggsurvplot(fit_cs,data = d_cs,conf.int = F)+labs(title = "Credit Score");p2


#ESTIMATED SALARY
es=data1$Salary_group;es
d_es=data.frame(tenure,status,es);d_es
fit_es=survfit(Surv(tenure,status)~es,data=d_es);fit_es
n1=ggsurvplot(fit_es,data=d_es,conf.int = F)+labs(title = "Estimated Salary");n1


#NO OF PRODUCTS
np=data1$NumOfProducts;np
d4=data.frame(tenure,status,np);d4
fit11=survfit(Surv(tenure,status)~np,data=d4);fit11
y3=ggsurvplot(fit11,data = d4,conf.int = F)+labs(title = "No of Products");y3


#HAS CREDIT CARD?
cr=data1$HasCrCard;cr
d5=data.frame(tenure,status,cr)
fit12=survfit(Surv(tenure,status)~cr,data=d5);fit12
n2=ggsurvplot(fit12,data = d5,conf.int = T)+labs(title = "Has Credit Card");n2


#IS ACTIVE MEMBER
am=data1$IsActiveMember;am
d6=data.frame(tenure,status,am)
fit13=survfit(Surv(tenure,status)~am,data=d6);fit13
p3=ggsurvplot(fit13,data = d6,conf.int = T)+labs(title = "IS ACTIVE MEMBER");p3

#GEOGRAPHY
g=data1$Geography;g
d7=data.frame(tenure,status,g);d7
fit14=survfit(Surv(tenure,status)~g,data=d7);fit14
y4=ggsurvplot(fit14,data = d7,conf.int = T)+labs(title = "Geography");y4

#SATISFICATION LEVEL
ss=data1$Satisfaction.Score;ss
d8=data.frame(tenure,status,ss);d8
fit15=survfit(Surv(tenure,status)~ss,data=d8);fit15
n3=ggsurvplot(fit15,data = d8,conf.int = T)+labs(title = "Satisfiaction Score");n3


#CARD TYPE
ct=data1$`Card.Type`;ct
d9=data.frame(tenure,status,ss);d9
fit16=survfit(Surv(tenure,status)~ct,data=d9);fit16
n4=ggsurvplot(fit16,data = d9,conf.int = T)+labs(title = "Card Type");n4

#POINTS EARNED
pe=data1$points_group;pe
d_pe=data.frame(tenure,status,pe);d_pe
fit_pe=survfit(Surv(tenure,status)~pe,data=d_pe);fit_pe
n5=ggsurvplot(fit_pe,data = d_pe,conf.int = T)+labs(title = "Points Earned");n5


#COMPLAIN
com=data1$Complain
d_com=data.frame(tenure,status,com);d_com
fit_com=survfit(Surv(tenure,status)~com,data=d_com);fit_com
y5=ggsurvplot(fit_com,data = d_com,conf.int = T)+labs(title = "Complains");y5



#Non- Parametric Model
data1=Churn_final;data1
View(data1)
tenure=data1$Tenure;tenure
status=data1$Exited;status
gender=data1$Gender;gender
d=data.frame(tenure,status,gender);d
summary(d)
library(survival)
library(survminer)
#Any Covariate
covariate=data1$covariate; covariate
d_covariate=data.frame(tenure, status, covariate);d_covariate
fit_covariate=survfit (Surv(tenure,status)~covariate,data=d_covariate);fit_covariate
var=ggsurvplot(fit_covariate,data = d_covariate,conf.int = T)+labs(title = "covariate");var



#Semi Parametric - Cox-PH model ####


#Gender
cox_model_1 <- coxph(Surv(Tenure, Exited) ~ Gender + Age_group + Geography, data = data)
summary(cox_model_1)

#Age
cox_model_2 <- coxph(Surv(Tenure, Exited) ~ Age_group, data = data)
summary(cox_model_2)

#balance
cox_model_3 <- coxph(Surv(Tenure, Exited) ~ balance_group, data = data)
summary(cox_model_3)

#credit score
cox_model_4 <- coxph(Surv(Tenure, Exited) ~ CrScore_group, data = data)
summary(cox_model_4)

#Estimated Salary
cox_model_5 <- coxph(Surv(Tenure, Exited) ~ Salary_group, data = data)
summary(cox_model_5)

#No of Products
cox_model_6 <- coxph(Surv(Tenure, Exited) ~ NumOfProducts, data = data)
summary(cox_model_6)

#Has credit card
cox_model_7 <- coxph(Surv(Tenure, Exited) ~ HasCrCard, data = data)
summary(cox_model_7)

#Is active member
cox_model_8 <- coxph(Surv(Tenure, Exited) ~ IsActiveMember, data = data)
summary(cox_model_8)



contingency_table <- table(data$NumOfProducts, data$IsActiveMember)




#Geography
cox_model_9 <- coxph(Surv(Tenure, Exited) ~ Geography, data = data)
summary(cox_model_9)


#Saisfaction score
cox_model_10 <- coxph(Surv(Tenure, Exited) ~ Satisfaction.Score, data = data)
summary(cox_model_10)


#Complain
cox_model_11 <- coxph(Surv(Tenure, Exited) ~ Complain, data = data)
summary(cox_model_11)

#Points
cox_model_12 <- coxph(Surv(Tenure, Exited) ~ points_group, data = data)
summary(cox_model_12)

#Card Type
levels(data$Card.Type)
cox_model_13 <- coxph(Surv(Tenure, Exited) ~ Age_group + Gender + Card.Type , data = data)
summary(cox_model_13)

schoen_test <- cox.zph(cox_model_13)
plot(schoen_test)
print(schoen_test)


#final Cox models

#i 
coxmodel1 <- coxph(Surv(Tenure, Exited) ~ Gender + Age_group + Geography + balance_group + Salary_group, data = data)
summary(coxmodel1)
schoen_test <- cox.zph(coxmodel1)
plot(schoen_test)
print(schoen_test)

coxmodel2 <- coxph(Surv(Tenure, Exited) ~ CrScore_group + points_group + Card.Type + HasCrCard, data = data)
summary(coxmodel2)
schoen_test <- cox.zph(coxmodel2)
plot(schoen_test)
print(schoen_test)


coxmodel3 <- coxph(Surv(Tenure, Exited) ~ Complain + Satisfaction.Score + NumOfProducts + IsActiveMember, data = data)
summary(coxmodel3)




## Test for proportionality of hazards
schoen_test <- cox.zph(coxmodel3)
plot(schoen_test)
print(schoen_test)


weibull_model <- survreg(Surv(time, status) ~ Age + Gender, data = data, dist = "weibull")

# View the summary of the model
summary(weibull_model)


