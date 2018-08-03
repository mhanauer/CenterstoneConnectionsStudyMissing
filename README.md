---
title: "ConnectionsStudy"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load stats packages
```{r}
library(Amelia)
library(prettyR)
library(nlme)
library(descr)
library(foreign)
library(lme4)
library(sjstats)
```
Load up the GPRA data and get the measures that Jon is interested in, the intake, 6-month, and housing variables.  

Getting rid of people who were housed at the start the program

Need to change living where, with 4 being housed for both pre and post 
Need to change employment with 1 being employed and 2 not employed
```{r}
setwd("S:/Indiana Research & Evaluation/Indiana Connections/Data/GPRA")
GPRAAll = read.csv("ConnGPRA.csv", header = TRUE) 
# subet the data based on 1's for baseline and 2's for six-month.  Then write as CSV's, then merge together.
GPRAConBase = subset(GPRAAll, InterviewType ==1)
GPRAConMonth6 = subset(GPRAAll, InterviewType == 2)
GPRAAll = merge(GPRAConBase, GPRAConMonth6, by = "ClientID", all = TRUE)
write.csv(GPRAAll, "GPRAAll.csv", row.names = FALSE)
GPRAAll = read.csv("GPRAAll.csv", header = TRUE, na.strings = c(-9, -8, -7, -1, "", " ", "NULL", NULL, NA, "NA"))
GPRAAll$InterviewDate.x = as.Date(GPRAAll$InterviewDate.x, format = "%m/%d/%Y") 
GPRAAll$InterviewDate.y = as.Date(GPRAAll$InterviewDate.y, format = "%m/%d/%Y")
head(GPRAAll)

dim(GPRAAll)

# Getting rid of people who were originally housed
GPRAAll = subset(GPRAAll,LivingWhere.x !=4)
GPRAAll$LivingWhere.y = ifelse(GPRAAll$LivingWhere.y == 4, 1,0)
GPRAAll$LivingWhere.x = ifelse(GPRAAll$LivingWhere.x == 4, 1,0)
GPRAAll$Employment.x = ifelse(GPRAAll$Employment.x == 1, 1,0)
GPRAAll$Employment.y = ifelse(GPRAAll$Employment.y == 1, 1,0)
GPRAAll = subset(GPRAAll, Gender.x <= 2)
GPRAAll = subset(GPRAAll, Gender.y <= 2)
GPRAAll$Gender.x = ifelse(GPRAAll$Gender.x == 1, 1,0)
GPRAAll$Gender.y = ifelse(GPRAAll$Gender.y == 1, 1,0)
```
These are the variables in the GPRA that have the least missing data
Add interview date for post and then create a time in intervention
```{r}
ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x = GPRAAll$InterviewDate.x, InterviewDate.y = GPRAAll$InterviewDate.y, Depression.x = GPRAAll$Depression.x, 	Anxiety.x = GPRAAll$Anxiety.x,	BrainFunction.x = GPRAAll$BrainFunction.x,	ViolentBehavior.x = GPRAAll$ViolentBehavior.x,	PhysicallyHurt.x = GPRAAll$PhysicallyHurt.x,	InteractFamilyFriends.x = GPRAAll$InteractFamilyFriends.x, Depression.y = GPRAAll$Depression.y, Anxiety.y=	GPRAAll$Anxiety.y,	BrainFunction.y = GPRAAll$BrainFunction.y,	ViolentBehavior.y = GPRAAll$ViolentBehavior.y,	PhysicallyHurt.y = GPRAAll$PhysicallyHurt.y,	InteractFamilyFriends.y = GPRAAll$InteractFamilyFriends.y, Employment.x = GPRAAll$Employment.x, Employment.y = GPRAAll$Employment.y,ArrestedDays.x = GPRAAll$ArrestedDays.x, ArrestedDays.y = GPRAAll$ArrestedDays.y,LivingWhere.x = GPRAAll$LivingWhere.x, LivingWhere.y = GPRAAll$LivingWhere.y, HealthStatus.x = GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, IncomeWages.x = GPRAAll$IncomeWages.x, Age.x = GPRAAll$Age.x, EducationYears.x = GPRAAll$EducationYears.x, Gender.x = GPRAAll$Gender.x, DAUseIllegDrugsDays.x = GPRAAll$DAUseIllegDrugsDays.x, DAUseIllegDrugsDays.y = GPRAAll$DAUseIllegDrugsDays.y)
dim(ConnPaper)

# Getting a complete data set for comparision.  Then gettting rid of the those who are not eligiable
ConnPaperComplete = na.omit(ConnPaper)
ConnPaperComplete = subset(ConnPaperComplete, InterviewDate.x < "2018-02-01")
dim(ConnPaperComplete)

## Geting the people that are eligible for reassessments
ConnPaper = subset(ConnPaper, InterviewDate.x < "2018-02-01")
dim(ConnPaper)

1-(dim(ConnPaperComplete)[1]/(dim(ConnPaper)[1]))
#summary(ConnPaperComplete)
ConnGPRA =  ConnPaper
```
Load up PHQ-9 and GAD-7 Data
```{r}
PHQ9Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 Baseline.sav", to.data.frame = TRUE)
PHQ96month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 6 Month.sav", to.data.frame = TRUE)
PHQ9All = merge(PHQ9Base, PHQ96month, by = "ParticipantID", all = TRUE)
GAD7Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 Baseline.sav", to.data.frame = TRUE)
GAD76month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 6 Month.sav", to.data.frame = TRUE)
GAD7All = merge(GAD7Base, GAD76month, by = "ParticipantID", all = TRUE)
```

Load PHQ9 and GAD7 into one data set with client ID.  Then put them together with the GPRA data and see how much is missing.
```{r}
PHQ9Connections = data.frame(ClientID = PHQ9All$ParticipantID,PHQ9Base= PHQ9All$PHQ9Total.x, PHQ9Month6= PHQ9All$PHQ9Total.y)
GAD7Connections = data.frame(ClientID = GAD7All$ParticipantID, GAD7Base = GAD7All$GAD7Total.x, GAD7Month6 = GAD7All$GAD7Total.y)
PHQ9_GAD7 = merge(PHQ9Connections, GAD7Connections, by = "ClientID", all = TRUE)
head(PHQ9_GAD7)

```

Combine PHQ9 and GAD7, with interview date so we can exclude intake where no reassessment is due and housing
```{r}
PHQ9_GAD7 = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all = TRUE)
dim(PHQ9_GAD7)
PHQ9_GAD7 = data.frame(ClientID = PHQ9_GAD7$ClientID, InterviewDate.x = PHQ9_GAD7$InterviewDate.x,InterviewDate.y = PHQ9_GAD7$InterviewDate.y,LivingWhere.x= PHQ9_GAD7$LivingWhere.x, LivingWhere.y= PHQ9_GAD7$LivingWhere.y, InteractFamilyFriends.x = PHQ9_GAD7$InteractFamilyFriends.x, InteractFamilyFriends.y = PHQ9_GAD7$InteractFamilyFriends.y, Employment.x = PHQ9_GAD7$Employment.x, Employment.y = PHQ9_GAD7$Employment.y, PHQ9_GAD7[,23:33])
dim(PHQ9_GAD7)
PHQ9_GAD7 = subset(PHQ9_GAD7, InterviewDate.x < "2018-02-01")
dim(PHQ9_GAD7)


PHQ9_GAD7Complete = na.omit(PHQ9_GAD7)
dim(PHQ9_GAD7Complete)
1-(dim(PHQ9_GAD7Complete)[1]/(dim(PHQ9_GAD7)[1]))
```
Data Cleaning: Changing housing to yes or no for both data sets PHQ9_GAD7 and GPRA.  Only care about the 6 month reassessment, because we want to know if they housed eventually

Need to change living where for both 
```{r}
describe.factor(PHQ9_GAD7$LivingWhere.x)
describe.factor(PHQ9_GAD7$LivingWhere.y)
describe.factor(ConnGPRA$LivingWhere.y)
describe.factor(ConnGPRA$Employment.y)
```
Put everything into long format for GPRA and PHQ9 and GAD7
```{r}

ConnGPRALong = reshape(ConnGPRA, varying = list(c("Depression.x", "Depression.y"), c("Anxiety.x", "Anxiety.y"), c("BrainFunction.x", "BrainFunction.y"), c("ViolentBehavior.x", "ViolentBehavior.y"), c("PhysicallyHurt.x", "PhysicallyHurt.y"), c("Employment.x", "Employment.y"), c("ArrestedDays.x", "ArrestedDays.y"), c("HealthStatus.x", "HealthStatus.y"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y"), c("InterviewDate.x", "InterviewDate.y"), c("LivingWhere.x", "LivingWhere.y"), c("InteractFamilyFriends.x", "InteractFamilyFriends.y")), times = c(0,1), direction = "long")
head(ConnGPRALong)



PHQ9_GAD7Long = reshape(PHQ9_GAD7, varying = list(c("PHQ9Base", "PHQ9Month6"), c("GAD7Base", "GAD7Month6"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y"), c("InterviewDate.x", "InterviewDate.y"), c("LivingWhere.x", "LivingWhere.y"), c("InteractFamilyFriends.x", "InteractFamilyFriends.y"), c("Employment.x", "Employment.y")), times = c(0,1), direction = "long")

PHQ9_GAD7AnalysisLong = na.omit(PHQ9_GAD7Long)
write.csv(PHQ9_GAD7AnalysisLong, "PHQ9_GAD7AnalysisLong.csv", row.names = FALSE)
PHQ9_GAD7AnalysisLong = read.csv("PHQ9_GAD7AnalysisLong.csv", header = TRUE)


ConnGPRALongAnalysis = na.omit(ConnGPRALong)
dim(ConnGPRALongAnalysis)
write.csv(ConnGPRALong, "ConnGPRALong.csv", row.names = FALSE)
ConnGPRALong = read.csv("ConnGPRALong.csv", header = TRUE)

ConnGPRALongMissing = ConnGPRALong
ConnGPRALongMissing$InterviewDate.x = NULL

summary(ConnGPRALongMissing)
```
Ok try this with GAD7 data instead, because that has everything besides the depression variable
```{r}


```



Get rid of date variables those will mess up the imputer
Use ConnGPRALong, because that 
```{r}
head(ConnGPRALongMissing)
m = 10
ConnGPRALongMissingOut = amelia(m = 10, ConnGPRALongMissing, noms = c("Employment.x", "InteractFamilyFriends.x", "LivingWhere.x"), ords = c("HealthStatus.x", "Depression.x", "Anxiety.x", "ViolentBehavior.x", "PhysicallyHurt.x","ArrestedDays.x", "DAUseIllegDrugsDays.x", "IncomeWages.x"), ts = "time")

ConnGPRALongMissingOut$imputations$imp1$Depression.x

compare.density(ConnGPRALongMissingOut, var = "HealthStatus.x")
```
Getting missing data ready 
```{r}
ConnGPRALongMissing = lapply(1:m, function(x){ConnGPRALongMissingOut$imputations[[x]]})
head(ConnGPRALongMissing)

ConnGPRALongMissing = na.omit(ConnGPRALongMissing)
head(ConnGPRALongMissing[[1]])
dim(ConnGPRALongMissing[[1]])
```
Now getting descriptives
```{r}
ConnGPRALongMissingDescribe = lapply(1:m, function(x){describe(ConnGPRALongMissing[[x]])})
ConnGPRALongMissingDescribe
```
####Final
Data Analysis: Factors with housing: Employment + Depression.x
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```


Data Analysis: Factors associated with housing: Employment
```{r}
output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results

```
Data Analysis: Factors with housing: Employment + Age.x Age not significant
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Age.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```
Data Analysis: Factors with housing: Employment + EducationYears.x, not significant
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ EducationYears.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```
Data Analysis: Factors with housing: Employment + Gender.x, not significant
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Gender.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```
Data Analysis: Factors with housing: Employment + Depression.x, significant
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```
Data Analysis: Factors with housing: Employment + Depression.x, Anxiety.x, not significant
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + Anxiety.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```
Data Analysis: Factors with housing: Employment + Depression.x, ArrestedDays.x, not significant
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + ArrestedDays.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```
Data Analysis: Factors with housing: Employment + Depression.x, HealthStatus.x, not significant
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + HealthStatus.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```
Data Analysis: Factors with housing: Employment + Depression.x, DAUseIllegDrugsDays.x, not significant
```{r}
head(ConnGPRALongMissing[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + DAUseIllegDrugsDays.x + (1 | ClientID), data  = ConnGPRALongMissing[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 710)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
```

