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
library(MissMech)
library(BaylorEdPsych)
library(ggplot2)
library(HLMdiag)
library(psych)
library(stargazer)
library(MuMIn)
```
Loading the data and reading it in
I am then subsetting the data for only those at baseline (InterviewType == 1) and 6 months (InterviewType == 2)
Then I am merging those datasets together back into one data set
Then I am writing and reading them back into R so I can set the missing values indicators
Then I am transforming the both the interview dates into date formats that R can read
Then I am getting rid of people who were housed at the start (LivingWhere.x == 4)
Because when LivingWhere is 4 that is the only value indicating that people are housed.  Then I am changing both variables to 1 and 0 where 1 is when LivingWhere equals 4 and 0 otherwise
Because when employment equals 1 that means that they are employed at all, I am making that one and all else zero
I am subetting the gender variable where gender is greater than 2 is excluded, because there are not enough non male or female gender identities to run any statistics on
Then for gender I am changing them to 1 for male and 0 for female
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

GPRAAll = subset(GPRAAll,LivingWhere.x !=4)
GPRAAll$LivingWhere.y = ifelse(GPRAAll$LivingWhere.y == 4, 1,0)
GPRAAll$LivingWhere.x = ifelse(GPRAAll$LivingWhere.x == 4, 1,0)
GPRAAll$Employment.x = ifelse(GPRAAll$Employment.x == 1, 1,0)
GPRAAll$Employment.y = ifelse(GPRAAll$Employment.y == 1, 1,0)
GPRAAll = subset(GPRAAll, Gender.x <= 2)
GPRAAll = subset(GPRAAll, Gender.y <= 2)
GPRAAll$Gender.x = ifelse(GPRAAll$Gender.x == 1, 1,0)
GPRAAll$Gender.y = ifelse(GPRAAll$Gender.y == 1, 1,0)
GPRAAll$EducationYears.x = ifelse(GPRAAll$EducationYears.x <= 12, 1, 0)
GPRAAll$EducationYears.y = ifelse(GPRAAll$EducationYears.y <= 12, 1, 0)
```
Here I am trying to get the variables that have the most data and the most relevance to research by subsetting the GPRA for only those questions.

Then I am subsetting the data for only those at the time of analysis that are eligible for reassessment.  Because the reassessment takes place every 6 months and the date for this analysis was 8-1-2018 anyone who entered the program later than 2-1-2018 would not be eligible for reassessments so they are not included.  InterviewDate.x equals the intake or baseline date.
```{r}
ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x = GPRAAll$InterviewDate.x, InterviewDate.y = GPRAAll$InterviewDate.y, Depression.x = GPRAAll$Depression.x, Depression.y = GPRAAll$Depression.y, 	Anxiety.x = GPRAAll$Anxiety.x, Anxiety.y = GPRAAll$Anxiety.y, Employment.x = GPRAAll$Employment.x, Employment.y = GPRAAll$Employment.y,ArrestedDays.x = GPRAAll$ArrestedDays.x, ArrestedDays.y = GPRAAll$ArrestedDays.y,LivingWhere.x = GPRAAll$LivingWhere.x, LivingWhere.y = GPRAAll$LivingWhere.y, HealthStatus.x = GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, IncomeWages.x = GPRAAll$IncomeWages.x, IncomeWages.y = GPRAAll$IncomeWages.y, Age.x = GPRAAll$Age.x, Age.y = GPRAAll$Age.y, EducationYears.x = GPRAAll$EducationYears.x, EducationYears.y = GPRAAll$EducationYears.y, Gender.x = GPRAAll$Gender.x, Gender.y = GPRAAll$Gender.y, DAUseIllegDrugsDays.x = GPRAAll$DAUseIllegDrugsDays.x, DAUseIllegDrugsDays.y = GPRAAll$DAUseIllegDrugsDays.y)

ConnPaper = subset(ConnPaper, InterviewDate.x < "2018-02-01")
```
Here I am loading up both the PHQ9 and GAD7 baselines and 6-month reassessments together.  I am using the merge function, which allows me to combine different data sets using a unique identifier common to both data sets.
```{r}
PHQ9Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 Baseline.sav", to.data.frame = TRUE)
PHQ96month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 6 Month.sav", to.data.frame = TRUE)
PHQ9All = merge(PHQ9Base, PHQ96month, by = "ParticipantID", all = TRUE)
GAD7Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 Baseline.sav", to.data.frame = TRUE)
GAD76month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 6 Month.sav", to.data.frame = TRUE)
GAD7All = merge(GAD7Base, GAD76month, by = "ParticipantID", all = TRUE)
```
Now from PHQ9 and GAD7 data sets, I am grabbing only the total scores from both assessments along with the id's and changing the name to ClientID so we can merge it with the GPRA data set.
```{r}
PHQ9Connections = data.frame(ClientID = PHQ9All$ParticipantID,PHQ9Base= PHQ9All$PHQ9Total.x, PHQ9Month6= PHQ9All$PHQ9Total.y)
GAD7Connections = data.frame(ClientID = GAD7All$ParticipantID, GAD7Base = GAD7All$GAD7Total.x, GAD7Month6 = GAD7All$GAD7Total.y)
PHQ9_GAD7 = merge(PHQ9Connections, GAD7Connections, by = "ClientID", all = TRUE)
head(PHQ9_GAD7)
```
Now I am merging the PHQ9_GAD7 data set with the GPRA data set using the ClientID.  all = TRUE means that all ClientID's will be included from both datasets.
```{r}
PHQ9_GAD7 = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all = TRUE)
PHQ9_GAD7 = subset(PHQ9_GAD7, InterviewDate.x < "2018-02-01")
dim(PHQ9_GAD7)
head(PHQ9_GAD7)
sum(is.na(PHQ9_GAD7$LivingWhere.x))

```
### For descriptives for paper 
First get without missing data 
Need to change some variables in factors
```{r}
PHQ9_GAD7Cat = data.frame(PHQ9_GAD7$Employment.x, PHQ9_GAD7$LivingWhere.x, PHQ9_GAD7$HealthStatus.x, PHQ9_GAD7$EducationYears.x, PHQ9_GAD7$Gender.x)
PHQ9_GAD7Cat = apply(PHQ9_GAD7Cat, 2, function(x){describe.factor(x)})
stargazer(PHQ9_GAD7, type = "text")
```
## Missing value descitptives
Now we need to get descriptives for the missing values.  We want to have the best missing data model, so we will first turn it into long format, impute, then subset by time 0, then get means and sd then combine those


Now I am using the reshape function to change everything from wide format (i.e. Depression.x, Depression.y) into long format (Depression over time points 0 (baseline) and 1 (6-month)).  Changing to wide format is kind of like stacking the variables where 6-month goes below the baseline and we use a time variable to indicate whether a row is baseline (time = 0) or 6-month (time = 1).
```{r}
PHQ9_GAD7Long = reshape(PHQ9_GAD7, varying = list(c("PHQ9Base", "PHQ9Month6"), c("GAD7Base", "GAD7Month6"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y"), c("InterviewDate.x", "InterviewDate.y"), c("LivingWhere.x", "LivingWhere.y"), c("Employment.x", "Employment.y"), c("Depression.x", "Depression.y"), c("HealthStatus.x", "HealthStatus.y"), c("Gender.x", "Gender.y"), c("EducationYears.x", "EducationYears.y"), c("IncomeWages.x", "IncomeWages.y"), c("ArrestedDays.x", "ArrestedDays.y"), c("Anxiety.x", "Anxiety.y"), c("Age.x", "Age.y")), times = c(0,1), direction = "long")
```
### Missing value descriptives
```{r}
write.csv(PHQ9_GAD7Long, "PHQ9_GAD7Long.csv", row.names = FALSE)
PHQ9_GAD7Long = read.csv("PHQ9_GAD7Long.csv", header = TRUE)
PHQ9_GAD7LongMissingDes = PHQ9_GAD7Long
PHQ9_GAD7LongMissingDes$InterviewDate.x = NULL
PHQ9_GAD7LongMissingDes$id = NULL
m = 10
PHQ9_GAD7LongMissingDesOut = amelia(m = 10, PHQ9_GAD7LongMissingDes, noms = c("Employment.x", "LivingWhere.x", "Gender.x", "EducationYears.x"), ords = c("Depression.x", "Anxiety.x","ArrestedDays.x", "DAUseIllegDrugsDays.x"), ts = "time")


datAnalysisAll = lapply(1:m, function(x){PHQ9_GAD7LongMissingDesOut$imputations[[x]]})


datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], time == 0)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
Center everything besides categorical, date, and count (centering count variables could result in negative and not whole numbers).  So create two data sets one for the categorical outcomes and one for the continuous ones that will be centered.  Then recombine them into one data set overwriting the old one.
```{r}
head(PHQ9_GAD7Long)

PHQ9_GAD7LongBinary = data.frame(ClientID = PHQ9_GAD7Long$ClientID, time = PHQ9_GAD7Long$time, Gender.x = PHQ9_GAD7Long$Gender.x, LivingWhere.x = PHQ9_GAD7Long$LivingWhere.x, Employment.x = PHQ9_GAD7Long$Employment.x, EducationYears.x = PHQ9_GAD7Long$EducationYears.x, DAUseIllegDrugsDays.x = PHQ9_GAD7Long$DAUseIllegDrugsDays.x, Depression.x = PHQ9_GAD7Long$Depression.x, InterviewDate.x = PHQ9_GAD7Long$InterviewDate.x, ArrestedDays.x = PHQ9_GAD7Long$ArrestedDays.x, Anxiety.x = PHQ9_GAD7Long$Anxiety.x)

PHQ9_GAD7LongCon = data.frame(HealthStatus.x = PHQ9_GAD7Long$HealthStatus.x, IncomeWages.x = PHQ9_GAD7Long$IncomeWages.x, Age.x = PHQ9_GAD7Long$Age.x, PHQ9Base = PHQ9_GAD7Long$PHQ9Base, GAD7Base = PHQ9_GAD7Long$GAD7Base)
summary(PHQ9_GAD7LongCon)

write.csv(PHQ9_GAD7LongCon, "PHQ9_GAD7LongCon.csv", row.names = FALSE)
PHQ9_GAD7LongCon = read.csv("PHQ9_GAD7LongCon.csv", header = TRUE)

PHQ9_GAD7LongCon = scale(PHQ9_GAD7LongCon, center = TRUE, scale = FALSE)

PHQ9_GAD7LongCenter = data.frame(PHQ9_GAD7LongBinary, PHQ9_GAD7LongCon)
write.csv(PHQ9_GAD7LongCenter, "PHQ9_GAD7LongCenter.csv", row.names = FALSE)
PHQ9_GAD7LongCenter = read.csv("PHQ9_GAD7LongCenter.csv", header = TRUE)

summary(PHQ9_GAD7LongCenter)
```

Ok now impute missing values
```{r}
head(PHQ9_GAD7LongCenter)
PHQ9_GAD7LongCenter$InterviewDate.x = NULL
m = 10
PHQ9_GAD7LongCenterOut = amelia(m = 10, PHQ9_GAD7LongCenter, noms = c("Employment.x", "LivingWhere.x", "Gender.x", "EducationYears.x"), ords = c("Depression.x", "Anxiety.x","ArrestedDays.x", "DAUseIllegDrugsDays.x"), ts = "time")

summary(PHQ9_GAD7LongCenterOut)

compare.density(PHQ9_GAD7LongCenterOut, var = "HealthStatus.x")


```
Getting missing data ready 
```{r}
PHQ9_GAD7LongCenter = lapply(1:m, function(x){PHQ9_GAD7LongCenterOut$imputations[[x]]})
head(PHQ9_GAD7LongCenter)

PHQ9_GAD7LongCenter = na.omit(PHQ9_GAD7LongCenter)
head(PHQ9_GAD7LongCenter[[1]])
dim(PHQ9_GAD7LongCenter[[1]])
```
Now getting descriptives.  Need to get the means and sd's later.
```{r}
PHQ9_GAD7LongCenterDescribe = lapply(1:m, function(x){describe(PHQ9_GAD7LongCenter[[x]])})
PHQ9_GAD7LongCenterDescribe
```
####Final
Data Analysis: Factors with housing: Employment + Depression.x
```{r}
head(PHQ9_GAD7LongCenter[[1]])

#test = glmer(LivingWhere.x ~ Employment.x+ Depression.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[1]], family = "binomial")

output = list()
coef_output =  NULL
se_output = NULL
df= NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + HealthStatus.x + Gender.x + EducationYears.x + Age.x + DAUseIllegDrugsDays.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$AICtab[5]
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

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = df[1])),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
results$expCoefs1 = exp(results$coefs1)
round(results,3) 
```
Run diagnostics on one data set then maybe try building for all of them
First develop an empty model
Run the code over all the models and just get the models then run the anova over all the models then get the diagnostics 
```{r}
outputEmpty = list()
output = list()


for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + HealthStatus.x + Gender.x + EducationYears.x + Age.x + DAUseIllegDrugsDays.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
}

for(i in 1:m){
  outputEmpty[[i]] = glmer(LivingWhere.x ~ 1 + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
}

anovaResults = NULL
for(i in 1:m){
  anovaResults[[i]] = anova(output[[i]], outputEmpty[[i]])
}

anovaResults

```
Now try to evaluate the level one and two residuals for each model maybe?
Ok HLMresid won't work with a list, so just check one of them.
Ok won't work with glm so cannot change the residuals
```{r}
modelResid = output[[1]]
resid1 = HLMresid(modelResid, level = 1, type = "LS", standardize = TRUE)
```


Try building the final model with additional variables that you included
```{r}
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL
df = NULL


for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + Age.x+  Gender.x+ EducationYears.x + DAUseIllegDrugsDays.x+ HealthStatus.x+ (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df = output[[i]] = output[[i]]$AICtab[5]
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
  p = round((2*pt(-abs(t_stat), df = df)),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output); results
results$expCoefs1 = exp(results$coefs1)
round(results,3)
```



Data Analysis: Factors associated with housing: Employment
```{r}
output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Age.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ EducationYears.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Gender.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + Anxiety.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + ArrestedDays.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + HealthStatus.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + DAUseIllegDrugsDays.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
Data Analysis: Factors with housing: Employment + Depression.x, PHQ9Base, not significant
```{r}
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + PHQ9Base + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
Data Analysis: Factors with housing: Employment + Depression.x, GAD7Base, not significant
```{r}
head(PHQ9_GAD7LongCenter[[1]])

output = list()
coef_output =  NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + GAD7Base + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
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
