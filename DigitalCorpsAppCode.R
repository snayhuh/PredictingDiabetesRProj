### Importing libraries 
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(tidyverse)
library(tidylog, warn.conflicts = FALSE)
library(visdat)
library(naniar)
library(cdlTools)

### Importing datasets 
numeric <- read_csv("/Users/snerbs/Desktop/DigitalCorpsDataTest/data_numeric.csv")
categorical <- read_csv("/Users/snerbs/Desktop/DigitalCorpsDataTest/data_categorical.csv")
### We have a warning here for duplicated column names (diabetes and marital-- look into these columns)
ordinal <- read_csv("/Users/snerbs/Desktop/DigitalCorpsDataTest/data_ordinal.csv")

num_ord <- left_join(numeric, ordinal, "PERSONID")
num_ord_cat <- left_join(num_ord, categorical, "PERSONID")

data1 <- num_ord_cat %>%
  select(starts_with("DIABET") | starts_with("MARITAL")) %>%
  mutate(DIABET3_diff = ifelse(DIABETE3.x == DIABETE3.y & 
                                 DIABETE3.x == DIABETE3 &
                                 DIABETE3.x == DIABETE3_1, 0, 1)) %>%
  ## There are 10 NA values in the marital difference variable-- want to see if they are missing for the same observations
  mutate(MARITAL_DIFF = ifelse((MARITAL == MARITAL_1) | 
                                 (is.na(MARITAL) & is.na(MARITAL_1)), 0 ,1)) 

## check the difference variable 
summary(data1$DIABET3_diff)
summary(data1$MARITAL_DIFF)

data2 <- num_ord_cat %>%
  select(-c(DIABETE3.x, DIABETE3.y, DIABETE3_1, MARITAL_1)) %>%
  janitor::clean_names()

vis_dat(data2)
## numadult, drvisits, mscode, and hlthcvr1 have a lot of missing values
## Number of adults in a household, How many times have you been to a doctor, nurse, or other health professional in the past 12 months?, 
## Metropolitan Status Code, What is the primary source of your health care coverage?
## Want to avoid using these in the model and if I do use them then I'll have to do some imputation 
## Can use Naniar to see if the missingness is random or if it is correlated with age, race, gender, diabetes prevalence, or any other variables of interest 

## Downloading and merging is complete, now go to some exploration
## Looking at the weight variable 
## According to documentation, 9999 is refused to answer, so we can make those missing 
sum(is.na(data2$weight2))
## There are 50 values with missing weight 
## Need to know if this is correlated to something else, hopefully they are not 
## If they are, I might have to justify potentially dropping them or imputing the values 
## Even if it is not correlated to something else then I can impute the values
## Look at the distribution of weight 
## Find closest matches among other columns and use the average 

## Cleaning up the weight variable-- there are exactly seven observations that are in grams as seen by sorting the data by weight
data3 <- data2 %>%
  replace_with_na(replace = list(weight2 = c(9999, 7777),
                                 children = 99, 
                                 drvisits = c(77, 99),
                                 income2 = c(77, 99),
                                 checkup1 = c(7, 9), 
                                 sleptim1 = c(77, 99), 
                                 menthlth = c(77, 99), 
                                 diabete3 = c(7, 9), 
                                 race = c(6, 9), 
                                 flushot6 = c(7, 9), 
                                 employ1 = 9, 
                                 marital = 9, 
                                 cvdcrhd4 = c(7, 9), 
                                 hlthcvr1 = c(77, 99), 
                                 chckidny = c(7, 9), 
                                 useequip = c(7, 9),
                                 totinda = 9, 
                                 addepev2 = c(7, 9),
                                 renthom1 = c(7, 9),
                                 exerany2 = c(7, 9),
                                 blind = c(7, 9),
                                 decide = c(7, 9),
                                 hlthpln1 = c(7, 9), 
                                 genhlth = c(7, 9), 
                                 asthma3 = c(7, 9)))%>%
  mutate(children = replace(children, children == 88, 0)) %>%
  mutate(drvisits = replace(drvisits, drvisits == 88, 0)) %>%
  mutate(menthlth = replace(menthlth, menthlth == 88, 0)) %>%
  mutate(checkup1 = replace(checkup1, checkup1 == 88, 0))

vis_dat(data3)

data3_cleanweight1 <- data3 %>%
  mutate(
    weight_correct = case_when(
      nchar(as.character(weight2))==4 ~ as.numeric(str_sub(weight2, -3)) * 2.20462,
      TRUE ~ weight2
    )) %>%
  select(-weight2)
  
sum(is.na(data3_cleanweight1$weight_correct))

data3_cleanweight1$state <- fips(data3_cleanweight1$state, to="Abbreviation")

## Checking for unique ID's:
uniqueidcheck <- data3_cleanweight1 %>% 
  group_by(personid) %>% 
  mutate(duplicate_name = n()-1)

vis_dat(data3_cleanweight1)
## After cleaning, numadult, drvisits, income2, mscode, hlthcvr1 are all very missing and I'd like to avoid using these variables 

## Work on describe the dataset and its basic characteristics (e.g., shape, variable types, basic stats)
## Before deciding on a model and relevant covariates we have 34 variables and 5000 individual ID's
## Continuous variables: NUMADULT	CHILDREN	WEIGHT2	DRVISITS
## Ordinal variables: GENHLTH	_AGEG5YR	_BMI5CAT	CHECKUP1	INCOME2	_EDUCAG	SLEPTIM1	MENTHLTH	_SMOKER3 
## Categorical variables: DIABETE3	_RACE	MSCODE	FLUSHOT6	EMPLOY1	SEX	MARITAL	CVDCRHD4	HLTHCVR1	CHCKIDNY	USEEQUIP	_TOTINDA	ADDEPEV2	RENTHOM1	EXERANY2	BLIND	DECIDE	HLTHPLN1	DIABETE3	_STATE	ASTHMA3	MARITAL

## Going to first choose models and covariates and stuff then come back to this to determine what data things need a high level overview 
## BUT everything is cleaned up which is nice!

## Choosing variables: 
## Demographic 
##### BMI- bmi5cat
##### Age - ageg5yr
##### Gender - sex
##### Race  - race 
table(data4$race) ## going to weight these 

## Social 
##### Stress - menthlth
##### Depression - addepev2

## Lifestyle 
##### Smoking - smoker3
##### Physical exercise - totinda
##### Sleep - sleptim1

## It would be nice to include clinical factors such as skin thickness, blood pressure, and glucose levels 
## But we don't have those, so we can include chronic heart conditions - cvdcrhd4, chckidny
## Can also proxy for some of these using sleep 

data4 <- data3_cleanweight1 %>%
  select(personid, ageg5yr, bmi5cat, sex, race, 
         addepev2, menthlth, cvdcrhd4, chckidny, 
         totinda, smoker3, diabete3, sleptim1) %>%
  filter(ageg5yr > 2) %>%
  filter(diabete3 == 1 | diabete3 == 3) %>%
  mutate(diabete3 = case_when(diabete3 == 3 ~ 0, 
         TRUE ~ 1)) %>%
  na.omit()
  
vis_dat(data4)

data4_sample <- data4 %>%
  group_by(race) %>%
  mutate(Freq=n()) %>%
  mutate(perc_total_respondents = Freq/3808*100) %>%
  mutate(perc_population = case_when(race == 1 ~  77.5,
                                     race == 2 ~ 13.2,
                                     race == 3 ~ 1.2, 
                                     race == 4 ~ 5.4, 
                                     race == 5 ~ 0.2, 
                                     race == 7 ~ 2.5, 
                         TRUE~15.3)) %>%
  ## Used https://www.census.gov/content/dam/Census/library/publications/2015/demo/p25-1143.pdf page 9
  ## subtracted 2.1 from the hispanic because we have 17.4 percent hispanic and that is not ONLY hispanic
  mutate(race_weight = perc_population/perc_total_respondents) %>%
  select(-c(perc_population, perc_total_respondents, Freq)) %>%
  ungroup()

## Imputing would be nice but doesn't make a lot of sense given the variables we've selected because the most missing var is bmi and it is only at 6%
## General rule of thumb is: Imputation works best when many variables are missing in small proportions such that a complete case analysis might render 60-30% completeness, 
## but each variable is perhaps only missing 10% of its values. Because we dont meet that threshold, we will just drop all missing values 

## But that means we can start modeling! 

## Logistic regression	 
data4_sample$diabete3 <- factor(data4_sample$diabete3)
data4_sample$race <- factor(data4_sample$race)
data4_sample$sex <- factor(data4_sample$sex)
data4_sample$ageg5yr <- factor(data4_sample$ageg5yr)
data4_sample$bmi5cat <- factor(data4_sample$bmi5cat)
data4_sample$smoker3 <- factor(data4_sample$smoker3)
data4_sample$addepev2 <- factor(data4_sample$addepev2)
data4_sample$cvdcrhd4 <- factor(data4_sample$cvdcrhd4)
data4_sample$chckidny <- factor(data4_sample$chckidny)
data4_sample$totinda <- factor(data4_sample$totinda)

mylogit <- glm(diabete3 ~., data = data4_sample, family = binomial(link = "logit"), 
               weights = race_weight)
summary(mylogit)
anova(mylogit, test="Chisq")

## Neural network	

## Linear SVM	
