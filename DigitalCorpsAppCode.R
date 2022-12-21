### Importing libraries 
library(dplyr)
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
                                 race = c(7, 9), 
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

data4 <- data3_cleanweight1
vis_dat(data4)
## After cleaning, numadult, drvisits, income2, mscode, hlthcvr1 are all very missing and I'd like to avoid using these variables 

## Work on describe the dataset and its basic characteristics (e.g., shape, variable types, basic stats)
## Before deciding on a model and relevant covariates we have 34 variables and 5000 individual ID's
## Continuous variables: NUMADULT	CHILDREN	WEIGHT2	DRVISITS
## Ordinal variables: GENHLTH	_AGEG5YR	_BMI5CAT	CHECKUP1	INCOME2	_EDUCAG	SLEPTIM1	MENTHLTH	_SMOKER3 
## Categorical variables: DIABETE3	_RACE	MSCODE	FLUSHOT6	EMPLOY1	SEX	MARITAL	CVDCRHD4	HLTHCVR1	CHCKIDNY	USEEQUIP	_TOTINDA	ADDEPEV2	RENTHOM1	EXERANY2	BLIND	DECIDE	HLTHPLN1	DIABETE3	_STATE	ASTHMA3	MARITAL
summary(data3$numadult) # Mean age is 7.8, around 54-55
summary(data3$children)
summary(data3_cleanweight1$weight_correct)
summary(data3_cleanweight1$drvisits)
summary(data3_cleanweight1$genhlth)
table(data3_cleanweight1$asthma3)
## Going to first choose models and covariates and stuff then come back to this to determine what data things need a high level overview 
## BUT everything is cleaned up which is nice!


## Choosing variables: 
## Social determinants of diabetes: 
## Medical determinants of diabetes: 



