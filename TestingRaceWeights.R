# comparing logit with and without race weights
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(tidyverse)
library(tidylog, warn.conflicts = FALSE)
library(visdat)
library(naniar)
library(cdlTools)
library(pROC)
library(caret)
library(e1071)
library(Boruta)
library(Rcpp)
library(randomForest)
library(ROSE)
library(rpart)
library(ggcorrplot)


numeric <- read_csv("Data/data_numeric.csv")
categorical <- read_csv("Data/data_categorical.csv")
ordinal <- read_csv("Data/data_ordinal.csv")

num_ord <- left_join(numeric, ordinal, "PERSONID")
num_ord_cat <- left_join(num_ord, categorical, "PERSONID")

data1 <- num_ord_cat %>%
  select(starts_with("DIABET") | starts_with("MARITAL")) %>%
  mutate(DIABET3_diff = ifelse(DIABETE3.x == DIABETE3.y & 
                                 DIABETE3.x == `DIABETE3...2` &
                                 DIABETE3.x == `DIABETE3...20`, 0, 1)) %>%
  mutate(MARITAL_DIFF = ifelse((`MARITAL...8` == `MARITAL...23`) | 
                                 (is.na(`MARITAL...8`) & is.na(`MARITAL...23`)), 0 ,1)) 

data2 <- num_ord_cat %>%
  select(-c(DIABETE3.y, `DIABETE3...2`, `DIABETE3...20`, `MARITAL...8`)) %>%
  janitor::clean_names()

data3 <- data2 %>%
  replace_with_na(replace = list(weight2 = c(9999, 7777),
                                 children = 99, 
                                 drvisits = c(77, 99),
                                 income2 = c(77, 99),
                                 checkup1 = c(7, 9), 
                                 sleptim1 = c(77, 99), 
                                 menthlth = c(77, 99), 
                                 diabete3_x = c(7, 9), 
                                 race = c(6, 9), 
                                 flushot6 = c(7, 9), 
                                 employ1 = 9, 
                                 marital_23 = 9, 
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
  mutate(checkup1 = replace(checkup1, checkup1 == 88, 0)) %>%
  mutate(marital = marital_23) %>%
  mutate(diabete3 = diabete3_x) %>%
  select(-c(marital_23, diabete3_x))

data3_cleanweight1 <- data3 %>%
  mutate(
    weight_correct = case_when(
      nchar(as.character(weight2))==4 ~ as.numeric(str_sub(weight2, -3)) * 2.20462,
      TRUE ~ weight2
    )) %>%
  select(-weight2)

data3_cleanweight1$state <- fips(data3_cleanweight1$state, to="Abbreviation")


uniqueidcheck <- data3_cleanweight1 %>% 
  group_by(personid) %>% 
  mutate(duplicate_name = n()-1)

data4 <- data3_cleanweight1 %>%
  select(personid, ageg5yr, bmi5cat, sex, race, 
         addepev2, menthlth, cvdcrhd4, chckidny, 
         totinda, smoker3, diabete3, sleptim1) %>%
  filter(ageg5yr > 2) %>%
  filter(diabete3 == 1 | diabete3 == 3) %>%
  mutate(diabete3 = case_when(diabete3 == 3 ~ 0, 
                              TRUE ~ 1)) %>%
  na.omit()

data4_sample <- data4 %>%
  group_by(race) %>%
  mutate(Freq=n()) %>%
  mutate(perc_total_respondents = Freq/3808*100) %>%
  # Compare perc_total_respondents with actual national racial breakdown using https://www.census.gov/content/dam/Census/library/publications/2015/demo/p25-1143.pdf page 9
  # There are a disproportionate number of white respondents, which could potentially skew the sample 
  # Let's use weights to fix for heteroskedasticity AFTER establishing the training and test models because the representative-ness will change based on the random sample
  ungroup()


data4_sample$diabete3 <- as.numeric(data4_sample$diabete3)
data4_sample$race <- factor(data4_sample$race)
data4_sample$sex <- factor(data4_sample$sex)
data4_sample$ageg5yr <- factor(data4_sample$ageg5yr)
data4_sample$bmi5cat <- factor(data4_sample$bmi5cat)
data4_sample$smoker3 <- factor(data4_sample$smoker3)
data4_sample$addepev2 <- factor(data4_sample$addepev2)
data4_sample$cvdcrhd4 <- factor(data4_sample$cvdcrhd4)
data4_sample$chckidny <- factor(data4_sample$chckidny)
data4_sample$totinda <- factor(data4_sample$totinda)


prop.table(table(data4_sample$diabete3))
train_weight <- data4_sample %>% 
  dplyr::sample_frac(0.75)  %>%
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
  mutate(race_weight = perc_population/perc_total_respondents) %>%
  select(-c(perc_population, perc_total_respondents, Freq)) %>%
  ungroup()
prop.table(table(train$diabete3))

test_weight  <- dplyr::anti_join(data4_sample, train_weight, by = 'personid') %>%
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
  mutate(race_weight = perc_population/perc_total_respondents) %>%
  select(-c(perc_population, perc_total_respondents, Freq)) %>%
  ungroup()


mylogit_weight <- glm(diabete3 ~.-personid-race_weight, data = train_weight, family = binomial(link = "logit"), weights=race_weight)
probabilities_weight <- mylogit_weight %>% predict(test_weight, type = "response")
# The result is continuous values of probabilities of diabetes occurrence, but we are interested in predicting if someone will have diabetes or not. If anyone has a probability above 50%, we assume that they will be diagnosed with diabetes.
predicted_classes_weight <- ifelse(probabilities_weight > 0.5, 1, 0)
# Take these predictions and append as a column to the test dataframe 
# Do do that, we must first converting predicted classes to a dataframe
predicted_classes_logit_weight <- as.data.frame(predicted_classes_weight)
test_predicted_logit_weight <- cbind(predicted_classes_logit_weight, test_weight)

logitconfusionmatrix_weight <-confusionMatrix(data=as.factor(test_predicted_logit_weight$predicted_classes_weight), reference=as.factor(test_predicted_logit_weight$diabete3))
roc_object_weight <- roc(test_weight$diabete3, probabilities_weight)
auc_logit_weight <- auc(roc_object_weight) 





############# NO WEIGHTS 
train <- data4_sample %>% 
  select(-c(Freq, perc_total_respondents)) %>%
  dplyr::sample_frac(0.75)

test  <- dplyr::anti_join(data4_sample, train, by = 'personid')
mylogit <- glm(diabete3 ~.-personid, data = train, family = binomial(link = "logit"))
probabilities <- mylogit %>% predict(test, type = "response")
# The result is continuous values of probabilities of diabetes occurrence, but we are interested in predicting if someone will have diabetes or not. If anyone has a probability above 50%, we assume that they will be diagnosed with diabetes.
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
# Take these predictions and append as a column to the test dataframe 
# Do do that, we must first converting predicted classes to a dataframe
predicted_classes_logit <- as.data.frame(predicted_classes)
test_predicted_logit <- cbind(predicted_classes_logit, test) 

logitconfusionmatrix <-confusionMatrix(data=as.factor(test_predicted_logit$predicted_classes), reference=as.factor(test_predicted_logit$diabete3))
roc_object <- roc(test$diabete3, probabilities)
auc_logit <- auc(roc_object) 
