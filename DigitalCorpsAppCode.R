### Importing libraries 
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
                                 DIABETE3.x == `DIABETE3...2` &
                                 DIABETE3.x == `DIABETE3...20`, 0, 1)) %>%
  ## There are 10 NA values in the marital difference variable-- want to see if they are missing for the same observations
  mutate(MARITAL_DIFF = ifelse((`MARITAL...8` == `MARITAL...23`) | 
                                 (is.na(`MARITAL...8`) & is.na(`MARITAL...23`)), 0 ,1)) 

## check the difference variable 
summary(data1$DIABET3_diff)
summary(data1$MARITAL_DIFF)

data2 <- num_ord_cat %>%
  select(-c(DIABETE3.y, `DIABETE3...2`, `DIABETE3...20`, `MARITAL...8`)) %>%
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

library(ggcorrplot)
model.matrix(~0+., data=data4_sample) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

## Normal sample
train <- data4_sample %>% 
  dplyr::sample_frac(0.7)
prop.table(table(train$diabete3))
test  <- dplyr::anti_join(data4_sample, train, by = 'personid')
prop.table(table(test$diabete3))

## We have a big problem with imbalanced classification
## Try oversampling first 
data_balanced_over_train <- ovun.sample(diabete3 ~ ., data = train, method = "over")$data

## Now undersampling  
data_balanced_under_train <- ovun.sample(diabete3 ~ ., data = train, method = "under")$data

## LOGISTIC REGRESSION- PLAIN SAMPLE
mylogit <- glm(diabete3 ~.-race_weight-personid, data = train, family = binomial(link = "logit"), 
               weights = race_weight)
summary(mylogit)
anova_logit <- anova(mylogit, test="Chisq")
# Got the coefficients! Use them on the test set now

# Make predictions
probabilities <- mylogit %>% predict(test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Take these and append as columns to the test dataframe 
predicted_classes_logit <- as.data.frame(predicted.classes)
test_predicted_logit <- cbind(predicted_classes_logit, test) 
logitconfusionmatrix <- confusionMatrix(data=as.factor(test_predicted_logit$predicted.classes), reference=as.factor(test_predicted_logit$diabete3))

## Creating ROC curve 
# create roc curve
roc_object <- roc(test$diabete3, probabilities)
# calculate area under curve
auc_logit <- auc(roc_object) ## 0.8125

## LOGISTIC REGRESSION -- OVERSAMPLED
mylogit_oversample <- glm(diabete3 ~.-race_weight-personid, data = data_balanced_over_train, family = binomial(link = "logit"), 
               weights = race_weight)
summary(mylogit_oversample)
anova_logit_over <- anova(mylogit_oversample, test="Chisq")
# Got the coefficients! Use them on the test set now

# Make predictions
probabilities_logit_over <- mylogit_oversample %>% predict(test, type = "response")
predicted_classes_over <- ifelse(probabilities_logit_over > 0.5, 1, 0)
# Take these and append as columns to the test dataframe 
predicted_classes_logit_over <- as.data.frame(predicted_classes_over)
test_predicted_logit_over <- cbind(predicted_classes_logit_over, test) 
logitconfusionmatrix_over <- confusionMatrix(data=as.factor(test_predicted_logit_over$predicted_classes_over), reference=as.factor(test_predicted_logit_over$diabete3))

## Creating ROC curve 
# create roc curve
roc_object_over <- roc(test$diabete3, probabilities_logit_over)
# calculate area under curve
auc_logit_over <- auc(roc_object) ## 0.8125

## LOGISTIC REGRESSION -- UNDERSAMPLED
mylogit_undersample <- glm(diabete3 ~.-race_weight-personid, data = data_balanced_under_train, family = binomial(link = "logit"), 
                          weights = race_weight)
summary(mylogit_undersample)
anova_logit_under <- anova(mylogit_undersample, test="Chisq")
# Got the coefficients! Use them on the test set now

# Make predictions
probabilities_logit_under <- mylogit_undersample %>% predict(test, type = "response")
predicted_classes_under <- ifelse(probabilities_logit_under > 0.5, 1, 0)
# Take these and append as columns to the test dataframe 
predicted_classes_logit_under <- as.data.frame(predicted_classes_under)
test_predicted_logit_under <- cbind(predicted_classes_logit_under, test) 
logitconfusionmatrix_under <- confusionMatrix(data=as.factor(test_predicted_logit_under$predicted_classes_under), 
                                              reference=as.factor(test_predicted_logit_under$diabete3))

## Creating ROC curve 
# create roc curve
roc_object_under <- roc(test$diabete3, probabilities_logit_under)
# calculate area under curve
auc_logit_under <- auc(roc_object) ## 0.8125

######### overall it doesn't seem to make a huge difference if we use the over or undersampled data
######### the over sampled data has a big overfitting problem so I will avoid it especially because AUC is not different
######### AND according to the confusion matrix the non-sampled model is much more accurate 
######### the auc is a little low and given that over and under sampling didn't make much of a difference I think that I could have chosen variables better
######### if I had more time I would try lots of different regressors then use Mallows CP and compare AUC's to see which model is best 
################################# DONE WITH LOGIT !!!!!!!!!!!!!! ################################################################## 
#build decision tree models
tree.normal <- rpart(diabete3 ~ .-race_weight-personid, data = train)
tree.over <- rpart(diabete3 ~ .-race_weight-personid, data = data_balanced_over_train)
tree.under <- rpart(diabete3 ~ .-race_weight-personid, data = data_balanced_under_train)

pred.tree <- as.data.frame(predict(tree.normal, newdata = test))
pred.tree.over <- as.data.frame(predict(tree.over, newdata = test))
pred.tree.under <- as.data.frame(predict(tree.under, newdata = test))

predicted.tree.nonsampled <- pred.tree %>%
  mutate(`predict(tree.normal, newdata = test)` = ifelse(`predict(tree.normal, newdata = test)` > 0.5, 1, 0))
predicted.tree.over1 <- pred.tree.over %>%
  mutate(`predict(tree.over, newdata = test)` = ifelse(`predict(tree.over, newdata = test)` > 0.5, 1, 0))
predicted.tree.under1 <- pred.tree.under %>%
  mutate(`predict(tree.under, newdata = test)` = ifelse(`predict(tree.under, newdata = test)` > 0.5, 1, 0))

decision.tree.over <- cbind(predicted.tree.over1, test)
decision.tree.under <- cbind(predicted.tree.under1, test)
decision.tree <- cbind(predicted.tree.nonsampled, test)

roc_over <- roc.curve(decision.tree.over$diabete3, decision.tree.over[,1]) ## auc = 0.703
roc_under <- roc.curve(decision.tree.under$diabete3, decision.tree.under[,1]) ## 0.695
roc <- roc.curve(decision.tree$diabete3, decision.tree[,1]) ## 0.5

decisiontree_confusionmatrix_over <- confusionMatrix(data=as.factor(decision.tree.over$`predict(tree.over, newdata = test)`), 
                                              reference=as.factor(decision.tree.over$diabete3))
decisiontree_confusionmatrix_under <- confusionMatrix(data=as.factor(decision.tree.under$`predict(tree.under, newdata = test)`), 
                                                     reference=as.factor(decision.tree.under$diabete3))
decisiontree_confusionmatrix <- confusionMatrix(data=as.factor(decision.tree$`predict(tree.normal, newdata = test)`), 
                                                      reference=as.factor(decision.tree$diabete3))
## Something weird happened in the last one-- Ajjit can you diagnose 

################################# DONE WITH DECISION TREE !!!!!!!!!!!!!! ################################################################## 

# Random Forests
rf <- randomForest(diabete3~.-race_weight-personid, data=train, proximity=TRUE, ntree=500) 
print(rf)


pred_randomforest_test <- predict(rf, newdata = test, type= "class")
predicted.classes.randomforest <- ifelse(pred_randomforest_test > 0.5, 1, 0)
test_predicted_randomforest <- cbind(predicted.classes.randomforest, test) 

confusionmatrixRandomForest <- confusionMatrix(data=as.factor(test_predicted_randomforest$predicted.classes.randomforest), 
                                               reference=as.factor(test_predicted_randomforest$diabete3))
# create roc curve
roc_object_randomforests <- roc(test$diabete3, predicted.classes.randomforest)
# calculate area under curve
auc_randomforests <- auc(roc_object_randomforests) ##0.5269


boruta.train <- Boruta(diabete3~.-race_weight-personid, data = train, doTrace = 2)
print(boruta.train)
getSelectedAttributes(boruta.train, withTentative = T)
rf_model1 <- randomForest(diabete3~ ageg5yr + bmi5cat + race + addepev2 +
                               menthlth + cvdcrhd4 + chckidny + totinda + sleptim1, 
                               data=train, proximity=TRUE, ntree=500)
print(rf1)
varImpPlot(rf1)
pred_randomforest_test1 <- predict(rf_model1, newdata = test, type= "class")
predicted.classes.randomforest1 <- ifelse(pred_randomforest_test1 > 0.5, 1, 0)
test_predicted_randomforest1 <- cbind(predicted.classes.randomforest1, test) 

confusionmatrixRandomForest1 <- confusionMatrix(data=as.factor(test_predicted_randomforest1$predicted.classes.randomforest), 
                                               reference=as.factor(test_predicted_randomforest1$diabete3))


# create roc curve
roc_object_randomforests1 <- roc(test$diabete3, predicted.classes.randomforest1)
# calculate area under curve
auc_randomforests <- auc(roc_object_randomforests1) ##0.5405



################################# DONE WITH RANDOM FORESTS !!!!!!!!!!!!!! ################################################################## 


# Boruta -- doing this on the whole dataset to see if we potentially missed any variables
## This could be worth exploring further! 
## Note for Ajjit: - This part of the
## "I developed a mdoel based on domain expertise but 
## here are some that I think are worth exploring further” 
boruta_train_data <- data3_cleanweight1 %>%
  select(-c(personid, state, weight_correct)) %>%
  filter(ageg5yr > 2) %>%
  filter(diabete3 == 1 | diabete3 == 3) %>%
  mutate(diabete3 = case_when(diabete3 == 3 ~ 0, 
                              TRUE ~ 1)) %>%
  na.omit()
boruta_train_data$diabete3 <- factor(boruta_train_data$diabete3)
boruta_train_data$race <- factor(boruta_train_data$race)
boruta_train_data$sex <- factor(boruta_train_data$sex)
boruta_train_data$ageg5yr <- factor(boruta_train_data$ageg5yr)
boruta_train_data$bmi5cat <- factor(boruta_train_data$bmi5cat)
boruta_train_data$smoker3 <- factor(boruta_train_data$smoker3)
boruta_train_data$addepev2 <- factor(boruta_train_data$addepev2)
boruta_train_data$cvdcrhd4 <- factor(boruta_train_data$cvdcrhd4)
boruta_train_data$chckidny <- factor(boruta_train_data$chckidny)
boruta_train_data$totinda <- factor(boruta_train_data$totinda)
boruta_train_data$genhlth <- factor(boruta_train_data$genhlth)
boruta_train_data$income2 <- factor(boruta_train_data$income2)
boruta_train_data$employ1 <- factor(boruta_train_data$employ1)
boruta_train_data$hlthcvr1 <- factor(boruta_train_data$hlthcvr1)
boruta_train_data$useequip <- factor(boruta_train_data$useequip)
boruta_train_data$asthma3 <- factor(boruta_train_data$asthma3)
boruta_train_data$hlthpln1 <- factor(boruta_train_data$hlthpln1)
boruta_train_data$decide <- factor(boruta_train_data$decide)
boruta_train_data$blind <- factor(boruta_train_data$blind)
boruta_train_data$exerany2 <- factor(boruta_train_data$exerany2)
boruta_train_data$renthom1 <- factor(boruta_train_data$renthom1)


boruta.train <- Boruta(diabete3~., data = boruta_train_data, doTrace = 2)
print(boruta.train)
getSelectedAttributes(boruta.train, withTentative = T)
## "children" "drvisits" "genhlth"  "ageg5yr"  "bmi5cat"  "income2"  "race" 
## "employ1" "cvdcrhd4" "useequip"
## "sleptim1" "menthlth"   "hlthcvr1"  "exerany2" "blind" "decide" 

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
