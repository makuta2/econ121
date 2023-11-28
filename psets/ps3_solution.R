# This R script presents solutions to ECON 121 Problem Set 3.

# Note: You may have noticed that the dataset includes a
#       primary sampling unit identifier and a sampling 
#       weight. It would be reasonable to cluster standard
#       errors at the PSU level and weight using sampling
#       weights, but these details were not the focus of the
#       problem set and are therefore not required.

 # # # # # #
# Problem 2 # 
 # # # # # #
  
# Summary statistics appear below. 16 percent of the sample 
# reports being in fair or poor health, and 13 percent died 
# died before 2019. The sample has a median age of 49. (The mean 
# age is less meaningful because age was top-coded at 85. You
# did not need to notice this.) 56 percent of the sample
# is female, perhaps surprisingly. This gender imbalance
# has two sources. First, men and women responded to the 
# survey at different rates, so the gender imbalance shrinks
# when we use the sampling weights. Second, men die at 
# higher rates than women, so the gender imbalance grows
# with age. For the outcome variables, 16 percent of the sample
# reports being in fair or poor health, while 13 percent dies
# by 2019.

# clear environment and load packages
rm(list=ls())
library(tidyverse)
library(fixest)
library(mfx)
library(car)

# load dataset
load(url("https://github.com/tvogl/econ121/raw/main/data/nhis2010.Rdata"))

# generate fair/poor health dummy
table(nhis2010$health)
nhis2010$fpoor <- ifelse(nhis2010$health == "Fair" |
                         nhis2010$health == "Poor", 1, 0)

# summarize the dataset
summary(nhis2010)

ggplot(nhis2010, aes(x=age)) +
  geom_histogram()
  
mean(nhis2010$male) # 44% male, 56% female
weighted.mean(nhis2010$male,w = nhis$sampweight) # 48% male, 52% female with weights


mean(nhis2010$fpoor, na.rm = TRUE)
mean(nhis2010$mort, na.rm = TRUE) 

 # # # # # #
# Problem 3 # 
 # # # # # #

# 5-year mortality is higher for people with fair/poor health
# than for people with good/very good/excellent health. Thus,
# self-reported health status is predictive of mortality. In
# both groups, 5-year mortality rises non-linearly with age.

nhis2010 %>% 
  drop_na(age, fpoor, mort) %>% # drop missing values
  group_by(age, fpoor) %>% # group by age and fpoor
  summarise(mort = mean(mort)) %>% # mean of mort
  ggplot(aes(x = age, y = mort, color = factor(fpoor))) + # factor() to make the color scale discrete
    geom_line() +
    labs(x="age", y="mortality rate", color = "") # color = "" to remove the legend title

 # # # # # #
# Problem 4 # 
 # # # # # #

# Rates of mortality and fair/poor health decline with
# family income. The same general pattern holds
# for education as well, although individuals with 
# post-graduate education do not appear to be in worse 
# health than college graduates.

# Mortality by income
nhis2010 %>%
  drop_na(incfam, mort) %>%
  group_by(incfam) %>%
  summarize(mean_mort = mean(mort)) %>%
  ggplot(aes(x = incfam, y = mean_mort)) +
    geom_bar(stat = "identity")

# Health by income
nhis2010 %>%
  drop_na(incfam, fpoor) %>%
  group_by(incfam) %>%
  summarize(mean_fpoor = mean(fpoor)) %>%
  ggplot(aes(x = incfam, y = mean_fpoor)) +
  geom_bar(stat = "identity")

# Mortality by education
nhis2010 %>%
  drop_na(edyrs, mort) %>%
  mutate(edlev = case_when((edyrs<12)                 ~ 1, # code edlev as numeric 
                          (edyrs==12)                 ~ 2, # so the graph is ordered 
                          (edyrs>=13 & edyrs<15)      ~ 3, # correctly
                          (edyrs==16)                 ~ 4,
                          (edyrs>=17)                 ~ 5)) %>%
  group_by(edlev) %>%
  summarize(mean_mort = mean(mort)) %>%
  ggplot(aes(x = edlev, y = mean_mort)) +
    geom_bar(stat = "identity")

# Health by education
nhis2010 %>%
  drop_na(edyrs, fpoor) %>%
  mutate(edlev = case_when((edyrs<12)                  ~ 1, 
                           (edyrs==12)                 ~ 2, 
                           (edyrs>=13 & edyrs<15)      ~ 3,
                           (edyrs==16)                 ~ 4,
                           (edyrs>=17)                 ~ 5)) %>%
  group_by(edlev) %>%
  summarize(mean_fpoor = mean(fpoor)) %>%
  ggplot(aes(x = edlev, y = mean_fpoor)) +
    geom_bar(stat = "identity")

 # # # # # #
# Problem 5 # 
 # # # # # #

# Because age and education have non-linear relation-
# ships with health, I include a series of dummy 
# variables for categories. I use the education cate-
# gories from above, and 10-year age intervals.

# For both outcomes and for all three models, the
# results show that mortality and fair/poor health 
# decline with income, decline with education, and 
# rise with age. One surprising result is that 
# conditional on the socioeconomic variables, racial
# gaps in mortality are small and insignificant.
# There are larger racial gaps in fair/poor health.
# Another surprising result is that Hispanics have
# low mortality risk (conditional on the other
# covariates).

# The linear probability results are similar to the
# probit and logit average marginal effects, although 
# the similarity is much stronger for fair/poor health
# than for mortality. You did not need to comment
# on the reason in your response, but the larger 
# difference in the case of mortality is probably
# due to the fact that mortality risk is exceptionally
# low across much of the age distribution, so that
# the marginal effect is calculated in the flatter part 
# of the CDF.

# I will assign the regression models to names and then 
# use summary(model_name), so all coefficients are reported. 
# Otherwise, as you know, the results table gets cut off.

# Generate age and education categories
nhis2010 <-
  nhis2010 %>%
  mutate(agecat = floor(age/10)*10,
         edlev = case_when((edyrs<12)                  ~ 1, 
                           (edyrs==12)                 ~ 2, 
                           (edyrs>=13 & edyrs<15)      ~ 3,
                           (edyrs==16)                 ~ 4,
                           (edyrs>=17)                 ~ 5))

# Mortality analyses
ols_mort <-
  feols(mort ~ incfam + factor(edlev) + factor(agecat) +
               black + hisp + asian + other, 
        data = nhis2010, vcov = 'hetero')
summary(ols_mort)

probit_mort <- 
  feglm(mort ~ incfam + factor(edlev) + factor(agecat) +
               black + hisp + asian + other, 
        data = nhis2010, vcov = 'hetero')
summary(probit_mort)

logit_mort <- 
  feglm(mort ~ incfam + factor(edlev) + factor(agecat) +
          black + hisp + asian + other, 
        data = nhis2010, vcov = 'hetero')
summary(logit_mort)

# Marginal effects for the probit and logit models
probitmfx(mort ~ incfam + factor(edlev) + factor(agecat) +
                 black + hisp + asian + other, 
          data = nhis2010, atmean = TRUE, robust = TRUE)

logitmfx(mort ~ incfam + factor(edlev) + factor(agecat) +
                black + hisp + asian + other, 
         data = nhis2010, atmean = TRUE, robust = TRUE)

# Fair/poor health analyses
ols_fpoor <-
  feols(fpoor ~ incfam + factor(edlev) + factor(agecat) +
                black + hisp + asian + other, 
        data = nhis2010, vcov = 'hetero')
summary(ols_fpoor)

probit_fpoor <- 
  feglm(fpoor ~ incfam + factor(edlev) + factor(agecat) +
                black + hisp + asian + other, 
        data = nhis2010, vcov = 'hetero', family = 'probit')
summary(probit_fpoor)

logit_fpoor <- 
  feglm(fpoor ~ incfam + factor(edlev) + factor(agecat) +
                black + hisp + asian + other, 
        data = nhis2010, vcov = 'hetero', family = 'logit')
summary(logit_fpoor)

# Marginal effects for the probit and logit models
probitmfx(fpoor ~ incfam + factor(edlev) + factor(agecat) +
                  black + hisp + asian + other, 
          data = nhis2010, atmean = TRUE, robust = TRUE)

logitmfx(fpoor ~ incfam + factor(edlev) + factor(agecat) +
                 black + hisp + asian + other, 
         data = nhis2010, atmean = TRUE, robust = TRUE)

# Odds ratio effects for the logit model
logitor(fpoor ~ incfam + factor(edlev) + factor(agecat) +
                black + hisp + asian + other, 
         data = nhis2010, robust = TRUE)

 # # # # # #
# Problem 6 # 
 # # # # # #

# I used the logit model for this test.

# It is possible to use car::deltaMethod() with coefficients on  
# the categories of factor variables, but it's easier to work with 
# dummy variables. For pedagogical purposes, I will generate income 
# and education category dummies, and then I will re-run the model.

table(nhis2010$incfam)
table(nhis2010$edlev)

nhis2010 <-
  nhis2010 %>%
  mutate(inc_35_50 = ifelse(incfam=="$35,000 - $49,999",1,0), 
         inc_50_75 = ifelse(incfam=="$50,000 - $74,999",1,0), 
         inc_75_100 = ifelse(incfam=="$75,000 - $99,999",1,0), 
         inc_gt_100 = ifelse(incfam=="$100,000 and over",1,0), 
         ed_12 = ifelse(edlev==2,1,0),
         ed_13_15 = ifelse(edyrs>12 & edyrs<16,1,0),
         ed_16 = ifelse(edyrs==16,1,0),
         ed_gt16 = ifelse(edyrs>16,1,0))


logit_mort2 <- 
  feglm(mort ~ inc_35_50 + inc_50_75 + inc_75_100 + inc_gt_100 +
               ed_12 + ed_13_15 + ed_16 + ed_gt16 + factor(agecat) +
               black + hisp + asian + other, 
        data = nhis2010, vcov = 'hetero', family = 'logit')
summary(logit_mort2)

# This model is the same as the one above. We just coded the
# categorical variables as dummies. The difference in log odds 
# between Groups A and B is given by:

deltaMethod(logit_mort2, "asian - black - ed_16 - inc_gt_100")

# Since this is positive, we conclude that the poorer, less-
# educated Asian group have higher mortality risk than
# richer, more-educated Black group. The 95% confidence
# interval does not include 0, so the difference is
# statistically significant at the 5% level. If we 
# exponentiate this difference, we get:

exp(.61244)

# which implies that the odds of dying are 84 percent higher
# for the poorer, less-educated, Asian group. You did not
# need to state this quantity in your answer.

# It's likely that this model is not the best for 
# testing differences between these groups. It would
# be better to include interactions of race and income.

 # # # # # #
# Problem 7 # 
 # # # # # #

# We probably should not interpret these results as causal.
# One problem is that there are many confounding variables
# that we do not observe but may jointly determine health
# and income, for instance place of birth. Another problem
# is that there may be reverse causality, i.e. health may
# affect income.

 # # # # # #
# Problem 8 # 
 # # # # # #

# I use the logit model again, and I exponentiated the 
# coefficients for interpretability. I control for
# insurance status, smoking status, exercise, bacon
# consumption, and obesity. To keep the samples the same
# in the regressions with and without the additional
# control variables, I run the long regression
# and the short regression on the same sample, which
# required subsetting the data first. This was not required.

# Smoking, exercise, and obesity predicted mortality: ever smoking raised 
# the odds of death by 62%, ever exercising reduced the odds by 59%, 
# and obesity raised the odds by 85%. In contrast, uninsurance and binge  
# drinking had weak to null relationships with mortality. The patterns 
# explain part of the socioeconomic gradient in health. After 
# controlling for these variables, the odds ratio on the highest 
# income category rose from 0.21 to 0.25 and that on the >16 years of 
# education dummy rose from 0.26 to 0.36. Because the odds ratios are  
# moving closer to 1, mortality gaps are smaller after controlling for 
# health behaviors. Health behavior explains a larger share of the 
# education-mortality relationship than the income-mortality relationship.

# Recode behavior variables as 0/1 dummies
nhis2010 <-
  nhis2010 %>%
  mutate(exev = ifelse(vig10fwk > 0, 1, 0), # ever exercise
         bingev = ifelse(alc5upyr > 0, 1, 0), # ever binge drink
         obese = ifelse(bmi >= 30, 1, 0) ) # obese if bmi>=30

# Subset data to non-missing obs
nhis_subset <- 
  nhis2010 %>%
  drop_na(mort, incfam, edlev, agecat, white, black, hisp, 
          uninsured, smokev, exev, bingev, obese)

# Run regression without health behaviors
logitor(fpoor ~ incfam + factor(edlev) + factor(agecat) +
                black + hisp + asian + other, 
        data = nhis_subset, robust = TRUE)

# Run regression with health behaviors
logitor(fpoor ~ incfam + factor(edlev) + factor(agecat) +
                black + hisp + asian + other +
                uninsured + smokev + exev + bingev + obese, 
        data = nhis_subset, robust = TRUE)

