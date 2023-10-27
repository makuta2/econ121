# analysis of self-reported sadness in the national health interview survey.

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(fixest)
# I will load the mfx package below to estimate marginal effects and odds ratios

# read in data
load(url("https://github.com/tvogl/econ121/raw/main/data/nhis2010.Rdata"))

# summarize
summary(nhis2010)

# drop observations with sadness missing/NA.
nhis2010 <- nhis2010 %>% drop_na(asad)

# generate a variable that equals one if ever sad, zero otherwise.
table(nhis2010$asad)
nhis2010$anysad <- ifelse(nhis2010$asad != "None of the time", 1, 0)
table(nhis2010$anysad)
# could have also used: nhis2010 <- nhis2010 %>% mutate(anysad = ifelse(asad > 0, 1, 0))
# or: nhis2010 <- mutate(nhis2010, anysad = ifelse(asad > 0, 1, 0))

# SOME DESCRIPTIVE GRAPHS

# first, sadness by education. I will draw the markers proportional
# to the number of observations in each cell to emphasize that there
# are very few individuals with less than 10 years of education.
nhis2010 %>%
  drop_na(edyrs) %>%
  group_by(edyrs) %>%
  summarize(mean_anysad = mean(anysad), # mean of anysad within each education level
            numobs = n()) %>%
  ggplot(aes(x = edyrs, y = mean_anysad, size=numobs)) +
    geom_point() # scatter plot

# next, sadness by age and sex. here, I won't keep track of cell size.
nhis2010 %>%
  drop_na(age, male) %>%
  group_by(age, male) %>%
  summarize(mean_anysad = mean(anysad)) %>%
  ggplot(aes(x = age, y = mean_anysad, color=factor(male))) + # I use factor() so the color scale is categorical, not continuous
    geom_line() # line plots

# finally, sadness by marital status
nhis2010 %>%
  drop_na(marstat) %>%
  group_by(marstat) %>%
  summarize(mean_anysad = mean(anysad)) %>%
  ggplot(aes(x = marstat, y = mean_anysad)) + 
    geom_bar(stat = "identity") # bar plot. identity just tells it to plot the y we specified, instead of a count or proportion.

# COMPARING OLS, LOGIT, AND PROBIT

# estimate a linear probability model, a logit, and a probit.
# then generate predicted probabilities for each of these approaches.
# then compare the predicted probabilities. we use the feglm() function
# from the fixest package.

ols_model <- feols(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
                   data = nhis2010, 
                   vcov = 'hetero')
ols_model
nhis2010$p_ols <- predict(ols_model, nhis2010, type="response")

probit_model <- feglm(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
                      data = nhis2010,
                      vcov = 'hetero', 
                      family = 'probit')
probit_model
nhis2010$p_probit <- predict(probit_model, nhis2010, type="response")

logit_model <- feglm(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
                     data = nhis2010,
                     vcov = 'hetero', 
                     family = 'logit')
logit_model
nhis2010$p_logit <- predict(logit_model, nhis2010, type="response")

# summarize predicted values from all three models - very similar!!
nhis2010 %>%
  select(p_ols, p_probit, p_logit) %>%
  summary()

# correlation matrix of predicted values from all three models - very close to 1!!
# need to drop_na() because otherwise cor() returns an error
nhis2010 %>%
  select(p_ols, p_probit, p_logit) %>%
  drop_na(p_ols, p_probit, p_logit) %>%
  cor()

# MARGINAL EFFECTS

# now let's compute marginal effects at the means of the independent
# variables. we use the mfx package. it has slightly different syntax
# from fixest::feglm(), but it is intuitive and convenient. it allows
# us to specify whether we want marginal effects at the averages of the
# covariates or averages of the individual marginal effects. we will
# see they are very similar.

library(mfx) # for easy estimation of marginal effects and odds ratios

# marginal effects at the means of the independent variables
logitmfx(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
         data = nhis2010, 
         atmean = TRUE, 
         robust = TRUE)
probitmfx(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
          data = nhis2010, 
          atmean = TRUE, 
          robust = TRUE)

# average of the individual marginal effects
logitmfx(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
         data = nhis2010, 
         atmean = FALSE, 
         robust = TRUE)
probitmfx(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
          data = nhis2010, 
          atmean = FALSE, 
          robust = TRUE)

# ODDS RATIOS

# finally, we can also estimate odds ratios in the logit setting.
# the mfx package also allows estimation of odds ratios:
logitor(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
        data = nhis2010, 
        robust = TRUE)
# these are especially convenient for binary independent variables.
# for instance, men have 32% lower odds of reporting sadness than women.