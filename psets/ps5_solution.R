# This R script presents solutions to ECON 121 Problem Set 5.

# Clear environment, load R packages, load dataset
rm(list=ls())
library(tidyverse)
library(fixest)
crime <- read_csv("https://github.com/tvogl/econ121/raw/main/data/crime_argentina.csv")

 # # # # # #
# Problem 2 # 
 # # # # # #
# Summary statistics appear below. The average conscription rate is 50
# percent, and the average crime rate is 7 percent.
summary(crime)

# There are large variations in conscription rates by birth year, and
# somewhat smaller variations in crime rates by birth year.
crime %>%
  group_by(birthyr) %>%
  summarize(conscripted_mean = mean(conscripted),
            crimerate_mean = mean(crimerate))

# To shed light on whether these birth year differences are statistically
# significant (this was not necessary for full credit), we can run 
# regressions as follows:
conscripted_model <- feols(conscripted ~ factor(birthyr), data = crime, vcov = 'hetero')
conscripted_model
fitstat(conscripted_model, "f")

crimerate_model <- feols(conscripted ~ factor(birthyr), data = crime, vcov = 'hetero')
crimerate_model
fitstat(crimerate_model, "f")
# The fitstat() output at the end tells us the p-value from the joint F test 
# on all estimated coefficients. For both regressions, this p-value is less  
# than 0.05, indicating that the birth year variations in both variables are
# statistically significant.
#
# I used fitstat() because the direct output from feols() does not include 
# the joint F-statistic.

 # # # # # #
# Problem 3 # 
 # # # # # #
  
# OLS regressions of various crime rates on conscription rates.  
# Crime rates are higher in cells with higher conscription rates.
feols(crimerate ~ conscripted + factor(birthyr) + indigenous + naturalized,
      data = crime,
      vcov = 'hetero')

# This association would reflect a causal effect only if the lottery
# completely determined conscription -- i.e., there was perfect compliance.
# But we know from the description that some individuals were exempted from
# military service, even if the lottery made them eligible. If people
# exempted from military service have different crime propensities than
# non-exempt people, then the OLS estimate is biased. For instance,
# clerics and individuals with dependents may be less prone to crime than
# non-exempt individuals, which would bias us toward finding a larger 
# (more positive) association between conscription and crime.

# Alternatively, it may be that patriotic individuals are more likely to
# sign up even if they are not eligible for the lottery, and are less likely
# to find ways to become exempt. In this case, conscription would be
# positively correlated with patriotism. If patriotic individuals are 
# also less likely to commit crimes, then these patterns would bias us 
# toward finding a smaller (more negative) association betetween conscription
# and crime.

 # # # # # #
# Problem 4 # 
 # # # # # #

crime <- 
  crime %>%
  mutate(eligible = ifelse((draftnumber>=175 & birthyr==1958) |
                           (draftnumber>=320 & birthyr==1959) |
                           (draftnumber>=341 & birthyr==1960) |
                           (draftnumber>=350 & birthyr==1961) |
                           (draftnumber>=320 & birthyr==1962), 1, 0))

 # # # # # #
# Problem 5 # 
 # # # # # #

# To estimate the first-stage effect of eligibility on conscription, we
# regress conscription rates on eligibility, controlling for birth year 
# dummies. The birth year dummies are required because the eligibility
# rule depended on birth year -- elibility is randomly assigned within
# each birth year, but it varies systematically across birth years.
# Ethnic composition covariates are not necessary because the assignment
# of eligibility is unrelated to ethnicity.
feols(conscripted ~ eligible + factor(birthyr),
      data = crime,
      vcov = 'hetero')

# The results indicate that eligibility has a large, positive, and 
# significant effect on conscription. Eligibility raises conscription 
# rates by 66 percentage points, with a t-statistic of 572!

 # # # # # #
# Problem 6 # 
 # # # # # #

# Like the first stage, the reduced form regression must control for
# birth year dummies. This result reflects a causal effect, since 
# eligibility is randomly assigned conditional on year of birth. However,
# it reflects the average effect of *eligibility*, not *conscription*.
feols(crimerate ~ eligible + factor(birthyr),
      data = crime,
      vcov = 'hetero')

# We find that eligibility raises crime rates (by 0.2 %-points).

 # # # # # #
# Problem 7 # 
 # # # # # #

# We can compute the instrumental variables estimator by dividing the
# reduced form coefficient by the first stage coefficient.
0.0017595/0.6587458

 # # # # # #
# Problem 8 # 
 # # # # # #

# We get exactly the same point estimates by running 2SLS.
feols(crimerate ~ factor(birthyr) | conscripted ~ eligible,
      data = crime,
      vcov = 'hetero')

# We find that conscription raises the crime rate by .27 percentage points,
# This magnitude is remarkably similar to the OLS result in Question 2. This 
# similarity suggests that perhaps after all, the OLS results were not biased.

 # # # # # #
# Problem 9 # 
 # # # # # #

# (1) The instrument is clearly relevant, with a first-stage t-statistic of over 500.

# (2) It also likely satisfies the independence assumption, since the lottery effectively
#     randomizes eligibility.

# (3) Whether it satisfies the exclusion restriction is a little less clear. 
#     It seems likely that eligibility only affects crime through its effect
#     on conscription, but alternative pathways are possible. For instance, 
#     suppose eligible people who were opposed to military service evaded 
#     conscription by hiding from the authorities, which exposed them to more crime. 

# (4) The monotonicity condition is likely to be satisfied. It is very difficult
#     to fathom why a person who would volunteer for the military without being
#     drafted would refuse to serve if drafted.

# Note: Instead of 2-4, you could have also discussed the instrument exogeneity
#       assumption from the constant treatment effects setup.

 # # # # # #
# Problem 10 # 
 # # # # # #

# As noted above, the 2SLS result for crimerate suggests that conscription
# raises the probability of having a criminal record by 0.27 percentage points.
# Viewed through the lens of heterogeneous treatment effects, this estimate
# reflects the average treatment effect among people who were induced
# to serve by the draft lottery. It does not reflect the effects of
# conscription among people who would have volunteered anyway (always takers),
# nor the effects of conscription among exempted people (never takers).

# If always takers exist in this context, then we should call the estimand
# a LATE, not a TOT. Recall the first stage regression:
feols(conscripted ~ eligible + factor(birthyr),
      data = crime,
      vcov = 'hetero')

# Because the intercept is positive, we can conclude that some non-eligible
# people did sign up for the military. So there are always takers, and we
# should call it a LATE.

 # # # # # #
# Problem 11 # 
 # # # # # #
# Part A
# The scatterplot represents the effect of crossing the cutoff on conscription.
# It shows an opportunity to use the cutoff rule in a fuzzy RD design. It is
# "fuzzy" because conscription rates are positive below the cutoff and less than
# 1 above the cutoff, so the change in treatment probability is smaller than 1.
ggplot(data = crime, aes(draftnumber, conscripted)) +
  geom_point() + 
  facet_wrap(~ birthyr)

# PART B
# generate distance to cutoff, filter to window from -100 to +100
crime_local <- 
  crime %>%
  mutate(distance = (draftnumber-175) * ifelse(birthyr == 1958,1,0) +
                    (draftnumber-320) * ifelse(birthyr == 1959,1,0) +
                    (draftnumber-341) * ifelse(birthyr == 1960,1,0) +
                    (draftnumber-350) * ifelse(birthyr == 1961,1,0) +
                    (draftnumber-320) * ifelse(birthyr == 1962,1,0)) %>%
  filter(distance<=100 & distance>=-100)

# PART C
# draw the first-stage scatterplot pooling all birth years
ggplot(data = crime_local, aes(distance, conscripted)) +
  geom_point()
# the discontinuity is clear. crossing the threshold has an effect on conscription.

# PART D
# draw the reduced-form scatterplot pooling all birth years
ggplot(data = crime_local, aes(distance, crimerate)) +
  geom_point()
# here, any discontinuity is very hard to see here. crossing the threshold does 
# not seem to have an effect on conscription.

# PART E
# estimate 2sls regression using local linear regression with a bandwidth of 100
crime_local <- crime_local %>% mutate(distXelig = distance*eligible)
fuzzy_rd <- 
  feols(crimerate ~ distance + distXelig | conscripted ~ eligible,
        data = crime_local,
        vcov = 'hetero')
summary(fuzzy_rd, stage = 1)
summary(fuzzy_rd, stage = 2)
# we can see from the first stage that crossing the cutoff raises conscription 
# by almost 60 percentage points. but there is no significant discontinuity 
# in crime rates. these findings confirm the patterns in the scatter plots.

# PART F
# why this disappointing result? a big problem is that by focusing right around 
# the cutoff, we have increased the variance (std. errors) of our estimator 
# a lot. the 2sls result in problem 8 had an SE of .0008. here we have a 
# standard error of .0037, much larger! here, we cannot reject an effect
# of 0.0027, which is what we found in problem 8.


