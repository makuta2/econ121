# This R script presents solutions to ECON 121 Problem Set 4.

# Clear environment, load R packages
rm(list=ls())
library(tidyverse)
library(fixest)

# Load the dataset
load("/Users/tvogl/Dropbox/courses/econ121/data/nlsy_kids/nlsy_kids.Rdata")

 # # # # # #
# Problem 2 # 
 # # # # # #
  
# Summary statistics appear below. 21 percent of the sample participated
# in HS. 32 percent of the sample is black, and 20 percent is Hispanic.
# Average mother's education is 12 years. 3 in 10 repeat a grade, another
# 3 in 10 go to college, and 7 in 10 graduate high school. Also worthy 
# of note is the number of NA values, which is very high for ppvt_3.
# This high level of "missingness" will be important later.
summary(nlsy_kids)

# The question asks about the backgrounds of kids who participated in HS.
# HS participants are more likely to be black, have lower family income,
# and have less educated mothers, on average. They are also more likely 
# to repeat a grade and less likely to go to college. However, these 
# differences in long-term outcomes may reflect selection bias rather
# than the effects of HS. In other words, HS participants may have
# worse outcomes because they come from disadvantaged backgrounds.
nlsy_kids %>% 
    group_by(head_start) %>% 
    summarize(black = mean(black, na.rm = TRUE),
              lninc_0to3 = mean(lninc_0to3, na.rm = TRUE),
              momed = mean(momed, na.rm = TRUE),
              repgrade = mean(repgrade, na.rm = TRUE),
              somecoll = mean(somecoll, na.rm = TRUE))

 # # # # # #
# Problem 3 # 
 # # # # # #
  
# Run an OLS regression of the age 5-6 test score on the HS indicator,
# clustering standard errors by mom_id.
feols(comp_score_5to6 ~ head_start,
      data = nlsy_kids,
      vcov = ~mom_id)

# For reference, compute the standard deviation of the test score. 
sd(nlsy_kids$comp_score_5to6, na.rm=TRUE)

# Average scores are 5.8 points lower for participants than for non-participants.
# The association is highly statistically significant and represents roughly
# one-quarter of a standard deviation in test scores. If we assumed participation
# is exogenous, then we would conclude that HS reduces test scores by one-
# quarter of a standard deviation on average. However, we already know that
# participation is associated with several background characteristics that
# are likely to have independent effects on test scores, which implies that
# the residual is correlated with HS participation. As a result, participation
# is not exogenous, and we should not interpret the association as a causal 
# effect. The bias is probably negative, since disadvantaged families select
# into HS, and kids from disadvantaged families may tend to have worse long-term
# outcomes.

 # # # # # #
# Problem 4 # 
 # # # # # #

# First create a data frame of families instead of kids. We can do so 
# using group_by(), as follows:
nlsy_families <-
  nlsy_kids %>%
  drop_na(comp_score_5to6, head_start) %>%
  group_by(mom_id) %>%
  summarise(mean_test = mean(comp_score_5to6),
            mean_head_start = mean(head_start))

# Now estimate OLS using the family averages
feols(mean_test ~ mean_head_start, 
      data = nlsy_families, 
      vcov = 'hetero')

# The estimated coefficient on HS participation is now even more negative
# than the one from question 3. That is consistent with family-level 
# omitted variables: kids from disadvantaged families enroll in HS,
# and they have have lower average test scores due to their disadvantage.

 # # # # # #
# Problem 5 # 
 # # # # # #

# Estimate the model with mother fixed effects.
feols(comp_score_5to6 ~ head_start | mom_id,
      data = nlsy_kids)

# The fixed effect model suggests that HS participation raises test scores, 
# in contrast to the negative effects suggested by OLS and the between effect
# model. The likely reason is that between-family variation in HS 
# participation is correlated with family disadvantage, which biases us toward
# finding a negative association in the pooled and between effect models.
# The full-sample fixed effect model without controls indicates that HS
# raises test scores by 7.6 points, or one-third of a SD, on average.

 # # # # # #
# Problem 6 # 
 # # # # # #

# In the fixed effect regression, we can include child-level covariates 
# only. We cannot control for any family-level variables that do not
# vary between siblings. I choose male, firstborn, lninc_0to3, 
# dadhome_0to3, and lnbw as covariates. I do not use ppvt_3 because
# it is available for few observations. When I include it, the sample 
# shrinks and changes composition a lot. This was a judgment call, and
# you could have done it differently. as researchers we often face 
# tradeoffs between having more information (by controlling for PPVT) 
# and maintaining the composition of the sample (by not controlling for PPVT).
feols(comp_score_5to6 ~ head_start + male + firstborn + lninc_0to3 +
                        dadhome_0to3 + lnbw | mom_id,
      data = nlsy_kids)

# The estimate is still positive and statistically significant, but it
# is slightly smaller, in magnitude: HS participation raises test scores
# by 5.6 points on average. It is useful to check whether this is due to
# omitted variable bias or the different composition of the subsample 
# with non-missing covariates. I re-estimate the model with no pre-HS
# covariates, but this time using the sub-sample with non-missing covariates.
# This was not necessary for full credit, but it is good practice.
nlsy_kids_subsample <-
  nlsy_kids %>%
  drop_na(male, firstborn, lninc_0to3, dadhome_0to3, lnbw)

feols(comp_score_5to6 ~ head_start | mom_id,
      data = nlsy_kids_subsample)

# The coefficient on HS is much closer to the regression with pre-HS 
# covariates. This suggest that within-family OVB is *NOT* the issue, but
# rather that individuals with missing data on covariates have larger effects.
# The estimates are robust to controlling for pre-HS covariates

 # # # # # #
# Problem 7 # 
 # # # # # #

# Standardize outcome variables by subtracting mean and dividing by SD. 
# The scale() function in R does this in one step:
nlsy_kids <- 
  nlsy_kids %>%
  mutate(std_5to6 = scale(comp_score_5to6),
         std_7to10 = scale(comp_score_7to10),
         std_11to14 = scale(comp_score_11to14))
# You were not expected to know this function. You could have also used:
nlsy_kids <- 
  nlsy_kids %>%
  mutate(stdb_5to6 = (comp_score_5to6 - mean(comp_score_5to6, na.rm = TRUE))/sd(comp_score_5to6, na.rm = TRUE),
         stdb_5to6 = (comp_score_7to10 - mean(comp_score_7to10, na.rm = TRUE))/sd(comp_score_7to10, na.rm = TRUE),
         stdb_5to6 = (comp_score_11to14 - mean(comp_score_11to14, na.rm = TRUE))/sd(comp_score_11to14, na.rm = TRUE))

# Now we run a FE regression of each standardized score on HS participation,  
# finding thatthe estimated effects shrink as children get older. HS raises 
# scores by 0.34 standard deviations on average at ages 5-6, by 0.16 standard
# deviations at ages 7-10, and by 0.15 standard deviations at ages 11 to 14.
feols(std_5to6 ~ head_start | mom_id,
      data = nlsy_kids)
feols(std_7to10 ~ head_start | mom_id,
      data = nlsy_kids)
feols(std_11to14 ~ head_start | mom_id,
      data = nlsy_kids)

# You may notice that the sample changes across regressions due to missingness.
# You could have also held the sample constant, as we did above for adding
# covariates. The effect on the test score at age 5-6 is still largest.
nlsy_kids_subsample <-
  nlsy_kids %>%
  drop_na(std_5to6, std_7to10, std_11to14)

feols(std_5to6 ~ head_start | mom_id,
      data = nlsy_kids_subsample)
feols(std_7to10 ~ head_start | mom_id,
      data = nlsy_kids_subsample)
feols(std_11to14 ~ head_start | mom_id,
      data = nlsy_kids_subsample)

 # # # # # #
# Problem 7 # 
 # # # # # #

# We run FE regressions for longer-term outcomes. We find that HS participation
# reduces grade repetition by 5 percentage points, reduces learning disability 
# diagnosis by 4 percentage points, raises high school graduation by 13 percentage
# points, raises college attendance by 7 percentage points, reduces idleness 
# (not working or studying) by 7 percentage points, and reduces fair/poor health
# by 7 percentage points. All of these results but one (for grade repetition)
# are significant at the 5 percent level. The grade repetition result is significant 
# at the 9 percent level.
feols(repgrade ~ head_start | mom_id,
      data = nlsy_kids)
feols(learndis ~ head_start | mom_id,
      data = nlsy_kids)
feols(hsgrad ~ head_start | mom_id,
       data = nlsy_kids)
feols(somecoll ~ head_start | mom_id,
       data = nlsy_kids)
feols(idle ~ head_start | mom_id,
       data = nlsy_kids)
feols(fphealth ~ head_start | mom_id,
       data = nlsy_kids)

 # # # # # #
# Problem 8 # 
 # # # # # #

# The easiest way to test for heterogeneous effects by race, ethnicity, and sex
# is include interactions of the HS dummy with race, ethnicity, and sex dummies.
# We also need to control for the main effect of sex, but not for the main effects
# or race and ethnicity because they are collinear with the mother fixed effects.
# I do this below for the high school graduation outcome. The results do not 
# show strong evidence of heterogeneity in effects by race, ethnicity, or sex. 
# The coefficients on the interaction terms are large, but none are significant 
# at the 5% level.

# Here I use R's nice approach to interaction terms, but you could have also 
# directly generated new variables for the interaction terms.

feols(hsgrad ~ head_start*(hispanic + black + male) | mom_id,
      data = nlsy_kids)

 # # # # # #
# Problem 9 # 
 # # # # # #

# The evidence suggests that HS participation has lasting effects on children's
# outcomes, which provides some justification for the program's existence. Whether
# the government chould expand or cut funding for this and similar programs depends
# on its cost-effectiveness compared with other potential use of funds. In general,
# it is difficult to extrapolate the effects of program expansion from our estimated 
# average effects of treatment on the treated because the effects may be different
# in the new subpopulations that would gain access if the program expanded. At the
# same time, the lack of significant treatment effect heterogeneity in Problem 9 
# suggests that perhaps we can extrapolate. Many answers could receive full credit 
# for this question.

