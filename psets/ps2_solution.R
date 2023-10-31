# This R script presents solutions to ECON 121 Problem Set 2

# clear environment
rm(list = ls())
# load packages
library(tidyverse) # for data manipulation and summary statistics
library(fixest) # for regression
library(car) # for linear and non-linear combinations of coefficients

#############
##Problem 2##
#############
  
# If education and experience are exogenous, then beta1 represents
# the causal effect of education on log wages. The quantitative
# interpretation is that each additional year of education raises
# wages by 100*beta1 percent.

# The squared term in experience allows for wages to vary non-
# linearly with experience. For instance, we might expect wages
# to rise with experience at a decreasing rate. In this case, 
# beta3 would be positive and beta4 would be negative.

#############
##Problem 3##
#############

# We open the CPS dataset, then describe and summarize the data
load(url("https://github.com/tvogl/econ121/raw/main/data/cps18.Rdata"))

summary(cps18)

# Drop observations with <50 weeks or <35 hours or missing hours 
# or 0 income (which will drop when we take logs anyway) 
cps18 <- cps18 %>% filter(hrs_per_wk >= 35 & hrs_per_wk <= 170 & incwage>0 & wkswork>=50)

# Generate new variables
cps18 <- cps18 %>% mutate(lnw = log(incwage/(wkswork*hrs_per_wk)),
                          black = ifelse(race == "black", 1, 0),
                          asian = ifelse(race == "asian/pacific", 1, 0),
                          native = ifelse(race == "native", 1, 0),
                          other = ifelse(race == "multiple/other", 1, 0),
                          edyrs = case_when((ed_lt_hs==1) ~ 6,
                                            (ed_some_hs==1) ~ 10,
                                            (ed_hs_degree==1) ~ 12,
                                            (ed_some_col==1) ~ 14,
                                            (ed_ba_degree==1) ~ 16,
                                            (ed_post_degree==1) ~ 19),
                          exper = age - edyrs - 5,
                          exper2 = exper^2)


# Summarize the new variables. The mean log wage is 3.13, or 
# approximately $23/hour. Education averages 14.5 years, with a 
# standard deviation of 2.8. Experience averages 24 years, with
# a standard deviation of 11. 12 percent of the sample is black.
cps18 %>%
  summarise(mean_lnw = mean(lnw,na.rm=TRUE),
            mean_edyrs = mean(edyrs,na.rm=TRUE),
            mean_exper = mean(exper,na.rm=TRUE),
            mean_black = mean(black,na.rm=TRUE),
            mean_asian = mean(asian,na.rm=TRUE),
            mean_native = mean(native,na.rm=TRUE),
            mean_other = mean(other,na.rm=TRUE),
            mean_male = mean(male,na.rm=TRUE)
  )

cps18 %>%
  summarise(sd_lnw = sd(lnw,na.rm=TRUE),
            sd_edyrs = sd(edyrs,na.rm=TRUE),
            sd_exper = sd(exper,na.rm=TRUE),
            sd_black = sd(black,na.rm=TRUE),
            sd_asian = sd(asian,na.rm=TRUE),
            sd_native = sd(native,na.rm=TRUE),
            sd_other = sd(other,na.rm=TRUE),
            sd_male = sd(male,na.rm=TRUE)
  )

#############
##Problem 4##
#############
  
# Estimate the Mincerian regression. The estimated return is
# 0.110, or an 11 percent wage increase per year of education.
feols(lnw ~ edyrs + exper + exper2, data = cps18, vcov = 'hetero')

#############
##Problem 5##
#############

# The extended Mincerian regression yields an estimated 
# return of 0.114, which is similar but slightly larger
# than the original estimate.
feols(lnw ~ edyrs + exper + exper2 + male + black + asian + native + other,data = cps18, vcov = 'hetero')

#############
##Problem 6##
#############

# The female-male wage gap is 0.274 log points, while the
# black white gap is -0.164 log points, leading to a 
# difference of 0.111, which is significant at less than the 0.1% level.
# Note that I wrote "black + male" because the coefficient on "male" is
# the male - female wage gap, and I want the female-male wage gap. I 
# included "rhs=0" to tell R to test the hypothesis that "black+male=0".
extended <- feols(lnw ~ edyrs + exper + exper2 + male + black + 
                  asian + native + other,data = cps18, vcov = 'hetero')
deltaMethod(extended,"black + male",rhs=0)

#############
##Problem 7##
#############

# If we run separate regressions for men and women, we
# find a return of 0.110 for men and 0.120 for women, 
# implying a difference in returns of 0.010 log points
# or 1.0 percent.
feols(lnw ~ edyrs + exper + exper2 + black + asian + native + other,
      data = cps18, 
      vcov = 'hetero',
      split = ~male)
# Alternatively, we could have run:
feols(lnw ~ edyrs + exper + exper2 + black + asian + native + other,
      data = cps18, 
      vcov = 'hetero',
      subset = ~male==0)
feols(lnw ~ edyrs + exper + exper2 + black + asian + native + other,
      data = cps18, 
      vcov = 'hetero',
      subset = ~male==1)

# To assess whether the difference is significant, we
# can compute the t-statistic using the coefficients and
# standard errors. That's because the male and female samples
# are independent, so the coefficients have no covariance.
# The t-statistic is 4.45, so the difference in coefficients
# is significant at the 5% level.
(0.120032-0.109961)/sqrt(0.001692^2+0.001500^2)

#############
##Problem 8##
#############

# To match the approach in Problem 6, we need to allow
# ALL of the coefficients to vary by gender, so we need
# many interaction terms. The coefficient on the interaction
# term between education and female is -0.010, with a 
# t-statistic of 4.45, just as in Problem 7!
cps18 <- cps18 %>% mutate(edyrs_m = edyrs*male,
                          exper_m = exper*male,
                          exper2_m = exper2*male,
                          black_m = black*male,
                          asian_m = asian*male,
                          native_m = native*male,
                          other_m = other*male)
interacted <- feols(lnw ~ edyrs + edyrs_m + exper + exper_m + exper2 + exper2_m +
                    black + black_m + asian + asian_m + native + native_m + 
                    other + other_m + male,
                    data = cps18,
                    vcov = 'hetero')
summary(interacted)

# Using the delta method to estimate the ratio of returns, we
# find a female/male ratio of 1.09. The 95% confidence interval
# starts at 1.05, so we can reject the null hypothesis that 
# the ratio is 1 at the 5% level.
deltaMethod(interacted, "edyrs/(edyrs+edyrs_m)", rhs=1)

#############
##Problem 9##
#############

# Open the NLSY dataset
load(url("https://github.com/tvogl/econ121/raw/main/data/nlsy79.Rdata"))

# The sample is 25% black and 14% Hispanic, but when we use 
# sampling weights, those shares fall to 14% black and 6% 
# Hispanic. Because the sampling weights undo the NLSY's over-
# sampling, the latter estimates are representatives of US
# adults who were teenagers residing in the United States
# in 1979. The weighted statistics provide unbiased estimates
# of the population racial composition, since they restore
# representativeness in the sample.
nlsy79 %>% summarise(mean_black = mean(black),
                     wtmean_black = weighted.mean(black, w=perweight),
                     mean_hisp = mean(hisp),
                     wtmean_hisp = weighted.mean(hisp, w=perweight))

##############
##Problem 10##
##############

# Keep full time workers with positive, non-missing earnings, 
# and generate new variables
nlsy79 <- 
  nlsy79 %>% 
  filter(hours18 >= 35*50 & laborinc18>0) %>%
  mutate(lnw = log(laborinc18/hours18),
         exper = age79 + 2018-1979 - educ - 5,
         exper2 = exper^2)

# Estimates of the Mincerian return to education are extremely 
# similar using OLS (0.121) and WLS (0.121). Since the 
# unweighted OLS estimates are more precise (consistent 
# with the Guass Markov theorem), I will continue the 
# analysis with unweighted regressions. (I could have
# also said that I prefer to have results that are 
# representative of the coefficient I would obtain in
# the full population, which would have led me to run
# weighted regressions for the rest of the analysis.)
feols(lnw ~ educ + exper + exper2 + black + hisp + male, data = nlsy79, vcov='hetero')
feols(lnw ~ educ + exper + exper2 + black + hisp + male, data = nlsy79, vcov='hetero', weights = ~perweight)

##############
##Problem 11##
##############

# The extended Mincerian regression yields an estimated 
# return to education of 12 percent, similar to the CPS.
# However, while the CPS had a significant positive coef-
# ficient on exper and a significant negative coefficient
# on exper2, these coefficients have the opposite sign
# in the NLSY. The difference is likely because NLSY respondents are 
# quite old in 2018, with substantial potential experience.
# The CPS indicated that wages rise with experience at labor
# market entry but then flatten out (due to the negative 
# squared term). Since the NLSY in 2007 only had mature 
# workers, the dataset is not well-suited for estimating
# the returns to experience. Workers may already be retiring
# too.

##############
##Problem 12##
##############

# It seems unlikely that the coefficient on education 
# represents the causal effect of education. Education
# and wages are likely to be correlated with a number of
# omitted variables, such as innate ability and parental
# socioeconomic status.

##############
##Problem 13##
##############

# To address the concerns above, we can control for 
# childhood background characteristics and cognitive
# test scores. Doing so reduces the estimated return to 
# education substantially, to 6 percent.
feols(lnw ~ educ + exper + exper2 + black + hisp + male +
            foreign + urban14 + mag14 + news14 + lib14 +
            educ_mom + educ_dad + numsibs + afqt81, 
      data = nlsy79,
      vcov = 'hetero')

# However, note that the sample size changed because
# we do not have all control variables for all 
# observations. We should reestimate the "short" model
# in the smaller sample to make sure the change in coefficients 
# is not due to sample composition. (This was not essential
# for full credit, but it is good practice.)
nlsy79_subsample <- nlsy79 %>% drop_na(c("foreign","urban14","mag14","news14","lib14",
                                         "educ_mom","educ_dad","numsibs","afqt81"))
feols(lnw ~ educ + exper + exper2 + black + hisp + male,
      data = nlsy79_subsample,
      vcov = 'hetero')
# Why did the return fall when we included additional
# covariates? It appears that urban residence, paternal
# education and AFQT scores (a measure of innate ability)
# are all positively related with wages, and it is likely
# that they also predict higher education. (You can check this.)





