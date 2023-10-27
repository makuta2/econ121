# This example studies the relationship between low birth weight and test scores

# uncomment to install plm package, which contains panel data models
# install.packages("plm")

# Import dataset
library(haven)
nlsy <- read_dta("https://github.com/tvogl/econ121/raw/main/data/nlsy_deming.dta")

# Decribe the data. Test scores go from 0 to 100, with a mean in the 40s.
summary(nlsy)

# Birth weight is in logs, which is a little complicated to interpret
# Let's convert to ounces
nlsy$bw <- exp(nlsy$lnbw)

# Have a look at the summary statistics
summary(nlsy$bw)

# Let's give ourselves a sense of how birth weight relates to the
# composite test score by plotting mean test scores by birthweight.
# I create a binned version of birthweight to make the plot less noisy.
library(dplyr) # for if_else() and group_by()
nlsy$bw_bin <- floor(nlsy$bw/10)*10               # now 100 means "100-109.9" and so forth
nlsy$bw_bin <- 70 * ifelse(nlsy$bw_bin<70,1,0) +  # compress the long tails into big bins 
               nlsy$bw_bin * ifelse(nlsy$bw_bin>=70 & nlsy$bw_bin<140,1,0) +
               140 * ifelse(nlsy$bw_bin>=140,1,0)
nlsy_bins1 <- nlsy %>% 
                group_by(bw_bin) %>% 
                summarise(mean_test = mean(comp_score_11to14,na.rm = TRUE))
library(ggplot2) # for plot
ggplot(nlsy_bins1,aes(x=bw_bin,y=mean_test)) +
  geom_line() +
  geom_point()

# There is a strong relationship! But how much of this is 
# due to family characteristics? Let's generate separate
# plots for mothers with <12, 12, and >12 years of schooling.
nlsy$momedlevel <- case_when(nlsy$momed<12 ~ "<HS",
                             nlsy$momed==12 ~ "HS",
                             nlsy$momed>12 ~ ">HS")
nlsy_bins2 <- nlsy %>% 
                filter(!is.na(momedlevel)) %>%  # remove obs with missing maternal education
                group_by(bw_bin, momedlevel) %>% 
                summarise(mean_test = mean(comp_score_11to14,na.rm = TRUE))
ggplot(nlsy_bins2,aes(x=bw_bin,y=mean_test,color=momedlevel)) +
  geom_line() +
  geom_point()

# Maternal education is clearly associated with test scores. At the same time,
# these plots don't look that much flatter than the full sample plot above.
# How much of the relationship is attributable to maternal characteristics
# rather than child health per se? We need to use fixed effects to find out.

# For simplicity, let's generate a very low birth weight indicator, 
# based on the 53 ounce threshold.
nlsy$vlow_bw <- ifelse(nlsy$bw<53,1,0)

# For this binary categorization, a bar graph may be a convenient way
# to visualize the data.
ggplot(data=nlsy, mapping=aes(x=vlow_bw, y=comp_score_11to14)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

# Let's look at the structure of the panel data for a few key variables
nlsy <- nlsy[order(mom_id),] # sort by mom_id so siblings are next to each other
glimpse(nlsy)

# OLS with robust standard errors
library(estimatr) # for lm_robust()
lm_robust(comp_score_11to14 ~ vlow_bw,data = nlsy)

# OLS with clustered standard errors
lm_robust(comp_score_11to14 ~ vlow_bw,data = nlsy,clusters = mom_id)

# Random effects
# The estimated coefficient changes a lot,
# which suggest that the between-family variation
# and the within-family variation lead to different
# coefficients. Most researchers would conclude 
# that we should rely on fixed effects.
library(plm) # for panel data models
re <- plm(comp_score_11to14 ~ vlow_bw, data = nlsy, index = c("mom_id"), model = "random")
summary(re)

# Random effects with cluster robust standard errors
# We need to pass our estimates through the coeftest() function from the lmtest package
library(lmtest)
coeftest(re, vcov=vcovHC(re, cluster="group"))

# Fixed effects
# Here, the estimated coefficient shrinks even more, 
# consistent with upward bias from between-family variation.
fe <- plm(comp_score_11to14 ~ vlow_bw, data = nlsy, index = c("mom_id"), model = "within")
summary(fe)

# Fixed effects with cluster-robust standard errors
coeftest(fe, vcov=vcovHC(fe, cluster="group"))
# Note that the coefficient estimates are all the same.
# But the SEs are all different!

# Let's try adding some control variables. We will add black,
# hispanic, and momed as examples of control variables that 
# DO NOT vary within family. We will add male and first born
# as examples of covariates that DO vary within family. Since
# adding control variables changes the sample size, we will 
# rerun the models without control variables in the smaller
# sample.

# OLS with and without control variables. Consistent with the FE vs OLS
# comparison, adding the family-level control variables reduces the estimates.
# Adding just the individual-level control variables doesn't do much.
nlsy_subset <- na.omit(nlsy[c("comp_score_11to14","vlow_bw","hispanic",
                               "black","momed","male","firstborn")])
lm_robust(comp_score_11to14 ~ vlow_bw + hispanic + black +  momed + male + firstborn, data = nlsy_subset)
lm_robust(comp_score_11to14 ~ vlow_bw + male + firstborn, data = nlsy_subset)
lm_robust(comp_score_11to14 ~ vlow_bw, data = nlsy_subset)

# FE with and without control variables. The family-level control
# variables are dropped because they are collinear with the mother
# fixed effects. Again, the individual-level control variables
# don't much change the estimates on vlow_bw
fe3 <- plm(comp_score_11to14 ~ vlow_bw + hispanic + black +  momed + male + firstborn, data = nlsy, index = c("mom_id"), model = "within")
coeftest(fe3, vcov=vcovHC(fe, cluster="group"))
fe2 <- plm(comp_score_11to14 ~ vlow_bw + male + firstborn, data = nlsy, index = c("mom_id"), model = "within")
coeftest(fe2, vcov=vcovHC(fe, cluster="group"))
fe1 <- plm(comp_score_11to14 ~ vlow_bw, data = nlsy, index = c("mom_id"), model = "within")
coeftest(fe1, vcov=vcovHC(fe, cluster="group"))
