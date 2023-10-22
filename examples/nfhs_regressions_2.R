# uncomment if these packages are not installed
# install.packages(c('tidyverse','fixest','car'))

# this script continues analyzing the relationship between age and  
# asset ownership in India, using the National Family Health Survey

# clear environment and load packages
rm(list=ls())
library(tidyverse)
library(fixest)
library(car) # for deltaMethod()

# load nlsy79.Rdata
load(url("https://github.com/tvogl/econ121/raw/main/data/nfhs4.Rdata"))

# filter data to include only 20-80 year old household heads
nfhs4 <- filter(nfhs4, age >= 20 & age <= 80)

# we left off with estimates of a single slope, # as a summary of a population 
# pattern. recall that we used sampling weights, to ensure that the estimates
# are representative of the population slope.
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight)

# heterogeneity between urban and rural areas using subset
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight, subset = ~rural==0)
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight, subset = ~rural==1)

# heterogeneity between urban and rural areas using split (equivalent to above)
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight, split = ~rural)

# the urban/rural subsamples are independent, so we can compute
# a t-statistic for the difference in slopes "by hand"
(0.001958-0.002472)/sqrt(0.000051^2+0.000089^2)

# alternatively, we can use an interaction term.

#first generate the interaction term
nfhs4 <- mutate(nfhs4, ageXrural = age*rural)
# could have also written: nfhs4$ageXrural <- nfhs4$age*nfhs4$rural

# now run the model and save the results as model1 for later
model1 <- feols(assets ~ age + rural + ageXrural, data = nfhs4, vcov = ~clustnum, weights = ~weight)
model1
# same t-statistic, subject to a bit of rounding error in our calculations. 
# assets rise less steeply with age in rural areas.

# we could also do this more elegantly using R's functionality for
# interaction terms, substituting "age * rural" for "age + rural + ageXrural".
# but this syntax doesn't play as nicely with the deltaMethod() function we use 
# below. just to show you how it's done:
feols(assets ~ age * rural, data = nfhs4, vcov = ~clustnum, weights = ~weight)

# if we wanted to obtain the slope for rural areas from the interacted model,
# we would take a linear combination of the coefficient on age plus coefficient 
# on the ruralXage interaction term. the standard way to do this would be the
# glht() function from the multcomp package. to reduce the number of packages
# and functions we use, we'll just rely on the deltaMethod() function from the 
# car package. recall that when the delta method is applied to a linear combination,
# it is exactly the same as the conventional approach to inference on linear combinations. 
deltaMethod(model1,"age + ageXrural")
# same as we got when we estimated a separate regression for rural areas!
# if we want the deltaMethod() function to test the hypothesis that the rural
# slope equals zero, we add:
deltaMethod(model1,"age + ageXrural", rhs = 0)
# which adds the three rightmost columns on the hypothesis test

# note: in this sample, we could not compute the t-statistic 
# by hand if we were interested in male/female differences in
# slopes. the primary sampling units are clusters, so the male
# and female subsamples are not independent. our only option
# here is to use an interaction term. here we will take advantage
# of R's elegant functionality for interaction terms:
feols(assets ~ age * male, data = nfhs4, vcov = ~clustnum, weights = ~weight)
# no significant difference in slopes between men and women!

# now let's suppose we are interested in the ratio of the urban
# slope to the rural slope. we use the delta method.
deltaMethod(model1,"age / (age + ageXrural)")
# this is nice for interpretation. it's hard to interpret the 
# difference of 0.05 percentile points between the urban and rural slopes. 
# but now we can say that the slope in urban areas is 26% higher than
# the slope in rural areas, and that proportional difference is
# statistically significant (because the 95% CI for the ratio
# excludes 1). we can add a formal test of the hypothesis by adding:
deltaMethod(model1,"age / (age + ageXrural)", rhs=1)

# Let's get some insight into why the slope is steeper in urban areas.
# First, let's draw the scatter plots by age, just like last time, but
# now separating the sample by urban/rural.
nfhs4 %>% 
  group_by(age, rural) %>%
  summarize(mean = weighted.mean(assets,w=weight)) %>%
  ggplot(aes(x=age, y=mean, color=rural)) +
    geom_point()
# This is not important, but you might notice that the color scale for
# rural is continuous. That's because R can't tell it's a categorical
# variable. To change it to a categorical (aka factor) variable, we can use 
# the factor() function as follows.
nfhs4 %>% 
  group_by(age, rural) %>%
  summarize(mean = weighted.mean(assets,w=weight)) %>%
  ggplot(aes(x=age, y=mean, color=factor(rural))) +
    geom_point()

# The nonlinear functions have similar shapes! So the different linear
# slopes must be coming from the distribution of age. Let's check that.
# We will draw kernel density plots for urban and rural areas. We will
# learn more about density estimation later in the course, but you
# can think of it as a flexible histogram. We use geom_density()
# from ggplot. 'bw' (bandwidth) sets how smooth the density estimator 
# is, and 'alpha' sets the darkness of the area under the curve.
ggplot(nfhs4, aes(x=age, weight = weight, fill=factor(rural))) +
  geom_density(bw = 2, alpha = 0.4)
# Here, you can see that rural areas have more mass at older ages, 
# where the regression function is flatter.
