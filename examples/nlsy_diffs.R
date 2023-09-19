# uncomment if these packages are not installed
# install.packages(c('tidyverse','fixest'))

# this script estimates racial differences in earnings using data from the
# National Longitudinal Survey of Youth '79

# clear environment and load packages
rm(list=ls())
library(tidyverse)
library(fixest)

# load nlsy79.Rdata
load(url("https://github.com/tvogl/econ121/raw/main/data/nlsy79.Rdata"))

# data structure
glimpse(nlsy79)

# mean and sd of labor earnings (na.rm=TRUE removes missing values from the calculation)
mean(nlsy79$laborinc18,na.rm=TRUE)
sd(nlsy79$laborinc18,na.rm=TRUE)

# more detailed summary with percentiles
summary(nlsy79$laborinc18)

# we can see more detail when we plot histograms by race
nlsy79 %>% 
  ggplot(aes(x = laborinc18)) +
    geom_histogram() +
    facet_wrap(~black, ncol=1) # separate graphs by race, stacked into one column

# we will estimate differences in mean income between blacks and non-blacks.

# means by race
nlsy79 %>% 
  drop_na(laborinc18) %>% # removes NA values so we don't need to use na.rm below
  group_by(black) %>% 
  summarize(mean=mean(laborinc18),
            sd=sd(laborinc18),
            n=n())

# alternatively could have written
nlsy79 %>% 
  group_by(black) %>% 
  summarize(mean=mean(laborinc18, na.rm = TRUE),
            sd=sd(laborinc18, na.rm = TRUE),
            n=sum(!is.na(laborinc18)))

# these results give us all the information we need to test for differences by race.
# difference
50798-31505
# t-statistic
(50798-31505)/sqrt(70856^2/4558 + 46907^2/2013)
# well above 1.96, so statistically significant by the usual standards.

# alternative ways to run this test are...
# t-test with unequal variances:
t.test(laborinc18 ~ black, data = nlsy79)
# regression with heteroskedasticity-robust SEs, using feols() from fixest package
feols(laborinc18 ~ black, data = nlsy79, vcov = 'hetero')
# note that lm() is the base-R way to estimate a regression, but it doesn't 
# directly allow for robust standard errors, and you need to use summary()
# to even see classical standard errors. feols() from fixest is more convenient.
model1 <- lm(laborinc18 ~ black, data = nlsy79)
model1
summary(model1)

# it is actually uncommon to test for average differences in the level 
# (rather than log) of earnings, including zeros from the non-employed. 
# it would be much more typical to restrict to employed individuals. so 
# let's restrict to people restrict to people who worked for pay for at least 
# 1000 hours: equivalent to a part-time job of 20 hours per week for 50 weeks.
summary(nlsy79$hours18)

nlsy79_workers <- 
  nlsy79 %>% 
  filter(hours18>=1000 & laborinc18>0)

summary(nlsy79_workers$hours18)

# means by race
nlsy79_workers %>% 
  drop_na(laborinc18) %>%
  group_by(black) %>% 
  summarize(mean=mean(laborinc18),
            sd=sd(laborinc18),
            n=n())

# still an $19k difference

# now let's look at log earnings.
nlsy79_workers <- 
  nlsy79_workers %>% 
    mutate(loginc18 = log(laborinc18))

nlsy79_workers %>% 
  drop_na(loginc18) %>%
  group_by(black) %>% 
  summarize(mean=format(mean(loginc18, na.rm = TRUE)),   # the format() function is just to report more decimal places
            sd=sd(loginc18, na.rm = TRUE),
            n=n())
# difference:
10.851-10.616

# this difference in logs can by roughly interpreted as 
# a 23.5% gap in earnings, although this interpretation
# relies on calculus [dln(y)/dx], since we are doing a 
# comparison by a discrete variable, we can think of 
# 23.5% as an approximation 

# the t-statistic:
(10.851-10.616)/sqrt(.867^2/3015 + .849^2/1078)
# well above 1.96, so statistically significant by the usual standards.

# alternative ways to run this test are...
# t-test with unequal variances:
t.test(loginc18 ~ black, data = nlsy79_workers)
# regression with heteroskedasticity-robust standard errors:
feols(loginc18 ~ black, data = nlsy79_workers, vcov = 'hetero')
# same results. that is to say, a regression on a "dummy variable"
# for black leads to the same results as a difference of means
# note that the t-statistic is very slightly different from what
# we computed "by hand." that's likely due to rounding errors.
