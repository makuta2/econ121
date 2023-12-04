# this script studies the relationship or gender and age with income in 
# the united states, using the CPS. it relies on nonparametric estimators.

# load packages
library(tidyverse)
library(KernSmooth)

# load dataset
load(url("https://github.com/tvogl/econ121/raw/main/data/cps18.Rdata"))

summary(cps18)

# from the summary() results above, we know incwage has many 0s.
# over 25% of sample has 0s! restrict to workers who work 40+ hrs/wk 
# with positive income. since we are just analyzing black/white diffs
# drop all other racial categories.
cps18 <- 
  cps18 %>% 
  filter(hrs_per_wk >=40 & incwage>0 & (race=="white" | race == "black"))

# generate log income
cps18$lninc <- log(cps18$incwage)

# what is the sd of lninc?
sd(cps18$lninc)

# kernel density estimates of log income by race and sex
# let's use .08 as the bandwidth, about one tenth of the sd of lninc.
ggplot(data = cps18, aes(x = lninc, group = male, color = factor(male))) +
  geom_density(bw = .08)

# you can also fill in the area under the curve
ggplot(data = cps18, aes(x = lninc, group = male, fill = factor(male))) +
  geom_density(bw = .08, alpha = .5) # alpha controls the transparency of the fill

# now let's try a bandwidth of .04 -> higher variance but more detail
ggplot(data = cps18, aes(x = lninc, group = male, fill = factor(male))) +
  geom_density(bw = .04, alpha = .5)

# for local linear regression, our syntax will be less elegant,
# since ggplot() is very clunky tool for regression smoothing.
# we will use locpoly() rom the KernSmooth package. we will need 
# to estimate local linear regressions first and then plot them
# using ggplot.

# for men and then women, estimate log wages over the lifecycle
# using local linear regression. set the degree to 1 because we are
# estimating a local LINEAR regression (not quadratic, cubic, etc.).
# and start with a bandwidth of 1 year of age.  

# men
# subset data
cps18_men <- cps18 %>% filter(male==1) %>% select(age, lninc)
# run local linear regression -- but output is two lists
fit <- locpoly(x = cps18_men$age, y = cps18_men$lninc, bandwidth = 1, degree = 1)
fit
# turn regression output into a data frame
fit_men <- data.frame(fit)

# women
cps18_women <- cps18 %>% filter(male==0) %>% select(age, lninc)
fit_women <- data.frame(locpoly(x = cps18_women$age, y = cps18_women$lninc, bandwidth = 1, degree = 1))

# plot estimates
ggplot() +
  geom_line(data = fit_men, aes(x = x, y = y, color = "men")) +
  geom_line(data = fit_women, aes(x = x, y = y, color = "women")) 
                               
# what happens if we expand the bandwidth to 2 years? smoother curves.
fit_men <- data.frame(locpoly(x = cps18_men$age, y = cps18_men$lninc, bandwidth = 2, degree = 1))
fit_women <- data.frame(locpoly(x = cps18_women$age, y = cps18_women$lninc, bandwidth = 2, degree = 1))
ggplot() +
  geom_line(data = fit_men, aes(x = x, y = y, color = "men")) +
  geom_line(data = fit_women, aes(x = x, y = y, color = "women")) 