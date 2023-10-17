# uncomment if these packages are not installed
# install.packages(c('tidyverse','fixest'))

# this script estimates the relationship between age and asset 
# ownership in India, using the National Family Health Survey

# clear environment and load packages
rm(list=ls())
library(tidyverse)
library(fixest)

# load nlsy79.Rdata
load(url("https://github.com/tvogl/econ121/raw/main/data/nfhs4.Rdata"))

# let's have a look at the distribution of age. note the "age heaping."
# we set binwidth to 1 so that each value gets its own bar.
ggplot(nfhs4, aes(x=age)) +
  geom_histogram(binwidth=1)

# filter data to include only 20-80 year old household heads
nfhs4 <- filter(nfhs4, age >= 20 & age <= 80)

# note that the survey has sampling weights. these are to
# adjust for the fact that the sample was not a simple
# random sample. subpopulations that were undersampled receive
# larger sampling weights, so that weighted statistics provide
# unbiased estimates of population parameters. if p is the
# probability that a household was included in the sample,
# then that household's weight is proportional to 1/p. the
# actual number is (1/p)*(sample size)*(1 million), but since
# all weighted statistics rescale by the sum of the weights,
# it is conceptually the same as weighting by 1/p. we will want
# to use sampling weights to estimate population parameters.
summary(nfhs4$weight)
# let's plot a histogram of the weight variable
ggplot(nfhs4, aes(x=weight)) +
  geom_histogram()


# let's aggregate the data to age bins and plot average assets by age.
# note that we want weighted statistics to make them representative of the population.
# we estimate weighted averages using the function weighted.mean().
# tidyverse does not have an analogous weighted.sd() function, 
# so let's create that function from scratch:
weighted_sd <- function(x, w) {
  weighted_mean <- sum(x * w) / sum(w)
  weighted_variance <- sum(w * (x - weighted_mean)^2) / sum(w)
  sqrt(weighted_variance)
}

nfhs4_by_age <- 
  nfhs4 %>% 
  group_by(age) %>%
  summarize(mean = weighted.mean(assets,w=weight),
            sd = weighted_sd(assets,w=weight),
            numobs = n())

# look at the table. we use print() to control how many rows R displays.
print(nfhs4_by_age, n=86)

# it is a lot of rows! let's plot the means by age.
# we see a rising but concave relationship.
ggplot(nfhs4_by_age, aes(x=age, y=mean, size=numobs)) + 
  geom_point()

# now let's plot the sd's by age.
# we see again a rising but concave relationship.
ggplot(nfhs4_by_age, aes(x=age, y=sd, size=numobs)) + 
  geom_point()

# now we are going to run a series of OLS regressions on the grouped data.
# since we have unequal group sizes, this may be a good scenario to weight
# by the number of observations in each group. however, that method works
# when the same model holds for everyone (i.e., beta is not heterogeneous)
# and the individual-level model is homoskedastic. we already know that
# the individual-level model is heteroskedastic, so we may not end up
# improving the precision of the estimator (i.e., shrinking the SEs).

# run a linear regression using grouped data, with and without weighting by N.
# we get different coefficients because of the model's linearity. not clear that  
# the weighted regression is preferred, since it is changing the coefficient
# rather than improving precision.
feols(mean ~ age, data = nfhs4_by_age, vcov = 'hetero')
feols(mean ~ age, data = nfhs4_by_age, vcov = 'hetero', weights = ~numobs)

# quadratic regression using grouped data, with and without weighting by N.
# we get very similar coefficients now, but smaller SEs in the unweighted model,
# not as predicted under correct functional form, homoskedasticity, and 
# constant coefficients.
feols(mean ~ age + I(age^2), data = nfhs4_by_age, vcov = 'hetero')
feols(mean ~ age + I(age^2), data = nfhs4_by_age, vcov = 'hetero', weights = ~numobs)

# back to the micro data
# estimate quadratic functional form, with and without weighting by 
# sampling weight. get slightly diffent coefficients, larger SEs.
# so the weighted estimate is NOT more precise. it is also not
# necessarily the average coefficient in the population. it IS
# an estimate of the answer we would get from a census of the population.
feols(assets ~ age + I(age^2), data = nfhs4, vcov = 'hetero')
feols(assets ~ age + I(age^2), data = nfhs4, vcov = 'hetero', weights = ~weight)

# we should actually estimate this taking the clustered sampling into
# account. villages were sampled, and then individuals within them.
# we need cluster-robust SEs. the SEs become larger, consistent with
# positive intra-cluster correlations.
feols(assets ~ age + I(age^2), data = nfhs4, vcov = ~clustnum, weights = ~weight)

# even though the relationship is non-linear, it may still be
# useful to use a single slope as a summary of a population pattern.
# here it probably makes sense to weight, since the single slope has no
# "structural" interpretation. it is just a description of a pattern in
# the population.
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight)