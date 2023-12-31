# this R script analyzes data from romanian schools. the data include
# students who live in towns with two high schools. high school admissions
# are based on an entry exam. we will use the admissions cutoff to
# the town's better high school in a fuzzy regression discontinuity design.

rm(list=ls())
library(tidyverse)
library(fixest)

load(url("https://github.com/tvogl/econ121/raw/main/data/romania_schools.Rdata"))

# variables in the dataset
ls(romania_schools)
# distance = score - cutoff
# better = went to better school in town
# ptile = percentile on high school exit exam

# summarize distance to the town-specific cutoff
summary(romania_schools$distance)

# graph of first stage: effect of being above cutoff on attending better school
# plot binned means for 0.05-wide bins
romania_schools %>%
  mutate(bin = floor(distance*20)/20+0.025) %>%
  group_by(bin) %>%
  summarize(better_mean = mean(better)) %>%
  ggplot(aes(x = bin, y = better_mean)) + 
    geom_point() +
    labs(x="distance", y="pr[better school]")
# HUGE discontinuity in treatment, aboue 60 percentage points

# graph of reduced form relationship --> effect of being above cutoff on exam
# plot binned means as above
romania_schools %>%
  mutate(bin = floor(distance*20)/20+0.025) %>%
  group_by(bin) %>%
  summarize(ptile_mean = mean(ptile)) %>%
  ggplot(aes(x = bin, y = ptile_mean)) + 
  geom_point() +
  labs(x="distance", y="exit exam percentile")
# noticeable discontinuity in outcome, hard to tell how large

# maybe zoom in...
romania_schools %>%
  mutate(bin = floor(distance*20)/20+0.025) %>%
  group_by(bin) %>%
  summarize(ptile_mean = mean(ptile)) %>%
  filter(bin>=-1 & bin<=1) %>%
  ggplot(aes(x = bin, y = ptile_mean)) + 
  geom_point() +
  labs(x="distance", y="exit exam percentile")
# discontinuity in outcome looks like about 7 percentile points

# regression analysis

# first generate dummy for being above cutoff, plus
# interaction of dummy with distance
romania_schools <- 
  romania_schools %>%
  mutate(above = ifelse(distance>=0,1,0),
         aboveXdistance = above*distance)

# first stage local linear regression with rectangular kernel and bw = 0.2
feols(better ~ above + distance + aboveXdistance, 
      data = romania_schools, 
      vcov = 'hetero',
      subset = ~(abs(distance)<=0.2))

# reduced form local linear regression with rectangular kernel and bw = 0.2
feols(ptile ~ above + distance + aboveXdistance, 
      data = romania_schools, 
      vcov = 'hetero',
      subset = ~(abs(distance)<=0.2))

# ratio of reduced form to first stage --> effect of going to the town's better school on exit exam percentile
3.95468/.6369932

# estimate by two stage least squares
feols(ptile ~ distance + aboveXdistance | better ~ above, 
      data = romania_schools, 
      vcov = 'hetero',
      subset = ~(abs(distance)<=0.2))

# we conclude that going to the better school raises exit 
# exam performance by 6 percentiles on average. the effect 
# is statistically significant at teh 5 percent level.