# ECON 121 Problem Set 1
# Solution R-script.
  
############
#Question 1#
############

# I had no group.

############
#Question 2#
############


# load packages
library(tidyverse)

# clear environment
rm(list = ls())

# load data
ssa <- read_csv("https://github.com/tvogl/econ121/raw/main/data/ssa_names.csv")

# summarize data
summary(ssa)

# The dataset spans 1940 to 2022. Name frequencies range from 5
# to nearly 100,000.

############
#Question 3#
############

# We can find the all-time most common boy and girl names as follows.
ssa %>% 
  group_by(name, sex) %>%
  summarise(total = sum(frequency)) %>%
  group_by(sex) %>%                  
  slice_max(total, n=1)

# The most popular names are Michael and Mary. 20 million girls
# were named Mary between 1940 and 2022, and 43 million boys
# were named Michael.

############
#Question 4#
############

# Trends in the number of unique names by sex
ssa %>% 
  group_by(year, sex) %>%
  summarise(numunique = n()) %>%
  ggplot(aes(x=year, y=numunique, color=sex)) +
    geom_line() +
    geom_point(size=1)

# Both curves rise gradually from 1940 through the 1960s,
# then rise rapidly until 2008, and then either fall rapidly
# (for girls) or plateau (for boys).

############
#Question 5#
############

# Trends in the number of unique names relative to the number of babies by sex
ssa %>% 
  group_by(year, sex) %>%
  summarise(numunique = n(),
            numbabies = sum(frequency)) %>%
  mutate(ratio = numunique/numbabies) %>%
  ggplot(aes(x=year, y=ratio, color=sex)) +
    geom_line() +
    geom_point(size=1)

# After a brief decline in the 1940s, the ratio rose consistently until
# the 2010s. The number of unique names increased faster than the 
# number of babies. This trend is consistent with decreasing conformity 
# over time.

############
#Question 6#
############

# I chose Hilary and Hillary, just as in the example code. I wanted to know
# if naming frequencies changed after presidential elections involving the Clintons.

############
#Question 7#
############

# Question 3 showed that the all-time most common names were 
# Michael and Mary. Let's subset to those names and Hilary/Hillary,
# and then sum up all-time totals.
ssa %>% 
  subset((name=="Michael"&sex=="M")|(name=="Mary"&sex=="F")|
         (name=="Hilary"&sex=="F")|(name=="Hillary"&sex=="F")) %>%
  group_by(name) %>%
  summarise(total = sum(frequency))

# Hilary/Hillary has been given to 53k girls since 1940, compared
# to 20 million Marys band 43 million Michaels.

############
#Question 8#
############

# Graph counts of both names, with a vertical line at 1992, 2008, and 2016.
# We will use the ggplot2 package, which is already loaded in the tidyverse.
# In the ggplot function, I first subset the ssa dataframe using dplyr syntax.

ssa %>% 
  subset((name=="Hilary"&sex=="F")|(name=="Hillary"&sex=="F")) %>% 
  ggplot(aes(x=year, y=frequency, group=name, color=name)) +
    geom_line() +
    geom_vline(xintercept=c(1992, 2008, 2016)) +
    scale_y_continuous(trans = "log10") # log scale not necessary but helpful for these names

# Hilary and Hillary steadily increased in popularity from 1940 to the early
# 1990s but then plummeted after the 1992 Clinton-Bush election. The names
# stabilized in popularity for the next decade and a half, then increased
# somewhat before the 2008 Clinton-Obama primary and fell again. Hillary then
# decreased further after the 2016 Clinton-Trump election.
