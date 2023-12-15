# ECON 121: Applied Econometrics
# Final Exam Solution

# Load R packages
library(tidyverse)
library(fixest)
library(car)
library(mfx)

# Load data
load(url("https://github.com/tvogl/econ121/raw/main/data/nfhs5.Rdata"))

# Part A
# A1 
# A linear probability model is estimated by OLS. We cluster at the PSU level 
# because of the clustered sampling design.
feols(child_u1death ~ child_bord, 
      data = nfhs5, 
      vcov = ~psu_id)

# A2
# An increase of birth order by 1 is associated with a .2 percentage point
# increase in the probability of dying before age 1. The p-value is 0.000, so
# the coefficient is significantly different from 0.

# A3  
# The odds ratio would be greater than 1 because the linear probability model
# indicates that higher birth orders are associated with higher mortality risk.
# That means the logit coefficient is likely to be positive. The odds ratio is the
# exponentiated logit coefficient, and the exponential of a positive number is
# greater than 1.

# A4  
feols(child_u1death ~ child_bord | mom_id, 
      data = nfhs5, 
      vcov = ~psu_id)
# Most analyses would continue to cluster at the PSU level, but you did not 
# lose points if you did not.

# A5  
# An increase of birth order by 1 is associated with a 1.8 percentage point
# decrease in the probability of dying before age 1. The coefficient from A4
# is negative, whereas the coefficient from A1 is positive. Since birth order 
# is positively associated with the number of children, the formulas for omitted
# variable bias imply that the number of children must be positively associated
# with mortality risk.

# A6
feols(child_u1death ~ child_bord + child_birthyr | mom_id, 
      data = nfhs5, 
      vcov = ~psu_id)
# You could have alternatively included birth year dummies, which would have 
# led to similar results.

# A7 
# Now the coefficient implies that an increase of birth order by 1 is 
# associated with a 2.9 percentage point deccrease in the probability 
# of dying before age 1. Now that we have purged the estimate of family
# size and birth year variation, it is reasonable to interpret it as a 
# causal effect of birth order. It captures how being later born in the
# family affects mortality risk, net of birth year. It is also possible
# to argue the opposite, so we graded based on the explanation only.

# A8
feols(child_u1death ~ child_bord*child_male + child_birthyr*child_male | mom_id, 
      data = nfhs5, 
      vcov = ~psu_id)
# It was important to include the main effect of child_male as well as its
# interaction with child_bord. To be careful, we should also include the
# interaction with child_birthyr, so that we can distinguish between
# gender differences in birth order effects and birth year effects.

# A further possibility is to interact the mother fixed effects with the gender 
# of the child, so that we only compare siblings of the same gender. This goes
# beyond what we discussed in class, but I wanted to include if in case you
# are interested.
nfhs5 <- mutate(nfhs5, mom_gender_id = 2*mom_id+child_male) # unique number for each mom-gender combination
feols(child_u1death ~ child_bord*child_male + child_birthyr*child_male | mom_gender_id, 
      data = nfhs5, 
      vcov = ~psu_id)
# Or you can use a fixest shortcut:
feols(child_u1death ~ child_bord*child_male + child_birthyr*child_male | mom_id^child_male, 
      data = nfhs5, 
      vcov = ~psu_id)
# Here we see that the benefits of being later born are much stronger for boys
# than for girls. The difference between this regression and the last comes from
# mortality risk being more associated with family size for boys than for girls.
# Note that we cannot include child_male in the regression because it is collinear
# with the fixed effects.

# A9
model1 <- feols(child_u1death ~ mom_rural + mom_edyrs + mom_age, 
                data = nfhs5, 
                vcov = ~psu_id)
summary(model1)
deltaMethod(model1, "mom_rural + (5-12)*mom_edyrs + (15-40)*mom_age")
deltaMethod(model1, "(Intercept + mom_rural + 5*mom_edyrs + 15*mom_age)/(Intercept + 12*mom_edyrs + 40*mom_age)")                                

# A10
# The answer in absolute terms comes from the linear combination, which
# indicates that infant mortality risk is 2.3 percentage points higher
# for the 15-year-old rural mom with 5 years than for the 40-year-old
# urban mom with 12 years. The 95% confidence interval excludes 0, so
# this difference is statistically significant at conventional levels.

# The answer in proportional terms comes from the nonlinear combination, which
# indicates that infant mortality risk for the 15-year-old rural mom 
# with 5 years is 1.93 times the infant mortality risk for the 40-year-old
# urban mom with 12 years. The 95% confidence interval excludes 1, so 
# this ratio is statistically significant at conventional levels.	  

# Part B
# B1
nfhs5 <- subset(nfhs5, child_bord==1)
feols(mom_working ~ mom_kids, 
      data = nfhs5, 
      vcov = ~psu_id)

# B2
# Poor moms may need to work for survival, and they tend to have more children.

# B3
# This instrument is unlikely to satisfy the exclusion restriction. For instance,
# gender-biased mothers might be more likely to stay home to case for a boy than
# for a girl. You could have also argued against the independence assumption 
# because of sex-selective abortion.

# B4
feols(mom_kids ~ child_male, 
      data = nfhs5, 
      vcov = ~psu_id)
feols(mom_working ~ child_male, 
      data = nfhs5, 
      vcov = ~psu_id)

# B5
# The first regression is the first-stage regression. The second regression is
# the reduced-form regression.

# B6
# The instrumental variables estimator is equal to the reduced form coefficient
# divided by the first-stage coefficient. In this case, -.0039/-.2806 = .0139, 
# implying that each additional child raises the probability of maternal work by
# 1.4 percentage points.

# B7
feols(mom_working ~ 1 | mom_kids ~ child_male, 
      data = nfhs5, 
      vcov = ~psu_id)

# B8
# If the IV assumptions were met, the result here captures the average effect
# of an additional child among women whose fertility depended on the gender of
# the first child.

# B9
probitmfx(child_male ~ mom_rural + mom_edyrs + child_birthyr + mom_age, 
          data = nfhs5, 
          atmean = TRUE,
          clustervar1 = "psu_id")

# B10
# An additional year of maternal education is associated with a .07 percentage
# point decline in the probability of having a first-born boy. The association
# is statistically significant at conventional levels, so it poses a concern for
# the independence assumption, which implies that the instrument should be 
# unrelated to the mother's pre-birth characteristics.

