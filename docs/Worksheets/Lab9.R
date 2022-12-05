### ---- Lab 9 script ---- ###





##### Load packages ----------

# If you need to first install the packages, remove the hashtag 
#   from in front of the `install.packages()` command to un-comment it 
#   and run it

# install.packages(c("tidyverse", "mosaic", "sjmisc", "jtools"))     

library(tidyverse)
library(mosaic)
library(sjmisc)
library(jtools)

##### Read in data ----------

ukhls <- readRDS("ukhls_w8.rds")



##### Step 1: Selecting variables ----------



##### Student task: descriptive stats ----------

# Descriptive statistics for 'sex':

ukhls %>% sjmisc::frq(sex)

# or #

frq(ukhls$sex)           # with sjmisc:: loaded



# Descriptive statistics for 'age_dv':

favstats( ~ age_dv, data = ukhls)         # with mosaic:: loaded




# Descriptive statistics for 'smoker':

frq(ukhls$smoker)         # with sjmisc:: loaded



# Descriptive statistics for 'finnow':

frq(ukhls$finnow)         # with sjmisc:: loaded


ukhls %>% 
  select(sex, age_dv, smoker, finnow) %>% 
  summary()

boxplot(age_dv ~ smoker, data = ukhls)

xtabs( ~ smoker + sex, data = ukhls)

xtabs( ~ smoker + sex, data = ukhls) %>% addmargins()


# row proportions:

xtabs( ~ smoker + sex, data = ukhls) %>% prop.table(margin = 1)

# column proportions:

xtabs( ~ smoker + sex, data = ukhls) %>% prop.table(margin = 2)

xtabs( ~ smoker + sex, data = ukhls) %>% 
  prop.table(1) %>% 
  round(2)

xtabs( ~ smoker + sex, data = ukhls) %>% 
  prop.table(2) %>% 
  round(2)


# row proportions

xtabs( ~ smoker + sex, data = ukhls) %>% 
  prop.table(1) * 100

# column proportions

xtabs( ~ smoker + sex, data = ukhls) %>% 
  prop.table(2) * 100

sjmisc::flat_table(smoker, sex, data = ukhls)

# or as row percentages:

sjmisc::flat_table(smoker, sex, data = ukhls , margin = "row")

# or as column percentages:

sjmisc::flat_table(smoker, sex, data = ukhls , margin = "col")




# Contingency table showing frequencies and marginal totals

xtabs( ~ finnow + smoker, data = ukhls) %>% 
    addmargins()


# Contingency table showing column percentages (that's the more useful proportion table for us in this case):

flat_table(finnow, smoker, data = ukhls , margin = "row")

# Contingency table of 'finnow' by 'smoker' showing column percentages (less useful):

flat_table(finnow, smoker, data = ukhls , margin = "col")

#### Student questions -------------------

# Answer to the second question: because the 'Doing alright' group is the largest group in the variable as a whole, so it's likely that it will be the largest among both smokers and non-smokers. In essence, it doesn't tell us much and definitely it doesn't tell as the answer to what we are looking for. So this question should draw students' attention to the care one needs to take when interpreting marginal proportions in contingency tables.


##### Step 4: Linear modeling ----------

model_logistic <- glm(smoker ~ finnow + sex + age_dv, family = binomial, data = ukhls)

jtools::summ(model_logistic, digits=3)

jtools::summ(model_logistic, digits=3, exp = TRUE)

# Fit a simpler model



model_logistic_simple <- glm(smoker ~ finnow, family = binomial, data = ukhls)
jtools::summ(model_logistic_simple, digits=3, exp = TRUE)
