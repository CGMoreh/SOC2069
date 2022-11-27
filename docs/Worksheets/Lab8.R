### ---- Lab 8 script ---- ###





##### Load packages ----------

# If you need to first install the packages, remove the hashtag 
#   from in front of the `install.packages()` command to un-comment it 
#   and run it

# install.packages(c("tidyverse", "mosaic", "sjmisc"))     

library(tidyverse)
library(mosaic)
library(sjmisc)

# We'll also install a new package that contains some nice functions 
#   for better formatted table outputs and graphing of regression models   

install.packages("jtools")
library(jtools)
library(jtools)

##### Read in data ----------

ukhls <- readRDS("ukhls_w8.rds")



##### Step 1: Selecting variables ----------



##### Student task: descriptive stats ----------

# Any of the commands below, with or without the 'package::' call becasue the packages should already be loaded

summary(ukhls$scghq1_dv)
 # or, ideally: #
mosaic::favstats( ~ scghq1_dv, data = ukhls)
 # or, if they copy from Lab6: #
ukhls %>% sjmisc::descr(scghq1_dv)

histogram( ~ scghq1_dv, data = ukhls)
histogram( ~ scghq1_dv, fit = "normal", data = ukhls) # with a normal distribution superimposed

sjmisc::frq(ukhls$sex)
  # or #
ukhls %>% sjmisc::frq(sex)

# or in base R; but with much less useful information without additional arguments; just in case there are errors loading packages #
summary(ukhls$sex)
summary(ukhls$sex) %>% prop.table()

# extract the highest value (check in the Environment if the extracted value is correct)
maxvalue <- max(ukhls$scghq1_dv, na.rm = TRUE)    

# reverse the scale
ukhls <- ukhls %>% 
  mutate(wellbeing = maxvalue - scghq1_dv)  

# quickly compare the two variables to make sure the recoding was correct
ukhls %>% 
  select(scghq1_dv, wellbeing) %>% 
  summary() 

# We should get this output:

xyplot(wellbeing ~ sex, type = c("p", "r"), data = ukhls)

# with base R `boxplot`
boxplot(wellbeing ~ sex, data = ukhls)
# with `bwplot`
bwplot(wellbeing ~ sex, data = ukhls)
# with `bwplot`, adding the setting ' pch = "|" ' which transforms the median dot into a line, like in `boxplot`, making the comparison easier
bwplot(wellbeing ~ sex, data = ukhls, pch = "|")

# with `boxplot`, add "horizontal = TRUE"
boxplot(wellbeing ~ sex, data = ukhls, horizontal = TRUE)

# with `bwplot` we can simply reverse the order of variables and place our categorical (factor, dichotomous) variable first. It's as if we'd say that we're modelling the sex variable on wellbeing, rather than the other way around.
bwplot(sex ~ wellbeing, data = ukhls, pch = "|")

mosaic::favstats(wellbeing ~ sex, data = ukhls)

##### Step 4: Linear modeling ----------

model_ex1 <- lm(wellbeing ~ sex, data = ukhls)

summary(model_ex1)



# Check a variable's type/class:
class(ukhls$sex)
# Check the structure of the variable:
str(ukhls$sex)
# Check if a variable is a 'factor':
is.factor(ukhls$sex)

# ! This will overwrite the previous model object
model_ex1 <- lm(wellbeing ~ as.factor(sex), data = ukhls)

summary(model_ex1)

##### Step 5: Presentation and interpretation ----------

# t-value for the factor variable: -/+ 19.83
t <- -19.83

# F-statistic: 393.2
F <- 393.2

# Is the statement "t squared (and rounded to one decimal point) equals F" true?
round(t^2, 1) == F

# A t-test; to get exactly the same test statistic as in the regression, we must assume that the variances in the two groups are equal ('var.equal=TRUE')
t.test(wellbeing ~ sex, data = ukhls, var.equal=TRUE)

# An ANOVA model; we can view its summary() just as with the lm() models
aov(wellbeing ~ sex, data = ukhls) %>% summary()

jtools::summ(model_ex1)

jtools::summ(model_ex1, digits = 3, confint = TRUE)


##### EXERCISE 2 CODE----------

sjmisc::frq(ukhls$hiqual_dv)

### Student exercise ---------------------

bwplot(hiqual_dv ~ wellbeing, data = ukhls, pch = "|")

mosaic::favstats(wellbeing ~ hiqual_dv, data = ukhls)


model_ex2 <- lm(wellbeing ~ as.factor(hiqual_dv), data = ukhls)

jtools::summ(model_ex2, digits = 3, confint = TRUE)

# take the data, `mutate` to create a new variable that is = to the result from the `ref_lvl` function:
ukhls <- ukhls %>% 
  mutate(ref_no_qual = ref_lvl(hiqual_dv, lvl = 6)
         )

# Check a frequency table of the old and new variables to see if it'sall as expected:
ukhls %>% frq(hiqual_dv)

ukhls %>% frq(ref_no_qual)

## model_ex2_refitted

model_ex2_refitted <- lm(wellbeing ~ as.factor(ref_no_qual), data = ukhls)

summ(model_ex2_refitted, digits = 3, confint = TRUE)

##### EXERCISE 3 CODE----------


# Median-centre 'age'

medianAge <- median( ~ age_dv, data = ukhls, na.rm = TRUE)

ukhls <- ukhls %>% 
  mutate(age_median_centred = age_dv - medianAge)

# All other variables are already prepared; fit the model:



model_ex3 <- lm(wellbeing ~ age_median_centred + as.factor(sex) + as.factor(hiqual_dv), data = ukhls)

jtools::summ(model_ex3, digits = 3, confint = TRUE)
