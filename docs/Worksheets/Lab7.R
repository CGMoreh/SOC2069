### ---- Lab 7 script ---- ###









##### Load packages ----------

# install.packages(c("tidyverse", "mosaic", "sjmisc")) 
# If you need to first install the packages, remove the hashtag from in front of the `install.packages()` command to un-comment the command and make it executable in R

library(tidyverse)
library(mosaic)
library(sjmisc)

##### Read in data ----------
ukhls <- readRDS("ukhls_w8.rds")





##### Descriptive stats ----------
favstats( ~ age_dv, data = ukhls)
histogram( ~ age_dv, fit = "normal", data = ukhls)
favstats( ~ scghq1_dv, data = ukhls)
histogram( ~ scghq1_dv, fit = "normal", data = ukhls)

bwplot( ~ scghq1_dv, data = ukhls)

##### Data transformation ----------
ukhls <- ukhls %>% 
  mutate(wellbeing = 36-scghq1_dv)

base::max(ukhls$scghq1_dv)
mosaic::max( ~ scghq1_dv, data = ukhls)

max(ukhls$scghq1_dv, na.rm = TRUE)
mosaic::max( ~ scghq1_dv, data = ukhls, na.rm = TRUE)

maxvalue <- max(ukhls$scghq1_dv, na.rm = TRUE)    # extract the highest value

print(maxvalue)                                   # print `maxvalue` to the console to check it

ukhls <- ukhls %>% 
  mutate(wellbeing = maxvalue-scghq1_dv)          # use `maxvalue` instead of 36

favstats( ~ scghq1_dv, data = ukhls)
favstats( ~ wellbeing, data = ukhls)

ukhls %>% 
  select(scghq1_dv, wellbeing) %>% 
  summary()

##### Step 3: Bivariate relationships ----------

xyplot(wellbeing ~ age_dv, data = ukhls)

xyplot(wellbeing ~ age_dv, type = c("p", "r"), data = ukhls)

xyplot(wellbeing ~ age_dv, 
       type = c("p", "r"), 
       col.line = "red", 
       data = ukhls)

cor.test(wellbeing ~ age_dv, data = ukhls)

##### Step 4: Linear modeling ----------

model1 <- lm(wellbeing ~ 1 + age_dv, data = ukhls)

model1 <- lm(wellbeing ~ age_dv, ukhls)

coef(model1)

lm(wellbeing ~ 1, data = ukhls) %>% coef()

mean( ~ wellbeing, data = ukhls, na.rm = TRUE)

24.3470 + (0.0109 * 21)

##### Back to Step 2: Centring the predictor ----------

#### Student task ----------



# First, extract the median age value from the data using the `median()` function:

medianAge <- median( ~ age_dv, data = ukhls, na.rm = TRUE)

# Then `print` the median to check it and see if it's correct

medianAge

# Then use the `mutate` function to create the new variables called "age_median_centred" by subtracting the median age from all values of `age_dv`

ukhls <- ukhls %>% 
  mutate(age_median_centred = age_dv - medianAge)

favstats( ~ age_median_centred, data = ukhls)



model2 <- lm(wellbeing ~ age_median_centred, ukhls)

coef(model2)

##### Step 5 ----------

summary(model2)

summary(model2) %>% rsquared %>% sqrt()

cor.test(wellbeing ~ age_median_centred, data = ukhls)$estimate

options(scipen = 999)
