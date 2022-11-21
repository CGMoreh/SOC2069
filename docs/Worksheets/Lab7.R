## -----------------------------------------------------------------------------
# install.packages(c("tidyverse", "mosaic", "sjmisc")) # If you need to first install the packages, remove the hashtag from in front of the `install.packages()` command to un-comment the command and make it executable in R

library(tidyverse)
library(mosaic)
library(sjmisc)

## ----eval=FALSE---------------------------------------------------------------
#  ukhls <- readRDS("ukhls_w8.rds")

## ----echo=TRUE, fig.show='asis'-----------------------------------------------
favstats( ~ age_dv, data = ukhls)
histogram( ~ age_dv, fit = "normal", data = ukhls)
favstats( ~ scghq1_dv, data = ukhls)
histogram( ~ scghq1_dv, fit = "normal", data = ukhls)

## -----------------------------------------------------------------------------
bwplot( ~ scghq1_dv, data = ukhls)

## -----------------------------------------------------------------------------
ukhls <- ukhls %>% 
  mutate(wellbeing = 36-scghq1_dv)

## -----------------------------------------------------------------------------
base::max(ukhls$scghq1_dv)
mosaic::max( ~ scghq1_dv, data = ukhls)

## -----------------------------------------------------------------------------
max(ukhls$scghq1_dv, na.rm = TRUE)
mosaic::max( ~ scghq1_dv, data = ukhls, na.rm = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  maxvalue <- max(ukhls$scghq1_dv, na.rm = TRUE)    # extract the highest value
#  
#  print(maxvalue)                                   # print `maxvalue` to the console to check it
#  
#  ukhls <- ukhls %>%
#    mutate(wellbeing = maxvalue-scghq1_dv)          # use `maxvalue` instead of 36

## ----results='hold'-----------------------------------------------------------
favstats( ~ scghq1_dv, data = ukhls)
favstats( ~ wellbeing, data = ukhls)

## -----------------------------------------------------------------------------
ukhls %>% 
  select(scghq1_dv, wellbeing) %>% 
  summary()

## -----------------------------------------------------------------------------
xyplot(wellbeing ~ age_dv, data = ukhls)

## -----------------------------------------------------------------------------
xyplot(wellbeing ~ age_dv, type = c("p", "r"), data = ukhls)

## -----------------------------------------------------------------------------
xyplot(wellbeing ~ age_dv, 
       type = c("p", "r"), 
       col.line = "red", 
       data = ukhls)

## -----------------------------------------------------------------------------
cor.test(wellbeing ~ age_dv, data = ukhls)

## -----------------------------------------------------------------------------
model1 <- lm(wellbeing ~ 1 + age_dv, data = ukhls)

## ----eval=FALSE---------------------------------------------------------------
#  model1 <- lm(wellbeing ~ age_dv, ukhls)

## -----------------------------------------------------------------------------
coef(model1)

## -----------------------------------------------------------------------------
lm(wellbeing ~ 1, data = ukhls) %>% coef()

mean( ~ wellbeing, data = ukhls, na.rm = TRUE)

## -----------------------------------------------------------------------------
24.3470 + (0.0109 * 21)

## ---- eval=FALSE--------------------------------------------------------------
#  # First, extract the median age value from the data using the `median()` function:
#  
#  medianAge <- median( ??? )
#  
#  # Then `print` the median to check it and see if it's correct
#  
#  ???
#  
#  # Then use the `mutate` function to create the new variables called "age_median_centred" by subtracting the median age from all values of `age_dv`
#  
#  ???

## ----echo=F-------------------------------------------------------------------
medianAge <- median( ~ age_dv, data = ukhls, na.rm = TRUE)

ukhls <- ukhls %>% 
  mutate(age_median_centred = age_dv - medianAge)

## -----------------------------------------------------------------------------
favstats( ~ age_median_centred, data = ukhls)

## ----eval=FALSE---------------------------------------------------------------
#  # Complete the lm() command
#  
#  model2 <- ????
#  
#  # Then print the coefficients from model2
#  
#  

## ----echo=FALSE---------------------------------------------------------------
model2 <- lm(wellbeing ~ age_median_centred, ukhls)
coef(model2)

## -----------------------------------------------------------------------------
summary(model2)

## -----------------------------------------------------------------------------
summary(model2) %>% rsquared %>% sqrt()

cor.test(wellbeing ~ age_median_centred, data = ukhls)$estimate

## ----eval=F-------------------------------------------------------------------
#  options(scipen = 999)

