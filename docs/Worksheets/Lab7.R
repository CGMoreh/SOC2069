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
#    mutate(wellbeing2 = maxvalue-scghq1_dv)          # use `maxvalue` instead of 36

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

