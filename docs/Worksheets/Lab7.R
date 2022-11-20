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

