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

##### Read in data ----------
ukhls <- readRDS("ukhls_w8.rds")



##### Step 1: Selecting variables ----------


##### Student task: descriptive stats ----------

# Any of the commands below, with or without the 'package::' call becasue the packages should already be loaded

summary(ukhls$scghq1_dv)
 # or #
mosaic::favstats( ~ scghq1_dv, data = ukhls)
 # or #
ukhls %>% sjmisc::descr(scghq1_dv)

histogram( ~ scghq1_dv, data = ukhls)
histogram( ~ scghq1_dv, fit = "normal", data = ukhls) # with a normal distribution superimposed

sjmisc::frq(ukhls$sex)
  # or #
ukhls %>% sjmisc::frq(sex)

  # or in base R; but with much less useful information without additional arguments; just in case there are errors loading packages #
table(ukhls$sex)
table(ukhls$sex) %>% prop.table()



##### Step x ----------

##### Step 4: Linear modeling ----------
