
# Some basic math calculations using R

five <- 1 + 1 + 3

(2 + 9) / 7 # comment: result is: 1.57

five + 3

nine <- 1 + 1 + 3

nine - five

nine = five

nine > five

nine < five

sqrt(10)

seq(1, 10, 2)

rep(5, 10)

gsub("R-studio", "Rstudio", "R-studio is a great piece of software")

grepl("chocolate", "Mary likes chocolate cookies")

# Packages

install.packages("tidyverse")  ## this command installs packages from CRAN; note the quotation marks around the package name

tidyverse::tidyverse_packages()

library(tidyverse)

pacman::p_load(
  tidyverse,    # general data management tools ('dplyr', etc.)
  mosaic,       # formula-type syntax for descriptive statistics
  ggformula,    # ggplot2 powered graphing using 'mosaic' formula-syntax
  summarytools, # summary statistics tables
  sjlabelled,   # data import from other software (alternative to 'haven') and labels management
  sjmisc,       # data transformation on variables (recoding,grouping, missing values, etc)
  sjPlot,       # Graphing and tabulation tools for regression model results
  jtools       # Graphing and tabulation tools for regression model results
)

evs <- sjlabelled::read_stata("https://cgmoreh.github.io/SSC7001M/data/evs5.dta")

# Number of rows (as first element), and the number of columns (as the second element); the 'dimensions' of the object:
dim(evs)

# Or separately, the number of rows (cases, observations):
nrow(evs)

# And number of columns (variables):
ncol(evs) 

evs.slim <- evs %>% select("country", "v31", "v33", "v35":"v37", "v54", "v55", "v102", "v225", "age")

sjlabelled::get_labels(evs.slim$country, values ="as.prefix")

evs.slim.gb <- evs.slim %>% filter(country==826)

evs.slim.gb %>% sjmisc::descr(show = c("type", "label", "range", "md", "NA.prc"))


evs.slim.gb %>% count(v31)

evs.slim.gb %>% frq(v31)

# Renaming variables 
evs.slim.gb <- evs.slim.gb %>%
  rename(ppl.trust.yn = v31)

# Getting rid of NAs
evs.slim.gb <- evs.slim.gb %>% set_na(ppl.trust.yn, na = c(-1:-10))


evs.slim.gb %>% frq(ppl.trust.yn)


evs.slim.gb <- evs.slim.gb %>% 
  sjmisc::var_rename(
    ppl.trust.yn = Trusting,
  )


evs.slim.gb <- evs.slim.gb %>% 
  sjmisc::recode_to(v33:v37, lowest = 0, suffix = "") %>%             # 1 #
  filter(rowSums(across(c(Trusting, v33:v37), ~ is.na(.))) < 3) %>%   # 2 #
  mutate(trust_in_strangers = as.numeric(                             # 3 #
    (Trusting * 3) +                                      # 3.1 #
      v33 + v35 + v36 + v37))                             # 3.2 #
