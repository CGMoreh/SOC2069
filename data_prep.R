## Data prep

library(sjlabelled)
library(tidyverse)

us <- haven::read_spss("A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/h_indresp_td.sav") 

us <- us |> sjlabelled::as_label() 

h <- haven::read_sav("A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/h_indresp_td.sav") |> mutate_all(list(~na_if(., .<0)))

us <- read_dta("C:/Users/ncm281/OneDrive - Newcastle University/WORK Newcastle/SOC2069/Data/6614stata_5FAC8DA9C415588B7AFA54AB180477074A808CCA1CD9354267911054D02007D4_V1/UKDA-6614-stata/stata/stata13_se/ukhls/h_indresp.dta")

us <- us %>% set_na(na = c(-1:-99))


labelled::remove_attributes("format.spss")

summarytools::dfSummary(h) |> summarytools::view()

h <- h |> 
  mutate(h_sex_dv = sjlabelled::as_label(h_sex_dv))
