## Data prep

library(tidyverse)
library(sjlabelled)
library(summarytools)
library(sjmisc)

# us <- haven::read_spss("A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/h_indresp_td.sav") 

# us <- us |> sjlabelled::as_label() 

# h <- haven::read_sav("A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/h_indresp_td.sav") |> mutate_all(list(~na_if(., .<0)))

# Import originally downloaded dataset

path_office <- "A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/Data/6614stata_5FAC8DA9C415588B7AFA54AB180477074A808CCA1CD9354267911054D02007D4_V1/UKDA-6614-stata/stata/stata13_se/ukhls/h_indresp.dta"

path_hp_laptop <- "C:/Users/ncm281/OneDrive - Newcastle University/WORK Newcastle/SOC2069/Data/6614stata_5FAC8DA9C415588B7AFA54AB180477074A808CCA1CD9354267911054D02007D4_V1/UKDA-6614-stata/stata/stata13_se/ukhls/h_indresp.dta"

## Load original dataset
us <- read_stata(path_office)
us2 <- haven::read_dta(path_office)

## Set all negative values to NA; see: https://www.understandingsociety.ac.uk/documentation/mainstage/user-guides/main-survey-user-guide/missing-values
us2 <- us %>%  
  set_na(na = c(-1:-21)) %>%            # Set negative coded missing values to NA  
  select(where(~mean(is.na(.)) < 0.6))  # Remove columns with more than 40% missing

us3 <- us2 %>% select(
  pidp, h_ivfio, h_ioutcome, # id, outcome
  h_sex, h_nchunder16, h_nchresp, 	h_nadoptch, h_nnatch, # sex, children
  h_istrtdaty:h_istrtdatd, # interview date
  h_lkmove:h_netpuse, # house move, economic activity, car, phone, computers, internet
  h_oprlg:h_oprlg3, # religion
  h_health:h_aidxhh, # health, caring for someone
  h_mstatsam, h_currmstat,
  h_jbhas:h_jbterm1, h_jbsemp, h_jbmngr, h_jbsect:h_jbsat, h_tujbpl, h_wkends:h_jbflex96, # work
  h_depenth1:h_depenth6, h_jbsec, h_j2has:h_trbikefq, h_ynotbike3, h_pncars:h_carbuy97,
  h_benbase1:h_bensta96, # benefits
  h_debt1:h_debt96, # debts
  h_finnow:h_save, # finances
  h_hubuys:h_hudiy, # Who does...
  h_huboss, h_howlng,
  h_scsf1, h_scghqa:h_scghql, 
  h_sclfsat1:h_sclfsato,
  h_eumem, h_scwhorupro:h_scwhoruage,
  h_marstat, h_employ, h_ppsex, h_hhsize, 
  h_fimngrs_dv, h_paygl:h_paynu_dv, 
  h_age_dv, h_doby_dv, h_marstat_dv, h_ethn_dv, h_fimnnet_dv, h_country, h_urban_dv, h_agegr10_dv,
  h_nchild_dv, h_ndepchl_dv, h_hiqual_dv, h_jbft_dv, h_jbes2000, h_jbnssec8_dv:h_jbnssec3_dv,  # various personal
  h_scghq1_dv:h_jwbs2_dv # scales
)


# Small data for testing

set.seed(123)
us_t <- us3 %>% slice_sample(n = 300)


us3 <- us3 %>% mutate_if(is.factor , as_numeric) %>% mutate_all(as_label) 

us3 <- us3 %>% mutate_if(is.factor , as_label)                         
  

us_t2 <- us_t %>% mutate(across(where(~ length(get_labels(.x))<12), as_label))
  
  mutate_if({length(get_labels(.)) < 10}, as_label)




# Data catalogues

us %>% summarytools::dfSummary() %>% print(file = "docs/Data/us_variable_list.html")

summarytools::dfSummary(as_character(as_numeric(us3))) %>% print(file = "docs/Data/us3n_variable_list.html")

summarytools::dfSummary(us_t2, style = "grid")



# us3 %>% sjmisc::descr()



labelled::remove_attributes("format.spss")

summarytools::dfSummary(h) |> summarytools::view()

h <- h |> 
  mutate(h_sex_dv = sjlabelled::as_label(h_sex_dv))
