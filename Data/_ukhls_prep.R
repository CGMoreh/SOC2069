# ## Data prep
# 
# # library(tidyverse)
# # #library(strengejacke)
# # library(sjlabelled)
# # library(summarytools)
# # library(sjmisc)
# # library(sjPlot)
# 
# # us <- haven::read_spss("A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/h_indresp_td.sav") 
# 
# # us <- us |> sjlabelled::as_label() 
# 
# # h <- haven::read_sav("A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/h_indresp_td.sav") |> mutate_all(list(~na_if(., .<0)))
# 
# # Import originally downloaded dataset
# 
# path_office <- "A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/Data/6614stata_5FAC8DA9C415588B7AFA54AB180477074A808CCA1CD9354267911054D02007D4_V1/UKDA-6614-stata/stata/stata13_se/ukhls/h_indresp.dta"
# 
# path_hp_laptop <- "C:/Users/ncm281/OneDrive - Newcastle University/WORK Newcastle/SOC2069/Data/6614stata_5FAC8DA9C415588B7AFA54AB180477074A808CCA1CD9354267911054D02007D4_V1/UKDA-6614-stata/stata/stata13_se/ukhls/h_indresp.dta"
# 
# ## Load original dataset
# us <- read_stata(path_office) %>% 
#   set_na(na = c(-1:-21)) %>%                 # Set negative coded missing values to NA  
#   select(where(~mean(is.na(.)) < 0.6)) %>%   # Remove columns with more than 40% missing
#   select(
#     pidp, h_ivfio, h_ioutcome, # id, outcome
#     h_sex, h_nchunder16, h_nchresp, 	h_nadoptch, h_nnatch, # sex, children
#     h_istrtdaty:h_istrtdatd, # interview date
#     h_lkmove:h_netpuse, # house move, economic activity, car, phone, computers, internet
#     h_oprlg:h_oprlg3, # religion
#     h_health:h_aidxhh, # health, caring for someone
#     h_mstatsam, h_currmstat,
#     h_jbhas:h_jbterm1, h_jbsemp, h_jbmngr, h_jbsect:h_jbsat, h_tujbpl, h_wkends:h_jbflex96, # work
#     h_depenth1:h_depenth6, h_jbsec, h_j2has:h_trbikefq, h_ynotbike3, h_pncars:h_carbuy97,
#     h_benbase1:h_bensta96, # benefits
#     h_debt1:h_debt96, # debts
#     h_finnow:h_save, # finances
#     h_hubuys:h_hudiy, # Who does...
#     h_huboss, h_howlng,
#     h_scsf1, h_scghqa:h_scghql, 
#     h_sclfsat1:h_sclfsato,
#     h_eumem, h_scwhorupro:h_scwhoruage,
#     h_marstat, h_employ, h_ppsex, h_hhsize, 
#     h_fimngrs_dv, h_paygl:h_paynu_dv, 
#     h_age_dv, h_doby_dv, h_marstat_dv, h_ethn_dv, h_fimnnet_dv, h_country, h_urban_dv, h_agegr10_dv,
#     h_nchild_dv, h_ndepchl_dv, h_hiqual_dv, h_jbft_dv, h_jbes2000, h_jbnssec8_dv:h_jbnssec3_dv,  # various personal
#     h_scghq1_dv:h_jwbs2_dv, # scales
#     -c(h_ivfio, h_ioutcome, h_istrtdaty:h_istrtdatd, h_mstatsam, h_currmstat,	h_jbsect, h_jbot, h_payusl,
#        h_paytyp, 	h_paygl, h_paynl, h_payg_dv, h_fimngrs_dv, h_ndepchl_dv, h_jbft_dv, h_jbes2000, 
#        h_scghq2_dv, h_benpen1:h_benpen96 ))  %>% 
#   labelled::remove_attributes("format.stata") %>% 
#   drop_labels() %>% 
#   mutate_if(is.factor , as_label) %>% 
#   rename_all(~stringr::str_replace(.,"^h_",""))
# 
# 
# sjlabelled::write_stata(us, "docs/Data/ukhls_w8.dta")  # Stata .dta
# saveRDS(us, file = "docs/Data/ukhls_w8.rds")           # gz compressed .rds
# 
# summarytools::dfSummary(us) %>% print(file = "docs/Data/varlist2.html", footnote = NA)
# 
# ### ADD TO <BODY>
# # <div class="container st-container">
# #   <h4>The UK Household Longitudinal Study (Understanding Society), Wave 8 (2016-17)</h4>
# #   <b>Data set name</b>: ukhls_w8
# #   <b>Dimensions</b>: 39293 x 199 <br>
# #   <b>Description</b>: This dataset is an edited version of the <b>h_indresp</b> dataset available from Understanding Society. The following changes have been done to the original dataset:
# #   <li>it has been reduced in size, keeping only <b>199</b> variables of the original <b>2152</b></li>
# #   <li>the wave prefix (<b>h_</b>) has been removed from all the variable names</li>
# #   <li>all negative-coded missing values (-21 to -1) were transformed to generic missing (NA) values</li>
# #   Keeping the above in mind, <a href="https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation">the variable search function </a> provided on the Understanding Society website can and should be used to gain insight into the meaning of variables beyond the description provided in the variable labels.<br>
# #   <b>Citation</b>: To cite this data, use the recommended citation to the original dataset:
# #   <li>University of Essex, Institute for Social and Economic Research (2022) Understanding Society: Wave 8, 2016-2017. UK Data Service, SN: 6614, http://doi.org/10.5255/UKDA-SN-6614-17</li>
# #   
# # #   ...
# # 
# # ### Change the page margins
# #   .st-container {
# #     width: 100%;
# #     padding-right: 15px;
# #     padding-left: 15px;
# #     margin-right: 5px;
# #     margin-left: 5px;
# #     margin-top: 15px;
# #   }
# 
# 
# # Read in data
# 
# us <- readRDS("C:/Users/ncm281/Documents/SOC2069/docs/Data/ukhls_w8.rds")
# 
# 
# # Gender identity variable for assignment
# 
# sjmisc::frq(us$scwhorusex)
# 
# table(us$scwhorusex, useNA = "always")
# 
# us <- us %>% sjmisc::rec(scwhorusex, rec = "Very important to my sense of who I am, 
# Fairly important to my sense of who I am
#             =1 [Important]; 
#             Not very important to my sense of who I am,
#             Not at all important to my sense of who I am
#             =0 [Not Important]")
# 
# us <- us %>% mutate(scwhorusex_r = as_label(scwhorusex_r))
# 
# 
# sjmisc::frq(us$scwhorusex_r)
# 
# 
# 
# 
# # Small data for testing
# 
# set.seed(123)
# us_t <- us3 %>% slice_sample(n = 300)
# us3 <- us3 %>% mutate_if(is.factor , as_numeric) %>% mutate_all(as_label) 
# us3 <- us3 %>% mutate_if(is.factor , as_label)                         
# 
# # Tabulations
# 
# us4 %>% frq(h_ethn_dv)
# us4 %>% count(h_ethn_dv)
# 
# us4 %>% filter(as_numeric(h_ethn_dv)==12) %>% frq(h_ethn_dv)
# 
# us3 %>% filter(h_ethn_dv==12) %>% frq(h_ethn_dv)
# us4 %>% filter(h_ethn_dv=="chinese") %>% count(h_ethn_dv)
# 
# us4 %>% frq(h_jwbs1_dv)
# 
# # Data catalogues
# 
# us3 %>% 
#   summarytools::dfSummary() %>% print(file = "docs/Data/varlist.html")
# 
# us3 %>% mutate_if(is.factor , as_label) %>% 
#   summarytools::dfSummary() %>% print(file = "docs/Data/varlist_labels.html")
# 
# 
# summarytools::dfSummary(as_character(as_numeric(us3))) %>% print(file = "docs/Data/us3n_variable_list.html")
# 
# summarytools::dfSummary(us_t2, style = "grid")
# 
# 
# descr(us_csv, out = "browser", file = "docs/Data/var_descr")
# 
# 
# 
# summarytools::dfSummary(h) |> summarytools::view()
# 
# h <- h |> 
#   mutate(h_sex_dv = sjlabelled::as_label(h_sex_dv))
# 
# # Graphs
# 
# us_csv %>% ggplot() + 
#   aes( h_age_dv, 	
#        h_fimnnet_dv) + 
#   geom_point() +
#   theme_minimal()
# 
# barplot(table(us_csv$h_sex))
# 
# gf_boxplot(h_age_dv ~ h_sex, data = us_csv)
# 
# gf_density( ~ h_age_dv, data = us_csv)
# 
# gf_bar( ~ h_sex, data = us_csv) +
#   theme_minimal()
