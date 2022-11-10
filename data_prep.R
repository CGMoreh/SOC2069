## Data prep

us <- haven::read_spss("A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/h_indresp_td.sav") 

us <- us |> sjlabelled::as_label() 

h <- haven::read_sav("A:/OneDrive - Newcastle University/WORK Newcastle/SOC2069/h_indresp_td.sav") |> mutate_all(list(~na_if(., .<0)))

_all(funs(replace(., .<0, NA)))

labelled::remove_attributes("format.spss")

summarytools::dfSummary(h) |> summarytools::view()

h <- h |> 
  mutate(h_sex_dv = sjlabelled::as_label(h_sex_dv))
