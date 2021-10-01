source("InstallPackages.R")
library(gridExtra)

# Read in and clean DAH data.
dah = read.csv("IHME_DAH_DATABASE_1990_2019_Y2020M04D23.CSV")
dah_sub = dah %>% filter(elim_ch!=1)

# Select key immunization variables & reformat them.
dah_sub %<>% 
  select(year,
         source,
         channel,
         recipient_country, 
         gbd_region,elim_ch,
         prelim_est,
         nch_other_dah_19,
         nch_hss_hrh_dah_19,
         nch_hss_other_dah_19,
         nch_cnv_dah_19,
         nch_cnn_dah_19,
         swap_hss_pp_dah_19)
dah_sub$nch_other_dah_19=as.numeric(dah_sub$nch_other_dah_19)
dah_sub$nch_hss_hrh_dah_19=as.numeric(dah_sub$nch_hss_hrh_dah_19)
dah_sub$nch_hss_other_dah_19=as.numeric(dah_sub$nch_hss_other_dah_19)
dah_sub$nch_cnv_dah_19=as.numeric(dah_sub$nch_cnv_dah_19)
dah_sub$nch_cnn_dah_19=as.numeric(dah_sub$nch_cnn_dah_19)
dah_sub$swap_hss_pp_dah_19 = as.numeric(dah_sub$swap_hss_pp_dah_19)
dah_sub[is.na(dah_sub)] = 0

# Subset for Africa and sum all finances for a given day.
dahanalysis = dah_sub %>% mutate(Africa = str_detect(gbd_region,"Africa")) %>% 
  filter(Africa == T) %>%
  group_by(year,recipient_country) %>% 
  summarize(Other = sum(nch_other_dah_19),
            HumanResources = sum(nch_hss_hrh_dah_19),
            HealthSystemStrengthening = sum(nch_hss_other_dah_19),
            Immunization = sum(nch_cnv_dah_19),
            Nutrition = sum(nch_cnn_dah_19),
            PandemicPrep = sum(swap_hss_pp_dah_19)) %>% 
  select(year,recipient_country,Immunization, PandemicPrep) %>%
  ungroup() 

