source("DAHClean.R")

## Make graphics from paper we looked at.
# Add all finances for all countries in AFrica, convert units.
dahsum = dah_sub %>% group_by(year) %>% 
  summarize(Other = sum(nch_other_dah_19)/10^6,
            HumanResources = sum(nch_hss_hrh_dah_19)/10^6,
            HealthSystemStrengthening = sum(nch_hss_other_dah_19)/10^6,
            Immunization = sum(nch_cnv_dah_19)/10^6,
            Nutrition = sum(nch_cnn_dah_19)/10^6) %>% 
  pivot_longer(Other:Nutrition,names_to="Variable",values_to="Value") %>%
  ungroup()

# Make first plot.
levels = unique(dahsum$Variable)
dahsum$Variable = factor(dahsum$Variable,levels=levels,ordered=T)
p1 = ggplot(dahsum) + aes(x=year,y=Value,fill=Variable) +
  geom_bar(stat="identity") + theme_bw() + 
  xlab("") + 
  ylab("Billions of 2019 US Dollars") +
  scale_fill_brewer(palette="Purples") +
  labs(title = "Development assistance for newborn
and child health by program area",
       subtitle="1990-2019") + 
  theme(axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_line(color="white"),
        panel.grid.major = element_line(color="gray"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = c(0.15,0.79),
        legend.direction = "vertical",
        legend.box = "horizontal")

# Repeat the above for maternal data.
dahplot2 = dah %>% filter(elim_ch!=1) %>% 
  select(year,
         source,
         channel,
         recipient_country, 
         gbd_region,elim_ch,
         prelim_est,
         rmh_other_dah_19,
         rmh_hss_hrh_dah_19,
         rmh_hss_other_dah_19,
         rmh_mh_dah_19,
         rmh_fp_dah_19) 
dahplot2$rmh_other_dah_19 = as.numeric(dahplot2$rmh_other_dah_19)
dahplot2$rmh_hss_hrh_dah_19 = as.numeric(dahplot2$rmh_hss_hrh_dah_19)
dahplot2$rmh_hss_other_dah_19 = as.numeric(dahplot2$rmh_hss_other_dah_19)
dahplot2$rmh_mh_dah_19 = as.numeric(dahplot2$rmh_mh_dah_19)
dahplot2$rmh_fp_dah_19 = as.numeric(dahplot2$rmh_fp_dah_19)
dahplot2[is.na(dahplot2)] = 0

dahplot2 %<>% group_by(year) %>%
  summarize(Other = sum( rmh_other_dah_19)/10^6,
            HumanResources = sum(rmh_hss_hrh_dah_19)/10^6,
            HealthSystemStrengthening = sum(rmh_hss_other_dah_19)/10^6,
            MaternalHealth = sum(rmh_mh_dah_19)/10^6,
            FamilyPlanning = sum(rmh_fp_dah_19)/10^6
  ) %>% 
  pivot_longer(Other:FamilyPlanning,names_to="Variable",values_to="Value") %>%
  ungroup()

# Make second plot.
levels2 = unique(dahplot2$Variable)
dahplot2$Variable = factor(dahplot2$Variable, levels=levels2,ordered=T)
p2 = ggplot(dahplot2) + aes(x=year,y=Value,fill=Variable) +
  geom_bar(stat="identity") + 
  xlab("") + 
  ylab("Billions of 2019 US Dollars") +
  scale_fill_brewer(palette="Reds") + 
  labs(title = "Development assistance for reproductive
and maternal health by program area",
       subtitle="1990-2019") + 
  theme(axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_line(color="white"),
        panel.grid.major = element_line(color="gray"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        
        legend.position = c(0.15,0.79),
        legend.direction = "vertical",
        legend.box = "horizontal")

# Make  plots!
grid.arrange(p1,p2,nrow=1)
