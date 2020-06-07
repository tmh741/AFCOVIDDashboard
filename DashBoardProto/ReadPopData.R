popdata <- WPP2019_TotalPopulationBySex %>% filter(Time==2020) %>% 
  select(Location,PopTotal) %>% distinct()

write.csv(popdata ,file="popdata.csv",row.names = F)
