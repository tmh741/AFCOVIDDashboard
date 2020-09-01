#Read covid_dta.
modeldata <- read_dta("covid_data.dta")

filterdata <- modeldata %>% select(countryorarea, region,lnrchange,lncaseload_lastobs,lnexpo,
                                   lnsdi,lnurban,lnp70p,lnhhsn,lnihr2018,lnsqualty,lnasthma,lnhiv,lntraffic)
write.csv(filterdata,"covid_data.csv",row.names=F)