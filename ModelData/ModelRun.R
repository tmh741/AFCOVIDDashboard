source("InstallPackages.R")

modeldata = read.csv("ModelData/modeldata.csv")
forecaststart = read.csv("ModelData/forecaststart.csv")

#First model has no "region" effect
model5name <- paste("ModelData/stanmodel",Sys.Date(),".rds",sep="")
model5 <- stan_lmer(log(Confirmed) ~ Exposure + lnsdi + lnurban + lnp70p +
                      lnhhsn + lnihr2018 + lnsqualty + (Exposure|Country_Region),
                    data=modeldata)
saveRDS(model5,model5name)

# Make forecasts with this model.
forecastpredict <- posterior_predict(model5,forecaststart %>% na.omit())
forecaststan <- na.omit(forecaststart)
forecastestimate <- data.frame(Est=apply(forecastpredict,2,mean),Var=apply(forecastpredict,2,sd),
                               Country=forecaststan$Country_Region,Date=forecaststan$Date) %>% 
  mutate(Upper=Est+2*Var,Lower=Est-2*Var)

#Save for app.
write.csv(forecastestimate,"ModelData/forecastestimates.csv")