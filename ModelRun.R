# ModelRun.R runs the model in STAN.
source("InstallPackages.R")

# Read in data made with ModelFiles.R
modeldata = read.csv("ModelData/modeldata.csv")
forecaststart = read.csv("ModelData/forecaststart.csv")

# Set up model. We're using a linear mixed effects model.
# It accounts for country-level differences and can also consider different impacts of time.
# Ideal for the situation and works well.
# We're also using RStan to run it to include uncertainty estimations.
# If you want to reduce runtimes, you can use lmer without STAN but will lose the uncertainty.
# It usually takes ~ 1 hour to run.
model5name <- paste("ModelData/stanmodel",Sys.Date(),".rds",sep="")
model5 <- stan_lmer(log(Confirmed) ~ Exposure + lnsdi + lnurban + lnp70p +
                      lnhhsn + lnihr2018 + lnsqualty + (Exposure|Country_Region),
                    data=modeldata)
saveRDS(model5,model5name)

# Make forecasts with this model.
# Make calculations for display in App.
forecastpredict <- posterior_predict(model5,forecaststart %>% na.omit())
forecaststan <- na.omit(forecaststart)
forecastestimate <- data.frame(Est=apply(forecastpredict,2,mean),Var=apply(forecastpredict,2,sd),
                               Country=forecaststan$Country_Region,Date=forecaststan$Date) %>% 
  mutate(Upper=Est+2*Var,Lower=Est-2*Var)

#Save for app.
write.csv(forecastestimate,"ModelData/forecastestimates.csv")