library(tidyverse)
library(haven)
library(AER)
library(lubridate)
library(GGally)
library(lme4)
library(rstanarm)
library(bayesplot)

#Read data for model.
modeldata <- read_dta("covid_data.dta")

#Read COVID data. 
# "Test" is the folder that has the app. Will change it soon! 
testdata <- read.csv("Test/testfile.csv")[,-1]
testdata$Date <- mdy(testdata$Date) #Ensures dates are dates and not characters.
testdata <- testdata %>% group_by(Country_Region) %>% 
  arrange(Date) %>% # Sorts data.
  mutate(`New Cases` = Confirmed - lag(Confirmed, default=0),# Calculates metrics.
         `New Deaths` = Deaths - lag(Deaths, default = 0),
         `New Recoveries` = Recovered - lag(Recovered, default=0),
         DaysSince = Date - ymd("2020-03-31"),
         Exposure = Date - first(Date) + 1) %>%
  ungroup()

country = "Ghana"
testdata %>%
  filter(Country_Region==country) %>%
  ggplot() + aes(x=Exposure,y=log(Confirmed)) + geom_point(color="darkorchid") + 
  labs(title = "COVID over Time", subtitle=country) + theme_bw()

#Filter the data to remove repeated imputations on countries.
#Select variables, calculate the means for each one by country and by region (to reduce to one value)
#Then see how many rows they had in the original data.
filterdata <- modeldata %>% select(countryorarea, region,lnrchange,lncaseload_lastobs,lnexpo,
                                   lnsdi,lnurban,lnp70p,lnhhsn,lnihr2018,lnsqualty,lnasthma,lnhiv,lntraffic)

filterdata <- filterdata %>% group_by(countryorarea,region) %>% 
  summarize(lnrchange=mean(lnrchange,na.rm=T),
            lncaseload_lastobs=mean(lncaseload_lastobs,na.rm=T),
            lnexpo=mean(lnexpo,na.rm=T),
            lnsdi=mean(lnsdi,na.rm=T),
            lnurban=mean(lnurban,na.rm=T),
            lnp70p=mean(lnp70p,na.rm=T),
            lnhhsn=mean(lnhhsn,na.rm=T),
            lnihr2018=mean(lnihr2018,na.rm=T),
            lnsqualty=mean(lnsqualty,na.rm=T),
            lnhiv=mean(lnhiv,na.rm=T),
            lnasthma=mean(lnasthma,na.rm=T),
            lntraffic=mean(lntraffic,na.rm=T),
            nrows = n())
filterdata[is.na(filterdata)] <- NA

#Plot distribution of variables. Pivot_longer puts all variables into one column by type.
filterdata %>% group_by(countryorarea,region) %>%
  pivot_longer(
    cols=-c(countryorarea,region),
    names_to="Variable",values_to="Value"
  ) %>% filter(Variable != "nrows")  %>%
  ggplot() + geom_histogram(aes(x=Value,fill=Variable)) + facet_wrap(~Variable)

## IV Regression Models
#Plot initial model and look at residuals
model <- ivreg(lncaseload_lastobs ~ lnrchange + lnexpo + lnsdi + lnurban + lnp70p +
                 lnhhsn + lnihr2018 + lnsqualty + lnasthma + lnhiv  | 
                 lnexpo + lnsdi + lnurban + lnp70p +
                 lnhhsn + lnihr2018 + lnsqualty + lnasthma + lnhiv + lntraffic,data= modeldata)

summary(model) #Coefficients seem close to the model in STATA.
plot(fitted(model),resid(model)) #Residuals seem weird. Most likely due to imputations.
qqnorm(resid(model)); qqline(resid(model)) ##QQplot also looks strange.

#Use on filterdata instead of modeldata - remove the repeated rows.
model2 <- ivreg(lncaseload_lastobs ~ lnrchange + lnexpo + lnsdi + lnurban + lnp70p +
                 lnhhsn + lnihr2018 + lnsqualty + lnasthma + lnhiv  | 
                 lnexpo + lnsdi + lnurban + lnp70p +
                 lnhhsn + lnihr2018 + lnsqualty + lnasthma + lnhiv + lntraffic,data= filterdata)
summary(model2) # Coefficients are much wider - less confidence.
plot(fitted(model2),resid(model2)) # Residuals show correlation.
qqnorm(resid(model2)); qqline(resid(model2))

#Remove lnrchange and see if correlation reduces in residuals.
model3 <- ivreg(lncaseload_lastobs ~ lnexpo + lnsdi + lnurban + lnp70p +
                 lnhhsn + lnihr2018 + lnsqualty + lnasthma + lnhiv  | 
                 lnexpo + lnsdi + lnurban + lnp70p +
                 lnhhsn + lnihr2018 + lnsqualty + lnasthma + lnhiv + lntraffic,data= filterdata)
plot(fitted(model3),resid(model3)) ## Less correlation - more random.
qqnorm(resid(model3)); qqline(resid(model3))  # Closer to normal
summary(model3) # lnexpo, lnurban, and lnasthma seem to be the variables that correlate the most.

# Set variables based on ranges. prange is the coefficient by itself,
#urange and lrange are the upper and lower ranges of the confidence interval.
#Maybe change to prediction interval?
prange = summary(model)$coefficients[,1]
lrange = summary(model)$coefficients[,1] - 
  2*summary(model)$coefficients[,2]
urange  = summary(model)$coefficients[,1] + 
  2*summary(model)$coefficients[,2]

ranges <- data.frame(prange=prange,urange=urange,lrange=lrange)
write.csv(ranges,"ranges.csv")

#Reverse calculate predictions. predict() could probably be used too but I had trouble
#getting it to work with ranges.
#If using model3, modify coefficients and numbers.
pred= data.frame(matrix(nrow=nrow(filterdata),ncol=95))
upper= data.frame(matrix(nrow=nrow(filterdata),ncol=95))
lower= data.frame(matrix(nrow=nrow(filterdata),ncol=95))
for (i in 1:95) {
  #This is hard to read but this takes the coefficient for prange and multiplies it
  #by each value in the other column to recalculate.
  #Again predict() would be much easier to read here.
  #Used filterdata to produce one value per country.
  pred[,i] = exp(prange[1] + (prange[2]* filterdata$lnrchange) + 
                   (prange[3])*log(exp(filterdata$lnexpo) + i) +
                   prange[4]*filterdata$lnsdi + prange[5]*filterdata$lnurban + 
                   prange[6]*filterdata$lnp70p + prange[7]*filterdata$lnhhsn + 
                   prange[8]*filterdata$lnihr2018 + prange[9]*filterdata$lnsqualty +
                   prange[10]*filterdata$lnasthma + prange[11]*filterdata$lnhiv)
  colnames(pred)[i] <- paste("forecast",i,sep=".")

  #Same as above but with the upper ranges of the coefficient.
  upper[,i] = exp(urange[1] + (urange[2]* filterdata$lnrchange) + 
                    (urange[3])*log(exp(filterdata$lnexpo) + i) + 
                    urange[4]*filterdata$lnsdi + urange[5]*filterdata$lnurban + 
                    urange[6]*filterdata$lnp70p + urange[7]*filterdata$lnhhsn + 
                    urange[8]*filterdata$lnihr2018 + urange[9]*filterdata$lnsqualty + 
                    urange[10]*filterdata$lnasthma + urange[11]*filterdata$lnhiv)
  colnames(upper)[i] <- paste("forecast",i,sep=".")

  #Same as above but with the lower ranges of the coefficient.
  lower[,i] = exp(lrange[1] + (lrange[2]* filterdata$lnrchange) + 
                     (lrange[3])*log(exp(filterdata$lnexpo) + i) +
    lrange[4]*filterdata$lnsdi + lrange[5]*filterdata$lnurban + lrange[6]*filterdata$lnp70p +
    lrange[7]*filterdata$lnhhsn + lrange[8]*filterdata$lnihr2018 + lrange[9]*filterdata$lnsqualty +
    lrange[10]*filterdata$lnasthma + lrange[11]*filterdata$lnhiv)
  colnames(lower)[i] <- paste("forecast",i,sep=".")

}
# Put in countries.
pred$country <- filterdata$countryorarea
lower$country <- filterdata$countryorarea
upper$country <- filterdata$countryorarea

#Forecast output is weird so this is
preddf <- pred %>% group_by(country) %>% pivot_longer(
  cols=forecast.1:forecast.95,
  names_to="day",
  values_to="prediction"
) %>% mutate(Day=1:95)

upperdf <- upper %>% group_by(country) %>% pivot_longer(
  cols=forecast.1:forecast.95,
  names_to="day",
  values_to="upper"
) %>% mutate(Day=1:95)

lowerdf <- lower %>% group_by(country) %>% pivot_longer(
  cols=forecast.1:forecast.95,
  names_to="day",
  values_to="lower"
) %>% mutate(Day=1:95)

# Join data together.
predictiondata <- preddf %>%
  left_join(upperdf,by=c("country","day","Day")) %>% 
  left_join(lowerdf,by=c("country","day","Day")) %>%
  select(-day) %>%
         mutate(Date = ymd("2020-03-31") + Day)

#Clean country names to match the COVID data.
predictiondata[predictiondata$country=="Cabo Verde",]$country <- "Cape Verde"
predictiondata[predictiondata$country=="Congo",]$country <- "Congo (Brazzaville)"
predictiondata[predictiondata$country=="Côte d'Ivoire",]$country <- "Cote d'Ivoire"
predictiondata[predictiondata$country=="Dem. Republic of the Congo",]$country <- "Congo (Kinshasa)"
predictiondata[predictiondata$country=="Swaziland",]$country <- "Eswatini"
predictiondata[predictiondata$country=="United Republic of Tanzania",]$country <- "Tanzania"
predictiondata <- predictiondata %>% 
  subset(predictiondata$country %in% unique(testdata$Country_Region))

#Put it all in one data frame.
afdata <- testdata %>%
  full_join(predictiondata,by=c("Country_Region"="country","Date"))


#Save filterdata and modelprojections.
#I found saving the files rather than generating them in code makes the app run faster.
#This may be the wrong way to do it though!
displaydata <- modeldata %>%
  select(countryorarea,region,asthma_standardised,hiv_standardised,
         airtraffic,service_quality2018,urbanization) %>% distinct()
displaydata[displaydata$countryorarea=="Cabo Verde",]$countryorarea <- "Cape Verde"
displaydata[displaydata$countryorarea=="Congo",]$countryorarea <- "Congo (Brazzaville)"
displaydata[displaydata$countryorarea=="Côte d'Ivoire",]$countryorarea <- "Cote d'Ivoire"
displaydata[displaydata$countryorarea=="Dem. Republic of the Congo",]$countryorarea <- "Congo (Kinshasa)"
displaydata[displaydata$countryorarea=="Swaziland",]$countryorarea <- "Eswatini"
displaydata[displaydata$countryorarea=="United Republic of Tanzania",]$countryorarea <- "Tanzania"
displaydata <- displaydata %>% subset(displaydata$countryorarea %in% unique(testdata$Country_Region))

write.csv(displaydata,file="Test/filtereddata.csv",na="NA",row.names=F)
write.csv(afdata,file="Test/modelprojections.csv",na="NA",row.names=F)


# Code for plot
modelcolors <- c("Projection Range" = "skyblue", "Model Projection" = "black", "Confirmed Cases" = "darkorchid")
afdata %>%
  filter(Country_Region=="South Africa") %>%
  ggplot() + aes(x=Date) + 
  geom_ribbon(aes(ymin=lower,ymax=upper, fill="Projection Range"),alpha=0.3) +
  geom_point(aes(y=Confirmed,color="Confirmed Cases")) + 
  geom_line(aes(y=prediction,color="Model Projection"), size = 1.5) +
#  geom_line(aes(y=upper,color="Projection Range")) +
# geom_line(aes(y=lower,color="Projection Range")) + 
  theme_bw() +
  ylab("Confirmed Cases") + 
  ggtitle("COVID-19 Model Projections",subtitle = "South Africa") +
  scale_color_manual(values=modelcolors) + 
  scale_fill_manual(values=modelcolors) +
  labs(color = "Legend") +
  guides(fill=F)
##Log scale
displaycountry <- unique(afdata$Country_Region)[1]
afdata %>%
  filter(Country_Region==displaycountry) %>%
  ggplot() + aes(x=Date) + 
  geom_ribbon(aes(ymin=log(lower),ymax=log(upper), fill="Projection Range"),alpha=0.3) +
  geom_point(aes(y=log(Confirmed),color="Confirmed Cases")) + 
  geom_line(aes(y=log(prediction),color="Model Projection"), size = 1.5) +
  #  geom_line(aes(y=upper,color="Projection Range")) +
  # geom_line(aes(y=lower,color="Projection Range")) + 
  theme_bw() +
  ylab("Confirmed Cases") + 
  ggtitle("COVID-19 Model Projections",subtitle = displaycountry) +
  scale_color_manual(values=modelcolors) + 
  scale_fill_manual(values=modelcolors) +
  labs(color = "Legend") +
  guides(fill=F)

## Linear Mixed Effects Model
## Idea: the IV Regression takes time and initial rate of change and uses
## instrumental varaibles that are unique and unchanging to each country.
## The goal is to capture those as random effects.

#Take new data.
filteradj <- filterdata
#Clean for country names to match the COVID data.
filteradj[filteradj$countryorarea=="Cabo Verde",]$countryorarea <- "Cape Verde"
filteradj[filteradj$countryorarea=="Congo",]$countryorarea <- "Congo (Brazzaville)"
filteradj[filteradj$countryorarea=="Côte d'Ivoire",]$countryorarea <- "Cote d'Ivoire"
filteradj[filteradj$countryorarea=="Dem. Republic of the Congo",]$countryorarea <- "Congo (Kinshasa)"
filteradj[filteradj$countryorarea=="Swaziland",]$countryorarea <- "Eswatini"
filteradj[filteradj$countryorarea=="United Republic of Tanzania",]$countryorarea <- "Tanzania"

popdata <- read.csv("Test/popdata.csv")
popdata$Location <- as.character(popdata$Location)
popdata[popdata$Location=="United Republic of Tanzania",]$Location <- "Tanzania"


#Left join testdata and the country variables to set up model.
lmedata <- left_join(testdata,filteradj[,-c(2:5)], by = c("Country_Region" = "countryorarea")) %>%
  left_join(popdata,by=c("Country_Region" = "Location")) %>% 
  mutate(ConfirmedScaled = Confirmed/PopTotal)

lmedata <- lmedata %>% mutate(lnexpo= log(as.numeric(Exposure)),lncaseload_lastobs=log(Confirmed))

lmetestdata <- lmedata%>%filter(`New Cases`>0)
lmetestdata2 <- lmetestdata %>% filter(Country_Region!="Mayotte"&Country_Region!="Cape Verde")
lmetestdata3 <- lmetestdata2 %>% group_by(Country_Region) %>% top_n(21,Exposure)


#Set up simple linear mixed effects model.
#Convergence not reached.
lmetest <- lmer(log(Confirmed) ~ Exposure + (Exposure|Country_Region),data=lmedata)
qqnorm(resid(lmetest)); qqline(resid(lmetest))
plot(fitted(lmetest),resid(lmetest)) #Weird nonlinear pattern in here too.

#Plot predictions for each country and see how they are.
countrydisplay <- unique(lmedata$Country_Region)[1] 
lmedata %>% filter(Country_Region==countrydisplay) %>% 
  ggplot() + aes(x=Date) + geom_point(aes(y=Confirmed),color="darkorchid") + 
  geom_line(aes(y=exp(predict(lmetest,lmedata[lmedata$Country_Region==countrydisplay,]))), 
             linetype="dotted")

#Try to fix convergence with using residuals in STAN.
#STAN uses Bayesian analysis to create a posterior distribution, which can be drawn from for predictoins.
#This means we can make predictive intervals as well based off of our data.
#It takes a while (~an hour on my laptop) to generate so I drew them from rds files.
#The rds files are large so not all of them may be on Github.

#model <- stan_lmer(log(Confirmed) ~ Exposure + (Exposure|Country_Region),data=lmedata)
#saveRDS(model,"stan_fit.rds")
model <- readRDS("stan_fit.rds")

#Draw from posterior and create estimates for variables.
posterior <- posterior_predict(model)
estimates <- data.frame(logEst=apply(posterior,2,mean),logVar=apply(posterior,2,sd),
                        Country = lmedata$Country_Region,Confirmed=lmedata$Confirmed,
                        Date=lmedata$Date,Exposure=lmedata$Exposure) %>%
  mutate(Estimate = exp(logEst),Upper = logEst + 2*logVar,Lower=logEst-2*logVar,
         logConfirmed = log(Confirmed))

#Create forecasts.
forecastdata <- lmedata %>% group_by(Country_Region) %>% 
  mutate(LastExposure=max(Exposure),LastDate=max(Date)) %>%
  filter(Exposure<=21) %>% select(Country_Region,Exposure,LastDate,LastExposure) %>%
  arrange(by=Country_Region) %>% 
  mutate(Date=LastDate+Exposure,Exposure=Exposure+LastExposure)


forecastpredict <- posterior_predict(model,forecastdata)
forecastestimate <- data.frame(Est=apply(forecastpredict,2,mean),Var=apply(forecastpredict,2,sd),
                               Country=forecastdata$Country_Region,Date=forecastdata$Date,
                               Exposure=forecastdata$Exposure) %>%
  mutate(Upper=Est+2*Var,Lower=Est-2*Var)

# Plot forecasts for each country.
countrydisplay <- unique(lmedata$Country_Region)[1]
ggplot() + aes(x=Exposure) +
  geom_ribbon(data=forecastestimate[forecastestimate$Country==countrydisplay,],
              aes(ymin=Lower,ymax=Upper),fill="skyblue",alpha=0.3) +
  geom_point(data=estimates[estimates$Country==countrydisplay,],
             aes(y=log(Confirmed)),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_point(data=forecastestimate[forecastestimate$Country==countrydisplay,],
            aes(x=Exposure,y=Est)) +
  labs(title=countrydisplay)
  

## What I realized: the model was capturing many linear patterns.
## Additionally - many countries, when visualizing log(Cases) ~ Exposure,
## showed many linear trends with break points.
## I wanted to test how well the model did using the most recent days.

#Filter out the most recent 21 days for each country.
lmecleaned <- lmedata %>% group_by(Country_Region) %>% 
  filter(`New Cases`!=0) %>% 
  top_n(21,Exposure)
## Remove some countries
lmerecent <- lmedata %>% group_by(Country_Region) %>% top_n(21,Exposure)
lmecleaned <- lmerecent %>% filter(Country_Region!="Cape Verde"&Country_Region!="Western Sahara"&
                                     Country_Region!="Mayotte"&Country_Region!="Equatorial Guinea"&
                                     `New Cases`>0)
lmemodel2 <- lmer(log(Confirmed) ~ Exposure + (Exposure|Country_Region),data=lmerecent)

lmemodel3 <- lmer(log(Confirmed) ~ Exposure + (Exposure|Country_Region), data=lmetestdata3)
summary(lmemodel3)
qqnorm(resid(lmemodel3)); qqline(resid(lmemodel3)) ## Residuals have heavy tails.
plot(fitted(lmemodel3),resid(lmemodel3))

forecastdata$Predict <- exp(predict(lmemodel3,forecastdata,allow.new.levels=T))

countrydisplay="Nigeria"
ggplot() + aes(x=Date) +
  geom_line(data=testdata[testdata$Country_Region==countrydisplay,],
            aes(y=Confirmed),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_line(data=forecastdata[forecastdata$Country_Region==countrydisplay,],
            aes(x=Date,y=Predict)) +
  labs(title=countrydisplay)


forecaststart <- forecastdata %>% select(Country_Region,Exposure.x,Date.x,Region.x,LastExposure,LastDate,
                                         lnsdi,lnurban,lnp70p,lnhhsn,lnihr2018,lnsqualty) %>% distinct()

colnames(forecaststart)[2] <- "Exposure"
colnames(forecaststart)[3] <- "Date"
colnames(forecaststart)[4] <- "Region"



lmemodel5 <- lmer(log(Confirmed) ~ Exposure + lnsdi + lnurban + lnp70p +
                 lnhhsn + lnihr2018 + lnsqualty + (Exposure|Country_Region),
               data=lmetestdata3)
forecaststart$Predict <- exp(predict(model5,forecaststart,allow.new.levels=T))

countrydisplay="Tanzania"
ggplot() + aes(x=Date) +
  geom_line(data=testdata[testdata$Country_Region==countrydisplay,],
            aes(y=Confirmed),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_line(data=forecaststart[forecaststart$Country_Region==countrydisplay,],
            aes(x=Date,y=Predict)) +
  labs(title=countrydisplay)


model5 <- stan_lmer(log(Confirmed) ~ Exposure + lnsdi + lnurban + lnp70p +
                 lnhhsn + lnihr2018 + lnsqualty + (Exposure|Country_Region),
               data=lmetestdata3)
saveRDS(model5,"stanmodeljul27.rds")

forecastpredict <- posterior_predict(model5,forecaststart %>% na.omit())
forecaststan <- na.omit(forecaststart)

forecastestimate <- data.frame(Est=apply(forecastpredict,2,mean),Var=apply(forecastpredict,2,sd),
                               Country=forecaststan$Country_Region,Date=forecaststan$Date) %>% 
  mutate(Upper=Est+2*Var,Lower=Est-2*Var)

countrydisplay <- "Tanzania"
ggplot() + aes(x=Date) +
  geom_ribbon(data=forecastestimate[forecastestimate$Country==countrydisplay,],
              aes(ymin=exp(Lower),ymax=exp(Upper)),fill="skyblue",alpha=0.3) +
  geom_line(data=lmedata[lmedata$Country_Region==countrydisplay,],
            aes(y=Confirmed),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_line(data=forecastestimate[forecastestimate$Country==countrydisplay,],
            aes(x=Date,y=exp(Est))) +
  labs(title=countrydisplay)

write.csv(forecastestimate,"Test/forecastlmenoregion.csv") # Save it to be put into app.

###


model6 <- stan_lmer(log(Confirmed) ~ Exposure + lnsdi + lnurban + lnp70p +
                      lnhhsn + lnihr2018 + lnsqualty + (Exposure|Region) + (Exposure|Region:Country_Region),
                    data=lmetestdata3)
saveRDS(model6,"stanmodeljul27reg.rds")
forecastpredict <- posterior_predict(model6,forecaststart %>% na.omit())
forecaststan <- na.omit(forecaststart)

forecastestimate <- data.frame(Est=apply(forecastpredict,2,mean),Var=apply(forecastpredict,2,sd),
                               Country=forecaststan$Country_Region,Date=forecaststan$Date) %>% 
  mutate(Upper=Est+2*Var,Lower=Est-2*Var)

countrydisplay <- "Seychelles"
ggplot() + aes(x=Date) +
  geom_ribbon(data=forecastestimate[forecastestimate$Country==countrydisplay,],
              aes(ymin=exp(Lower),ymax=exp(Upper)),fill="skyblue",alpha=0.3) +
  geom_line(data=lmedata[lmedata$Country_Region==countrydisplay,],
            aes(y=Confirmed),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_line(data=forecastestimate[forecastestimate$Country==countrydisplay,],
            aes(x=Date,y=exp(Est))) +
  labs(title=countrydisplay)
write.csv(forecastestimate,"Test/forecastlmewithregion.csv") # Save it to be put into app.


###

nbtest <- glmer.nb(`New Cases` ~ Exposure + (Exposure|Country_Region),data=lmetestdata3)
qqnorm(resid(nbtest)); qqline(resid(nbtest))

nbtest <- glmer.nb(`New Cases` ~ Exposure + (Exposure|Country_Region/Region),data=lmedata%>%filter(`New Cases`>0))

nbtest <- glmer.nb(`New Cases` ~ Exposure + lnurban + lntraffic + lnasthma + (Exposure|Country_Region),data=lmetestdata)

forecastdata <- lmedata %>% group_by(Country_Region) %>% 
  mutate(LastExposure=max(Exposure),LastDate=max(Date),LastConfirmed=max(Confirmed)) %>%
  filter(Exposure<=21) %>% select(Country_Region,Exposure,Region,LastDate,LastExposure,LastConfirmed) %>%
  arrange(by=Country_Region) %>% 
  mutate(Date=LastDate+Exposure,Exposure=Exposure+LastExposure) %>% na.omit()

lmetest <- lmer(log(`New Cases`) ~ Exposure + (Exposure|Country_Region),data=lmetestdata2)
# model5 <- stan_lmer(log(`New Cases`) ~ Exposure + (Exposure|Country_Region),data=lmetestdata2)
forecastdata$Predict <- exp(predict(lmetest,forecastdata))
forecastdata <- forecastdata %>% group_by(Country_Region) %>% 
  mutate(Cumulative = cumsum(Predict),
         CumScaled = Cumulative + LastConfirmed)

countrydisplay="Egypt"
ggplot() + aes(x=Date) +
  geom_line(data=testdata[testdata$Country_Region==countrydisplay,],
            aes(y=Confirmed),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_line(data=forecastdata[forecastdata$Country_Region==countrydisplay,],
            aes(x=Date,y=CumScaled)) +
  labs(title=countrydisplay)
write.csv(forecastdata,"jul27forecast.csv",row.names=F)



residplotdata <- data.frame(fit = fitted(lmetest),resid=resid(lmetest),Country=lmetestdata2$Country_Region)
ggplot(residplotdata) + aes(x=fit,y=resid,label=Country) + geom_text()


poismodel <- stan_glmer(`New Cases` ~ Exposure + (Exposure|Country_Region),data=lmetestdata3,
                  family=poisson(link="log"))


countrydisplay="South Africa"
ggplot() + aes(x=Date) +
  geom_line(data=testdata[testdata$Country_Region==countrydisplay,],
            aes(y=Confirmed),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_line(data=forecastdata[forecastdata$Country_Region==countrydisplay,],
            aes(x=Date,y=Predict)) +
  labs(title=countrydisplay)



nbtest <- glmer.nb(`New Cases` ~ Exposure + (Exposure|Country_Region),data=lmetestdata2)






lmemodel4 <- lmer(log(`New Cases`) ~ Exposure + (Exposure|Country_Region) + (Exposure|Region),data=lmecleaned)



lmemodel4 <- lmer(log(ConfirmedScaled) ~ Exposure + (Exposure|Country_Region) + (Exposure|Region),data=lmecleaned)
qqnorm(resid(lmemodel3)); qqline(resid(lmemodel3)) ## Residuals have heavy tails.
plot(fitted(lmemodel3),resid(lmemodel3))

forecastdata <- lmedata %>% group_by(Country_Region) %>% 
  mutate(LastExposure=max(Exposure),LastDate=max(Date)) %>%
  filter(Exposure<=21) %>% select(Country_Region,Exposure,Region,LastDate,LastExposure) %>%
  arrange(by=Country_Region) %>% 
  mutate(Date=LastDate+Exposure,Exposure=Exposure+LastExposure) %>% na.omit() %>% 
  left_join(popdata,by=c("Country_Region" = "Location"))

forecastdata$Forecasts <- predict(lmemodel4,forecastdata,allow.new.levels=T)

displaycountry = "Tanzania"
ggplot() + 
  geom_point(data=lmedata[lmedata$Country_Region==displaycountry,],
             aes(x=Date,y=Confirmed),color="darkorchid") +
  geom_line(data=forecastdata[forecastdata$Country_Region==displaycountry,],
            aes(x=Date,y=exp(Forecasts)*PopTotal))


ggplot(lmecleaned) + aes(x=log(`New Cases`),fill=Country_Region) + 
  geom_histogram() + theme_bw() + facet_wrap(~Region) + theme(legend.position="none")

ggplot(lmecleaned) + aes(x=log(`New Cases`),fill=Country_Region) + 
  geom_histogram() + theme_bw() + facet_wrap(~Country_Region) + theme(legend.position="none")


#Again we're dealing with con
summary(lmemodel2)
qqnorm(resid(lmemodel2)); qqline(resid(lmemodel2)) ## Residuals have heavy tails.

test <- qqnorm(resid(lmemodel2),plot.it=F)

ggplot() + aes(x= test$x, y= test$y,color=lmecleaned$Region,label=lmecleaned$Country_Region) + geom_text() 

ggplot(lmecleaned) + aes(x=fitted(lmemodel2),y=resid(lmemodel2),label=lmecleaned$Country_Region,
                        color=lmecleaned$Region) + geom_text()

# model3 <- stan_lmer(log(Confirmed) ~ Exposure + (Exposure|Country_Region) + (Exposure|Region),data=lmecleaned)
# saveRDS(model3,"stan_model717.rds")
model3 <- readRDS("stan_model717.rds")
forecastdata <- lmedata %>% group_by(Country_Region) %>% 
  mutate(LastExposure=max(Exposure),LastDate=max(Date)) %>%
  filter(Exposure<=21) %>% select(Country_Region,Exposure,Region,LastDate,LastExposure) %>%
  arrange(by=Country_Region) %>% 
  mutate(Date=LastDate+Exposure,Exposure=Exposure+LastExposure) %>% na.omit()

forecastpredict <- posterior_predict(model3,forecastdata)

forecastestimate <- data.frame(Est=apply(forecastpredict,2,mean),Var=apply(forecastpredict,2,sd),
                               Country=forecastdata$Country_Region,Date=forecastdata$Date) %>% 
  mutate(Upper=Est+2*Var,Lower=Est-2*Var)

countrydisplay <- unique(lmedata$Country_Region)[]
countrydisplay <- "Seychelles"
ggplot() + aes(x=Date) +
   geom_ribbon(data=forecastestimate[forecastestimate$Country==countrydisplay,],
               aes(ymin=exp(Lower),ymax=exp(Upper)),fill="skyblue",alpha=0.3) +
  geom_line(data=testdata[testdata$Country_Region==countrydisplay,],
             aes(y=Confirmed),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_line(data=forecastestimate[forecastestimate$Country==countrydisplay,],
             aes(x=Date,y=exp(Est))) +
  labs(title=countrydisplay)


#Save file to be put into app
write.csv(forecastestimate,"Test/forecastlmenoregion.csv") # Save it to be put into app.


  
