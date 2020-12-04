library(tidyverse)
library(haven)
library(AER)
library(lubridate)
library(GGally)
library(lme4)
library(rstanarm)
library(bayesplot)

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

#Example display of some information
country = "Ghana"
testdata %>%
  filter(Country_Region==country) %>%
  ggplot() + aes(x=Exposure,y=log(Confirmed)) + geom_point(color="darkorchid") + 
  labs(title = "COVID over Time", subtitle=country) + theme_bw()

#Filter the data to remove repeated imputations on countries.
#Select variables, calculate the means for each one by country and by region (to reduce to one value)
#Then see how many rows they had in the original data.
filterdata <- read.csv("covid_data.csv")

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


## Model
# The model is a Linear Mixed Effects Model
# This will allow us to find coefficients for each variable, and adjust them for stuff.

#Makes a new data frame for LME model.
filteradj <- filterdata
filteradj[filteradj$countryorarea=="Cabo Verde",]$countryorarea <- "Cape Verde"
filteradj[filteradj$countryorarea=="Congo",]$countryorarea <- "Congo (Brazzaville)"
filteradj[filteradj$countryorarea=="Côte d'Ivoire",]$countryorarea <- "Cote d'Ivoire"
filteradj[filteradj$countryorarea=="Dem. Republic of the Congo",]$countryorarea <- "Congo (Kinshasa)"
filteradj[filteradj$countryorarea=="Swaziland",]$countryorarea <- "Eswatini"
filteradj[filteradj$countryorarea=="United Republic of Tanzania",]$countryorarea <- "Tanzania"

#Reads in population data and joins with the JHU data. 
popdata <- read.csv("Test/popdata.csv")
popdata$Location <- as.character(popdata$Location)
popdata[popdata$Location=="United Republic of Tanzania",]$Location <- "Tanzania"
lmedata <- left_join(testdata,filteradj[,-c(2:5)], by = c("Country_Region" = "countryorarea")) %>%
  left_join(popdata,by=c("Country_Region" = "Location")) %>% 
  mutate(ConfirmedScaled = Confirmed/PopTotal)

#Visualize the data for regions and non-regions

ggplot(lmedata) + aes(x=log(`New Cases`),fill=Country_Region) + 
  geom_histogram() + theme_bw() + facet_wrap(~Region) + theme(legend.position="none")

ggplot(lmedata) + aes(x=log(`New Cases`),fill=Country_Region) + 
  geom_histogram() + theme_bw() + facet_wrap(~Region) + theme(legend.position="none")

ggplot(lmedata) + aes(x=log(`New Cases`),fill=Country_Region) + 
  geom_histogram() + theme_bw() + facet_wrap(~Country_Region) + theme(legend.position="none")

ggplot(lmedata) + aes(x=log(`New Cases`),fill=Country_Region) + 
  geom_histogram() + theme_bw() + facet_wrap(~Country_Region) + theme(legend.position="none")


#Set up simple linear mixed effects model. This is used as a sample.
lmetest <- lmer(log(Confirmed) ~ Exposure + lnsdi + lnurban + lnp70p +
                  lnhhsn + lnihr2018 + lnsqualty + (Exposure|Country_Region),data=lmedata)
hist(resid(lmetest)) # Very skewed residuals.
qqnorm(resid(lmetest)); qqline(resid(lmetest)) #Heavy tailed residuals.
plot(fitted(lmetest),resid(lmetest)) #Weird nonlinear pattern in here.

#Try making cleaned-out data.
lmetest2 <- lmer(log(Confirmed) ~ Exposure + (Exposure|Country_Region) + (Exposure|Country_Region),data=lmedata)
hist(resid(lmetest2))
qqnorm(resid(lmetest2)); qqline(resid(lmetest2)) 
plot(fitted(lmetest2),resid(lmetest2)) #Weird nonlinear pattern in here.

#Set Up Data Frame for Forecasting
forecastdatainit <- lmedata %>% group_by(Country_Region) %>% 
  mutate(LastExposure=max(Exposure),LastDate=max(Date)) %>%
  filter(Exposure<=21) %>% 
  arrange(by=Country_Region) %>% 
  mutate(Date=LastDate+Exposure,Exposure=Exposure+LastExposure)

forecaststart <- forecastdatainit %>% select(Country_Region,Exposure,Date,Region,LastExposure,LastDate,
                                         lnsdi,lnurban,lnp70p,lnhhsn,lnihr2018,lnsqualty) %>% distinct()


#Both of these had convergence issues. I wanted to try working with STAN to adjust iterations.
#STAN uses Bayesian analysis to create a posterior distribution, which can be drawn from for predictoins.
#This means we can make predictive intervals as well based off of our data.
#It takes a while (~4 hour on my laptop) to generate.
#I wrote RDS files and can read them from the page.

#Model
#model <- stan_lmer(log(Confirmed) ~ Exposure + (Exposure|Country_Region),data=lmedata)
#saveRDS(model,"stan_fit.rds")
model <- readRDS("stan_fit.rds")

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
  geom_point(data=lmedata[lmedata$Country_Region==countrydisplay,],
             aes(y=log(Confirmed)),color="darkorchid") +
  geom_point(data=forecastestimate[forecastestimate$Country==countrydisplay,],
             aes(x=Exposure,y=Est)) +
  labs(title=countrydisplay)

# The patterns come out weird. My thought is that a lot of the non-case days
# and long exposure time may make the predictions worse.
# My thought was to try narrowing down the data and cleaning it.
# I went about this in three ways:
# 1) Remove non-zero datapoints for countries. Given a lot of the countries, my assumption is that there 
# will be new cases, and each day of non is non-recorded data.
# As of literally today this may not be true and I may be able to adjust it.
# 2) Remove countries that are weird after removing non-zero data. 
# Mayotte was this one. I also took out Cape Verde which had a point for some reason.
# 3) Pick the 21 most recent days to forecast using the current pattern.

lmetestdata <- lmedata%>%filter(`New Cases`>0) #Remove days with no new cases - each data point is a new COVID recording.
lmetestdata2 <- lmetestdata %>% filter(Country_Region!="Mayotte"&Country_Region!="Cape Verde") #Removed these countries due to their weird nature.
lmetestdata3 <- lmetestdata2 %>% group_by(Country_Region) %>% top_n(21,Exposure) #Take the 21 most recent days.

# New Model also includes more of the COVID data variables.
# This probably isn't the best orientation of where the variables are, and can probably be optimized further!

#First model has no "region" effect
model5name <- paste("stanmodel",Sys.Date(),".rds",sep="")
model5 <- stan_lmer(log(Confirmed) ~ Exposure + lnsdi + lnurban + lnp70p +
                  lnhhsn + lnihr2018 + lnsqualty + (Exposure|Country_Region),
                data=lmetestdata3)
saveRDS(model5,model5name)


# Make forecasts with this model.
forecastpredict <- posterior_predict(model5,forecaststart %>% na.omit())
forecaststan <- na.omit(forecaststart)
forecastestimate <- data.frame(Est=apply(forecastpredict,2,mean),Var=apply(forecastpredict,2,sd),
                               Country=forecaststan$Country_Region,Date=forecaststan$Date) %>% 
  mutate(Upper=Est+2*Var,Lower=Est-2*Var)

#Display forecasts!
countrydisplay <- "Egypt"
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

#Save for app.
write.csv(forecastestimate,"Test/forecastlmenoregion.csv") # Save it to be put into app.

###

#Model with regional effects.
model6name <- paste("stanmodelregional",Sys.Date(),".rds",sep="")
model6 <- lmer(log(Confirmed) ~ Exposure + lnsdi + lnurban + lnp70p +
                       lnhhsn + lnihr2018 + lnsqualty + (Exposure|Region) + (Exposure|Region:Country_Region),
                     data=lmetestdata3)
# saveRDS(model6,model6name)
model6 <- readRDS("stanmodeljul27reg.rds")

#Forecasts.
test = predict(model6,forecaststart%>%na.omit())

forecastestimate <- data.frame(Est=test,Country=forecaststan$Country_Region,Region=forecaststan$Region,Date=forecaststan$Date)

forecastpredict <- posterior_predict(model6,forecaststart %>% na.omit())
forecastestimate <- data.frame(Est=apply(forecastpredict,2,mean),Var=apply(forecastpredict,2,sd),
                               Country=forecaststan$Country_Region,Date=forecaststan$Date) %>% 
  mutate(Upper=Est+2*Var,Lower=Est-2*Var)

countrydisplay <- "Tanzania"
ggplot() + aes(x=Date) +
  geom_line(data=lmedata[lmedata$Country_Region==countrydisplay,],
            aes(y=Confirmed),color="darkorchid") +
  # geom_point(data=estimates[estimates$Country==countrydisplay,],
  #           aes(y=logEst),size=1.5) + 
  geom_line(data=forecastestimate[forecastestimate$Country==countrydisplay,],
            aes(x=Date,y=exp(Est))) +
  labs(title=countrydisplay)
write.csv(forecastestimate,"Test/forecastlmewithregion.csv") # Save it to be put into app.


### 

## A few other models I attempted.

nbtest <- glmer.nb(`New Cases` ~ Exposure + (Exposure|Country_Region),data=lmetestdata3)
qqnorm(resid(nbtest)); qqline(resid(nbtest))

nbtest <- glmer.nb(`New Cases` ~ Exposure + (Exposure|Country_Region/Region),data=lmedata%>%filter(`New Cases`>0))

nbtest <- glmer.nb(`New Cases` ~ Exposure + lnurban + lntraffic + lnasthma + (Exposure|Country_Region),data=lmetestdata)

poismodel <- stan_glmer(`New Cases` ~ Exposure + (Exposure|Country_Region),data=lmetestdata3,
                        family=poisson(link="log"))

lmemodel4 <- lmer(log(`New Cases`) ~ Exposure + (Exposure|Country_Region) + (Exposure|Region),data=lmecleaned)

lmemodel4 <- lmer(log(ConfirmedScaled) ~ Exposure + (Exposure|Country_Region) + (Exposure|Region),data=lmecleaned)
qqnorm(resid(lmemodel3)); qqline(resid(lmemodel3)) ## Residuals have heavy tails.
plot(fitted(lmemodel3),resid(lmemodel3))

























