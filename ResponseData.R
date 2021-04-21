source("ReadData.R")

responses <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
responses$Date <- gsub("(.{6})(.*)","\\1/\\2",responses$Date)
responses$Date <- gsub("(.{4})(.*)","\\1/\\2",responses$Date)
responses$Date <- ymd(responses$Date)

countrylist2 <- unique(responses$CountryName)
setdiff(countrylist,countrylist2)
setdiff(unique(testdata$Country_Region),countrylist2)

testdata <- read.csv("Test/testfile.csv")[,-1]
testdata$Date <- mdy(testdata$Date)
testdata$Country_Region <- as.character(testdata$Country_Region)
testdata[testdata$Country_Region=="Congo (Brazzaville)",]$Country_Region <- "Republic of Congo"
testdata[testdata$Country_Region=="Congo (Kinshasa)",]$Country_Region <- "Democratic Republic of the Congo"
testdata[testdata$Country_Region=="Cote d'Ivoire",]$Country_Region <- "Ivory Coast"
testdata[testdata$Country_Region=="Eswatini",]$Country_Region <- "Swaziland"
testdata[testdata$Country_Region=="Guinea-Bissau",]$Country_Region <- "Guinea Bissau"
testdata[testdata$Country_Region=="Tanzania",]$Country_Region <- "United Republic of Tanzania"


popdata <- read.csv("Test/popdata.csv")
popdata$Location <- as.character(popdata$Location)
popdata[popdata$Location=="Congo",]$Location <- "Republic of Congo"
popdata[popdata$Location=="Côte d'Ivoire",]$Location <- "Ivory Coast"

testdata <- left_join(testdata,popdata,by=c("Country_Region"="Location"))
testdata <- testdata %>% group_by(Country_Region) %>% 
  mutate(Active = Confirmed - Recovered - Deaths,
         `New Active` = Active - lag(Active, default=0),
         `Case Fatalities` = Deaths/Confirmed,
         `Deaths per 100k pop` = Deaths/PopTotal*100000,
         `Cases per 100k pop` = Confirmed/PopTotal*100000,
         `Active per 100k pop` = Active/PopTotal*100000,
         `Recoveries per 100k pop` = Recovered/PopTotal*100000,
         `New Cases` = Confirmed-lag(Confirmed,default=0),
         `New Deaths` = Deaths-lag(Deaths,default=0),
         `New Recoveries` = Recovered-lag(Recovered,default=0),
         `New Deaths per 100k pop` = `New Deaths`/PopTotal*100000,
         `New Cases per 100k pop` = `New Cases`/PopTotal*100000,
         `log Cases` = log(Confirmed),
         `log Active` = log(Confirmed),
         `log Recovered` = log(Confirmed),
         `log Deaths` = log(Deaths),
         Exposure = Date - first(Date) + 1) %>% ungroup()



responses[responses$CountryName=="Cote d'Ivoire",]$CountryName = "Ivory Coast"
responses[responses$CountryName=="Democratic Republic of Congo",]$CountryName = "Democratic Republic of the Congo"
responses[responses$CountryName=="Eswatini",]$CountryName = "Swaziland"
responses[responses$CountryName=="Tanzania",]$CountryName = "United Republic of Tanzania"
responses[responses$CountryName=="Congo",]$CountryName = "Republic of Congo"

responsedata <- subset(responses, responses$CountryName %in% unique(testdata$Country_Region))

write.csv(responsedata,"Test/responsedata.csv")

isthisit <- left_join(testdata,responsedata,by=c("Country_Region" = "CountryName","Date"))

setdiff(unique(isthisit$Country_Region),unique(covacc$location))

covacc[covacc$location=="Cote d'Ivoire",]$location = "Ivory Coast"
covacc[covacc$location=="Eswatini",]$location = "Swaziland"
covacc[covacc$location=="Cape Verde",]$location = "Cabo Verde"
covacc$date = ymd(covacc$date)

isthisit <- left_join(isthisit,covacc,by=c("Country_Region" = "location","Date"="date"))

testingdisplay <- isthisit %>% select(c(1:24,seq(29,43,2),44,46:48,49,51:55,57,59,64,68,70,72,74))

write.csv(testingdisplay,"Test/responsemodeldata.csv")

testingdisplay <- testingdisplay %>% mutate(C1 = ifelse(C1_School.closing>=2,1,0),
                             C2 = ifelse(C2_Workplace.closing>=2,1,0),
                             C3 = ifelse(C3_Cancel.public.events>=2,1,0),
                             C4 = ifelse(C4_Restrictions.on.gatherings>=2,1,0),
                             C5 = ifelse(C5_Close.public.transport>=2,1,0),
                             C6 = ifelse(C6_Stay.at.home.requirements>=2,1,0),
                             C7 = ifelse(C7_Restrictions.on.internal.movement>=2,1,0),
                             C8 = ifelse(C8_International.travel.controls>=2,1,0),
                             H1 = ifelse(H1_Public.information.campaigns>=1,1,0),
                             H2 = ifelse(H2_Testing.policy>=2,1,0),
                             H3 = ifelse(H3_Contact.tracing>=2,1,0))



testingdisplay %>% subset(testingdisplay$Country_Region %in% unique(testingdisplay$Country_Region)[1:5]) %>% ggplot() + aes(x=Date,y=`New Cases`,color=C1) + 
  geom_line() + scale_color_viridis_c() + facet_wrap(~Country_Region,scale="free")

lmedata <- testingdisplay %>% group_by(Country_Region) %>% 
  mutate(timespan = Date - first(Date) + 1) %>% ungroup() %>% 
  filter(`New Cases` > 0)

lmedata <- lmedata %>% mutate(LogCasesPop = log(`New Cases`/PopTotal),
                              LogConfirmedPop = log(Confirmed/PopTotal))

lmerecent <- lmedata %>% group_by(Country_Region) %>% top_n(60,Exposure) %>% ungroup()

lmerecent %>% ggplot() + aes(x=Date,y=`New Cases`,color=C3) + 
  geom_point() + scale_color_viridis_c() + facet_wrap(~Country_Region,scale="free")

lmerecent = lmerecent %>% na.omit()

lmedata %>% filter(Country_Region=="Egypt") %>% ggplot() + aes(x=Date,y=`New Cases`,color=log(daily_vaccinations)) +
  geom_point() + scale_color_viridis_c() + facet_wrap(~Country_Region,scale="free")

modeldata = lmedata %>% filter(!is.na(daily_vaccinations))

test <- lm(LogConfirmedPop ~ timespan + 
             C1:timespan + C1 +
             C2:timespan + C2 +
             C3:timespan + C3 +
             C4:timespan + C4 +
             C5:timespan + C5 +
             C6:timespan + C6 +
             C7:timespan + C7 +
             C8:timespan + C8 +
             log(daily_vaccinations)
           , lmerecent %>% na.omit())




lmedata <- lmedata %>% mutate(C1 = as.factor(ifelse(C1_School.closing>=2,1,0)),
                   C2 = as.factor(ifelse(C2_Workplace.closing>=2,1,0)),
                   C3 = as.factor(ifelse(C3_Cancel.public.events>=2,1,0)),
                   C4 = as.factor(ifelse(C4_Restrictions.on.gatherings>=2,1,0)),
                   C5 = as.factor(ifelse(C5_Close.public.transport>=2,1,0)),
                   C6 = as.factor(ifelse(C6_Stay.at.home.requirements>=2,1,0)),
                   C7 = as.factor(ifelse(C7_Restrictions.on.internal.movement>=2,1,0)),
                   C8 = as.factor(ifelse(C8_International.travel.controls>=2,1,0)))
lmedata <- lmedata %>% left_join(popdata,by=c("Country_Region" = "Location"))

huh <- lm(log(`New Cases`) ~ timespan +  timespan:as.factor(C2) + timespan:as.factor(C7) + timespan:as.factor(C8) +
              as.factor(C2) + as.factor(C7) + as.factor(C8),
          lmedata %>% filter(Country_Region=="Nigeria") %>%na.omit())

huh <- lm(log(`New Cases`) ~ timespan +  timespan:C2 + timespan:C7 + timespan:C8 + 
            C2 + C7 + C8,
          lmedata %>% filter(Country_Region=="Nigeria") %>%na.omit())

huh <- lmer(log(`New Cases`) ~ timespan +  timespan:C2 + timespan:C7 + timespan:C8 + 
            C2 + C7 + C8 + (timespan|Country_Region),
          lmedata %>% na.omit())


huh <- lmer(log(`New Cases`) ~ timespan +  timespan:C8 + 
            C8 + (timespan|Country_Region),
            lmedata %>% na.omit())


huh <- lmer(log(`Confirmed`) ~ timespan + 
            C8 + timespan:C8 +
            C2 + timespan:C2 + 
            C7 + timespan:C7 +
            C3 + timespan:C3 +
            C1 + timespan:C1 +
            C4 + timespan:C4 +
            C5 + timespan:C5 +
            C6 + timespan:C6 +
              (timespan|Country_Region),
          lmerecent)

huh <- lmer(LogCasesPop ~ timespan + 
              C8 + timespan:C8 +
              C2 + timespan:C2 + 
              C7 + timespan:C7 +
              C3 + timespan:C3 +
              C1 + timespan:C1 +
              C4 + timespan:C4 +
              C5 + timespan:C5 +
              C6 + timespan:C6 +
              (timespan|Country_Region),lmerecent)


huh <- lmer(log(`Confirmed`) ~ timespan + 
              (timespan|Country_Region),
            lmerecent)



huh <- lmer(log(`Confirmed`) ~ timespan + 
              GovernmentResponseIndex + PopTotal +
              (timespan|Country_Region),
            lmerecent)


betas <- fixef(huh)
se <- sqrt(diag(vcov(huh,useScale=F)))

df <- data.frame(betas=betas,se=se,var = names(betas))

ggplot(df) + aes(x=var,y=betas) + geom_point() + 
  geom_errorbar(aes(ymin=betas-2*se,ymax=betas+2*se)) + coord_flip() +
  geom_hline(yintercept=1)

ggplot(lmedata) + aes(x=C1,y=log(Confirmed)) + geom_point()

p1 <- lmerecent %>% filter(Country_Region==thing) %>% ggplot() + 
  aes(x=Date,y=H5_Investment.in.vaccines) + 
  geom_point() 

p2 <- lmerecent %>% filter(Country_Region==thing) %>% ggplot() + 
  aes(x=Date,y=StringencyIndex) + 
  geom_point() 

grid.arrange(p1,p2)

countries <- unique(lmerecent$Country_Region)

thing <- countries[1]
p1 <- lmerecent %>% filter(Country_Region==thing) %>% ggplot() + 
  aes(x=Date) + 
  geom_point(aes(y=LogCasesPop)) +
  ggtitle(thing)
  
#C6

p2 <- lmerecent %>% filter(Country_Region==thing) %>% ggplot() + 
  aes(x=Date) + 
  geom_point(aes(y=StringencyIndex),color="darkorchid") +
  geom_point(aes(y=ContainmentHealthIndex),color="green3")

p3 <- lmerecent %>% filter(Country_Region==thing) %>% ggplot() + 
  aes(x=Date) + 
  geom_point(aes(y=C8_International.travel.controls)) 
  
grid.arrange(p1,p2,p3)




 test <- lm(LogCasesPop ~ timespan + 
              C1:timespan + C1 +
              C2:timespan + C2 +
              C3:timespan + C3 +
              C4:timespan + C4 +
              C5:timespan + C5 +
              C6:timespan + C6 +
              C7:timespan + C7 +
              C8:timespan + C8
            , lmerecent %>% filter(Country_Region=="South Africa"))
 
 test <- lm(LogConfirmedPop ~ timespan + 
              C1:timespan + C1 +
              C2:timespan + C2 +
              C3:timespan + C3 +
              C4:timespan + C4 +
              C5:timespan + C5 +
              C6:timespan + C6 +
              C7:timespan + C7 +
              C8:timespan + C8
            , lmerecent %>% na.omit())
 
 test <- lm(LogCasesPop ~ timespan + 
              C1:timespan + C1 +
              C2:timespan + C2 +
              C3:timespan + C3 +
              C4:timespan + C4 +
              C5:timespan + C5 +
              C6:timespan + C6 +
              C7:timespan + C7 +
              C8:timespan + C8
            , lmerecent %>% filter(Country_Region == "Algeria") %>%  na.omit())
 
 test <- stan_lm(LogCasesPop ~ timespan + 
              C1:timespan + C1 +
              C2:timespan + C2 +
              C3:timespan + C3 +
              C4:timespan + C4 +
              C5:timespan + C5 +
              C6:timespan + C6 +
              C7:timespan + C7 +
              C8:timespan + C8
            , data=lmerecent %>% filter(Country_Region == "Algeria") %>%  na.omit(),
            prior=NULL)
 
 

 

test <- lm(LogCasesPop ~ timespan + 
             C6_Stay.at.home.requirements + C6_Stay.at.home.requirements:timespan,
           lmerecent %>% filter(Country_Region=="Algeria"))

betas <- coef(test)
se <- sqrt(diag(vcov(test,useScale=F)))

df <- data.frame(betas=betas,se=se,var = names(betas))

ggplot(df) + aes(x=var,y=betas) + geom_point() + 
  geom_errorbar(aes(ymin=betas-2*se,ymax=betas+2*se)) + coord_flip() +
  geom_hline(yintercept=0)


one <- lmerecent %>% filter(Country_Region=="Algeria") %>% na.omit()
one$Predict <- fitted(test)
one <- one %>% mutate(Test = exp(Predict)*PopTotal)

ggplot(one %>% filter(Country_Region=="Algeria")) + aes(x=Date) + 
  geom_point(aes(y=`New Cases`),color="darkorchid") + 
  geom_point(aes(y=Test),color="skyblue")

forecastpredict <- posterior_predict(test, lmerecent %>% filter(Country_Region == "Algeria") %>% na.omit())

open_coefs <- lmerecent %>% select(LogCasesPop,Country_Region,C1,C2,C3,C4,C5,C6,C7,C8,timespan) %>% na.omit() %>%
  nest(-Country_Region) %>%
  mutate(model = map(data, ~lm(LogCasesPop ~ timespan + 
                                      C1:timespan + C1 +
                                      C2:timespan + C2 +
                                      C3:timespan + C3 +
                                      C4:timespan + C4 +
                                      C5:timespan + C5 +
                                      C6:timespan + C6 +
                                      C7:timespan + C7 +
                                      C8:timespan + C8,data=.)),
         tidied = map(model,tidy)) %>% filter(term == "timespan:C7") %>% 
  unnest(tidied) %>% 
  mutate(adjusted = p.adjust(p.value)) %>%
  arrange(desc(estimate))

barplot(open_coefs$estimate, ylab = "Coefficients", xlab = "Countries", main = "Openness Coefficients")

