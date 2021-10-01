# Accumulation of models regarding response data.
# This was all exploratory so I'm keeping notes of everything.

source("ResponseData.R")

# Create binary variables.
# Government response at some level was 2, so I set it to 2 or more.
# However, the scale can be changed depending on interest.
modeldata <- modeldata %>% mutate(C1 = ifelse(C1_School.closing>=2,1,0),
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

# Convert dataset for models.
# Remove days with no new cases - assumption is that no data was recorded.
# May not be an applicable assumption anymore.
lmedata <- modeldata %>% group_by(Country_Region) %>% 
  mutate(timespan = Date - first(Date) + 1) %>% ungroup() %>% 
  filter(`New Cases` > 0)

# New variables & converting data.
lmedata <- lmedata %>% mutate(LogCasesPop = log(`New Cases`/PopTotal),
                              LogConfirmedPop = log(Confirmed/PopTotal))

lmerecent <- lmedata %>% group_by(Country_Region) %>% 
  top_n(60,Exposure) %>% ungroup()

#Remove NAs
lmerecent = lmerecent %>% na.omit()

# Subsetting for vaccinations.
modeldata = lmedata %>% filter(!is.na(total_vaccinations))


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

