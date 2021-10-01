source("Expenditure.R")

# Make a function for z-scoring.
# I should use these more often.
zscore = function(x){
  return((x - mean(x))/sd(x))
}

### Newest Model

modelData = fullsubcleaned %>%
  filter(Year >= 2000) %>% 
  mutate(GovEff_z = zscore(Estimate_GovEff),
         RuleLaw_z = zscore(Estimate_RuleLaw),
         PolStab_z = zscore(Estimate_PolStab),
         VoiceAcc_z = zscore(Estimate_VoiceAcc),
         ImmunScaled = ImmunpCap*10000 - mean(ImmunpCap)*10000,
         PrepTenMill_c = PandemicPrep/10000 - mean(PandemicPrep/10000)) %>% na.omit()

modelTest = modelData %>% filter(Year==max(Year))
modelTrain = modelData %>% filter(Year!=max(Year))

modelFinal = glmer(cbind(pct_Measles1, 100-pct_Measles1) ~ 
                     CHEscaled +
                     ImmunScaled +
                     Year_c + 
                     GovEff_z +
                     RuleLaw_z +
                     PolStab_z +
                     VoiceAcc_z +
                     (Year_c|Country), 
                   family = binomial(link="logit"), data=modelTrain)

modelNull = glmer(cbind(pct_Measles1, 100-pct_Measles1) ~ 
                    1 +
                    (1|Country), 
                  family = binomial(link="logit"), data=modelTrain)

qqnorm(resid(modelFinal))
qqline(resid(modelFinal))

qqnorm(ranef(modelFinal)$Country[,1])
qqline(ranef(modelFinal)$Country[,1])

plot(fitted(modelFinal),resid(modelFinal))
abline(h=0)

modelTrain$fitted = fitted(modelFinal)
plot(modelTrain$fitted*100,jitter(modelTrain$pct_Measles1,0.1),xlab="Fitted",ylab="Observed")
abline(b=1,a=0)

modelTest$predict = predict(modelFinal,newdata=modelTest,type="response",allow.new.levels=T)
modelTest$ErrSq = (modelTest$pct_Measles1/100-modelTest$predict)^2
mean(modelTest[modelTest$Country!="South Sudan",]$ErrSq)
summary(modelFinal)

r.squaredGLMM(modelFinal,modelNull)

ggplot() + geom_point(data=modelTrain, aes(x=Year,y=pct_Measles1/100),color="skyblue") +
  geom_point(data=modelTest, aes(x=Year,y=predict),color="darkorchid") +
  geom_point(data=modelTest,aes(x=Year,y=pct_Measles1/100),color="skyblue") + ylab("% Vaccinated (Measles 1st Dose)") +
  theme_bw() +
  facet_wrap(~Country)

coefficients = as.data.frame(summary(modelFinal)$coefficients)
colnames(coefficients) = c("Estimate","StdError","Z","alpha")
coefficients$LowBound = coefficients$Estimate-2*coefficients$StdError
coefficients$UpBound = coefficients$Estimate+2*coefficients$StdError

ggplot(coefficients) + aes(x=Estimate,y=rownames(coefficients),label=round(Estimate,2)) + geom_point() + 
  geom_errorbar(aes(xmin=LowBound,xmax=UpBound),width=0.1) +
  geom_vline(xintercept=0,linetype="dashed") + 
  geom_text(position=position_nudge(y=-0.15))+
  xlab("Estimate") +
  ylab("Variable") +
  theme_bw() + 
  theme(text=element_text(size=15))

