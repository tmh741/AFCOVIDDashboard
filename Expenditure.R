source("InstallPackages.R")
source("VaccDataClean.R")
source("DAHClean.R")

# Read in and fix population data.
popdata = read.csv("Test/popdata.csv")
popdata[popdata$Location=="Congo",]$Location = "Republic of Congo"
popdata[popdata$Location=="Côte d'Ivoire",]$Location = "Ivory Coast"

# Read in data from government response?
responsedata = read.csv("Test/responsemodeldata.csv")[,-1]
responsedata[responsedata$Country_Region=="Swaziland",]$Country_Region = "Eswatini"

# Read in and clean expenditure data.
# This first step is matching shortened variable names.
expend = read_xlsx("VaccData/NHA indicators (3).xlsx")[-1,-3]
varmatch = data.frame(Indicators=unique(expend$Indicators),
                      Variable=c("CHEPctGDP",
                                 "CHEpCapUSD",
                                 "DOMExpPctCHE",
                                 "DomGGHEPctGDP",
                                 "DomGGHEpCapUSD",
                                 "PHCExpendpCapUSD",
                                 "DomGGEPCHpctGGHE",
                                 "PHCPctGDP"))
expend = left_join(expend,varmatch,by=c("Indicators"))

# Pivot data to format it correctly.
expend = expend %>% select(-Indicators) %>% group_by(Countries) %>% 
  pivot_wider(
    names_from = Variable,
    names_glue="{Variable}_{.value}",
    values_from = c(`2000`:`2018`)
  ) %>% ungroup()
expend = expend %>% group_by(Countries) %>% 
  pivot_longer(cols = CHEPctGDP_2000:PHCPctGDP_2018,
               names_to=c(".value","Year"),
               names_pattern= "(.*)_(.*)"
  ) %>% ungroup()

# Correct country names.
setdiff(unique(responsedata$Country_Region),unique(expend$Countries))
expend[expend$Countries=="Cabo Verde Republic of",]$Countries= "Cabo Verde"
expend[expend$Countries=="Côte d'Ivoire",]$Countries = "Ivory Coast"
expend[expend$Countries=="Congo",]$Countries = "Republic of Congo"

# Subset data.
expenddata = expend %>% subset(Countries %in% unique(responsedata$Country_Region))

# Read in nurse dataset & adjsut formats.
nurses= read.csv("VaccData/HWF_0006,HWF_0007,HWF_0008,HWF_0009.csv")

setdiff(unique(expenddata$Countries),unique(nurses$Country))
nurses[nurses$Country=="Congo",]$Country = "Republic of Congo"
nurses[nurses$Country=="Côte d'Ivoire",]$Country = "Ivory Coast"
expenddata$Year=as.numeric(expenddata$Year)
expenddata$Countries=as.character(expenddata$Countries)
expenddata = expenddata %>% mutate(Country=Countries, replace=T) # Included to address join problems.
nurses$Country=as.character(nurses$Country)

# Join with nurses.
expenddata = expenddata %>% left_join(nurses,by=c("Country","Year"))

# Join with the rest of the data from ReadData!
alldata[alldata$Country=="Congo",]$Country = "Republic of Congo"
alldata[alldata$Country=="Côte d'Ivoire",]$Country = "Ivory Coast"
alldata[alldata$Country=="Cote d'Ivoire",]$Country = "Ivory Coast"
alldata[alldata$Country=="Democratic Republic of Congo",]$Country = "Democratic Republic of the Congo"
alldata[alldata$Country=="Tanzania",]$Country = "United Republic of Tanzania"
alldata$Year = as.numeric(alldata$Year)

# Join datasets.
fulldata = expenddata %>% select(-Countries) %>% 
  left_join(alldata,by=c("Country","Year"))

# Subset data
fullsub = fulldata %>% subset(Country %in% unique(responsedata$Country_Region)) %>% 
  select(Country,Year,
         pct_Measles1,
         pct_DPT, 
         Nursing.and.midwifery.personnel...number., 
         CHEpCapUSD,
         DomGGHEPctGDP,
         DomGGHEpCapUSD,
         Estimate_GovEff,
         Estimate_VoiceAcc,
         Estimate_PolStab,
         Estimate_RegQual,
         Estimate_RuleLaw,
         Estimate_CorruptControl) 

# Calculate new variables.
fullsub = as.data.frame(fullsub) # Made a df because there were tibble problems. 
for (i in 2:ncol(fullsub))
  fullsub[,i] = as.numeric(fullsub[,i])
fullsub <- fullsub %>% 
  mutate(LogNurses = log(Nursing.and.midwifery.personnel...number.)) %>% 
  select(-Nursing.and.midwifery.personnel...number.)

# Read in birth data & clean it.
births = read_xlsx("WPP2019_FERT_F01_BIRTHS_BOTH_SEXES.xlsx",skip=16) %>% filter(Type=="Country/Area")
colnames(births)[3] = "Country"
births= births[,c(3,8:21)]

for (i in 1:(ncol(births)-1))
  colnames(births)[i+1] = str_split(colnames(births)[i+1],pattern="-")[[1]][2]
colnames(births)

births[births$Country=="Côte d'Ivoire",]$Country = "Ivory Coast"
births[births$Country=="Congo",]$Country = "Republic of Congo"
births = births %>% pivot_longer(-Country,names_to="Year",values_to="Births") %>% mutate(Births=as.numeric(Births))

# Calculate average every 5 years to convert to annual scale.
birthsav = data.frame(Country=rep(unique(births$Country),each=70),
                      Year=rep(1951:2020,times=201),
                      Births_Ave=rep(births$Births*1000/5,each=5))

# Join datasets & reformat.
fullsub = left_join(fullsub,birthsav,by=c("Country","Year")) %>% 
  mutate(MeaslesPop = 0.01*Births_Ave*pct_Measles1)
fullsub$Country[fullsub$Country == "Ivory Coast"] = "Cote d'Ivoire"
fullsub$Country[fullsub$Country == "Cabo Verde"] = "Cape Verde"
fullsub$Country[fullsub$Country == "Gambia"] = "The Gambia"

# Join with DAH data
dahanalysis$recipient_country[dahanalysis$recipient_country == "Congo"] = "Republic of Congo"
dahanalysis$recipient_country[dahanalysis$recipient_country == "Swaziland"] = "Eswatini"
dahanalysis$recipient_country[dahanalysis$recipient_country == "Tanzania"] = "United Republic of Tanzania"
fullsub = left_join(fullsub, dahanalysis, by = c("Year" = "year", "Country" = "recipient_country"))

# Subset once again! 
fullsubcleaned = fullsub %>% 
  select(pct_Measles1,
         CHEpCapUSD, 
         Estimate_GovEff, 
         Estimate_VoiceAcc, 
         Estimate_PolStab, 
         Estimate_RuleLaw, 
         Immunization,
         PandemicPrep,
         Year,
         Country) %>% 
  na.omit()

# Make scaled variables
fullsubcleaned$Year_c = fullsubcleaned$Year - min(fullsubcleaned$Year[1])
fullsubcleaned$CHEscaled = fullsubcleaned$CHEpCapUSD/100 - mean(fullsubcleaned$CHEpCapUSD/100)

# Read population data
popestimates = read.csv("WorldPopEstimates.csv")
popestimates[popestimates$Location == "Cabo Verde",]$Location = "Cape Verde"
popestimates[popestimates$Location == "Congo",]$Location = "Republic of Congo"
popestimates[popestimates$Location == "Gambia",]$Location = "The Gambia"
popestimates[popestimates$Location == "Côte d'Ivoire",]$Location = "Cote d'Ivoire"


fullsubcleaned = fullsubcleaned %>% 
  left_join(popestimates, by = c("Country" = "Location","Year" = "Time"))

fullsubcleaned$ImmunpCap = fullsubcleaned$Immunization/fullsubcleaned$PopTotal



