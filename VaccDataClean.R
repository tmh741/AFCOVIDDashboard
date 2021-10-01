# Read and set up the vaccine datasets.
source("InstallPackages.R")
source("ResponseData.R")
library(readxl)

# Read in vaccine data.
covacc =read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
covacc <- covacc %>% select(location,date,total_vaccinations) %>% na.omit()
covacc$date = ymd(covacc$date)

# Read in first measles shot.
measles1 = read.csv("VaccData/data.csv",skip=1) #Measels1
colnames(measles1)=str_replace(colnames(measles1),
                               "X",
                               "Measels1_")
measles1 = measles1 %>% 
  pivot_longer(cols=starts_with("Measels1_"),
               names_prefix="Measels1_",
               names_to="Year",
               values_to="pct_Measles1",
               values_drop_na=T)

# Read in second measles shot.
measles2=read.csv("VaccData/data-2.csv",skip=1) #Measels2
colnames(measles2)=str_replace(colnames(measles2),
                               "X",
                               "Measels2_")
measles2 = measles2 %>% 
  pivot_longer(cols=starts_with("Measels2_"),
               names_prefix="Measels2_",
               names_to="Year",
               values_to="pct_Measles2",
               values_drop_na=T)

# Read in DPT.
dpt=read.csv("VaccData/data-3.csv",skip=1) #DPT
colnames(dpt) = str_replace(colnames(dpt),
                            "X",
                            "DPT_")
dpt = dpt %>% pivot_longer(cols=starts_with("DPT_"),
                           names_prefix="DPT_",
                           names_to="Year",
                           values_to="pct_DPT",
                           values_drop_na=T)

# Read in the WGI Datasets.
# There's probably a way to standardize.
# When I tried to make it a function I ran into problems.

# Government Effectiveness
wgidata = read_xlsx("VaccData/wgidataset.xlsx",sheet=4,skip=14,na="#N/A") 
cleantext=str_replace_all(colnames(wgidata),"[[:punct:]]+","")
cleantext=str_replace_all(cleantext,"[[:digit:]]+","")
years=rep(c(1996,1998,2000,2002:2019),each=6)
cleantext=paste(cleantext,c("","",years),sep="_")
colnames(wgidata) = cleantext
govEff = wgidata %>% 
  dplyr::select(contains(c("Country","Estimate","StdErr","NumSrc"))) %>% 
  pivot_longer(cols=Estimate_1996:NumSrc_2019,
               names_to=c(".value","Year"),
               names_pattern="(.*)_(.*)")
colnames(govEff)[3:5] = paste(colnames(govEff)[3:5],"GovEff",sep="_")


# Voice & Accountability
wgidata = read_xlsx("VaccData/wgidataset.xlsx",sheet=2,skip=14,na="#N/A") 
cleantext=str_replace_all(colnames(wgidata),"[[:punct:]]+","")
cleantext=str_replace_all(cleantext,"[[:digit:]]+","")
years=rep(c(1996,1998,2000,2002:2019),each=6)
cleantext=paste(cleantext,c("","",years),sep="_")
colnames(wgidata) = cleantext
voiAcc = wgidata %>% 
  dplyr::select(contains(c("Country","Estimate","StdErr","NumSrc"))) %>% 
  pivot_longer(cols=Estimate_1996:NumSrc_2019,
               names_to=c(".value","Year"),
               names_pattern="(.*)_(.*)")
colnames(voiAcc)[3:5] = paste(colnames(voiAcc)[3:5],"VoiceAcc",sep="_")

# Political Stability
wgidata = read_xlsx("VaccData/wgidataset.xlsx",sheet=3,skip=14,na="#N/A") #Goverment
cleantext=str_replace_all(colnames(wgidata),"[[:punct:]]+","")
cleantext=str_replace_all(cleantext,"[[:digit:]]+","")
years=rep(c(1996,1998,2000,2002:2019),each=6)
cleantext=paste(cleantext,c("","",years),sep="_")
colnames(wgidata) = cleantext
polStab = wgidata %>% dplyr::select(contains(c("Country","Estimate","StdErr","NumSrc"))) %>% 
  pivot_longer(cols=Estimate_1996:NumSrc_2019,
               names_to=c(".value","Year"),
               names_pattern="(.*)_(.*)")
colnames(polStab)[3:5] = paste(colnames(polStab)[3:5],"PolStab",sep="_")

# Regulatory Quality
wgidata = read_xlsx("VaccData/wgidataset.xlsx",sheet=5,skip=14,na="#N/A") #Goverment
cleantext=str_replace_all(colnames(wgidata),"[[:punct:]]+","")
cleantext=str_replace_all(cleantext,"[[:digit:]]+","")
years=rep(c(1996,1998,2000,2002:2019),each=6)
cleantext=paste(cleantext,c("","",years),sep="_")
colnames(wgidata) = cleantext
regQual = wgidata %>% dplyr::select(contains(c("Country","Estimate","StdErr","NumSrc"))) %>%
  pivot_longer(cols=Estimate_1996:NumSrc_2019,
               names_to=c(".value","Year"),
               names_pattern="(.*)_(.*)")
colnames(regQual)[3:5] = paste(colnames(regQual)[3:5],"RegQual",sep="_")

# Rule & Law
wgidata = read_xlsx("VaccData/wgidataset.xlsx",sheet=6,skip=14,na="#N/A") #Goverment
cleantext=str_replace_all(colnames(wgidata),"[[:punct:]]+","")
cleantext=str_replace_all(cleantext,"[[:digit:]]+","")
years=rep(c(1996,1998,2000,2002:2019),each=6)
cleantext=paste(cleantext,c("","",years),sep="_")
colnames(wgidata) = cleantext
rulLaw = wgidata %>% dplyr::select(contains(c("Country","Estimate","StdErr","NumSrc"))) %>%
  pivot_longer(cols=Estimate_1996:NumSrc_2019,
               names_to=c(".value","Year"),
               names_pattern="(.*)_(.*)")
colnames(rulLaw)[3:5] = paste(colnames(rulLaw)[3:5],"RuleLaw",sep="_")

# Control of Corruption
wgidata = read_xlsx("VaccData/wgidataset.xlsx",sheet=7,skip=14,na="#N/A") #Goverment
cleantext=str_replace_all(colnames(wgidata),"[[:punct:]]+","")
cleantext=str_replace_all(cleantext,"[[:digit:]]+","")
years=rep(c(1996,1998,2000,2002:2019),each=6)
cleantext=paste(cleantext,c("","",years),sep="_")
colnames(wgidata) = cleantext
corCon = wgidata %>% dplyr::select(contains(c("Country","Estimate","StdErr","NumSrc"))) %>% 
  pivot_longer(cols=Estimate_1996:NumSrc_2019,
               names_to=c(".value","Year"),
               names_pattern="(.*)_(.*)")
colnames(corCon)[3:5] = paste(colnames(corCon)[3:5],"CorruptControl",sep="_")

# Join these datasets together.
wgi = full_join(govEff,voiAcc,by=c("CountryTerritory_","Year")) %>% full_join(polStab,by=c("CountryTerritory_","Year")) %>% 
  full_join(regQual,by=c("CountryTerritory_","Year")) %>%
  full_join(rulLaw,by=c("CountryTerritory_","Year")) %>%
  full_join(corCon,by=c("CountryTerritory_","Year"))

# Fix countries names for linking.
wgi$CountryTerritory_[wgi$CountryTerritory_=="Bahamas, The"] = "Bahamas"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Cape Verde"] = "Cabo Verde"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Congo, Dem. Rep."] = "Democratic Republic of the Congo"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Congo, Rep."] = "Congo"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Egypt, Arab Rep."] = "Egypt"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Swaziland"] = "Eswatini"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Gambia, The"] = "Gambia"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Kyrgyz Republic"] =  "Kyrgyzstan"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Lao PDR"] = "Lao People's Democratic Republic"
wgi$CountryTerritory_[wgi$CountryTerritory_=="St. Kitts and Nevis"] = "Saint Kitts and Nevis"
wgi$CountryTerritory_[wgi$CountryTerritory_=="St. Lucia"] = "Saint Lucia"
wgi$CountryTerritory_[wgi$CountryTerritory_=="St. Vincent and the Grenadines"] = "Saint Vincent and the Grenadines"
wgi$CountryTerritory_[wgi$CountryTerritory_=="São Tomé and Principe"] = "Sao Tome and Principe"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Slovak Republic"] = "Slovakia"
wgi$CountryTerritory_[wgi$CountryTerritory_=="Yemen, Rep."] = "Yemen"

# Join measles & DPT Datasets.
alldata = full_join(measles1,measles2,by=c("Country","Year"))
alldata = full_join(alldata,dpt,by=c("Country","Year"))
alldata$Country[alldata$Country=="Bolivia (Plurinational State of)"] = "Bolivia"
alldata$Country[alldata$Country=="Viet Nam"] = "Vietnam"
alldata$Country[alldata$Country=="Czechia"] = "Czech Republic"
alldata$Country[alldata$Country=="Democratic People's Republic of Korea"] = "Korea, Dem. Rep."
alldata$Country[alldata$Country=="United Republic of Tanzania"] = "Tanzania"
alldata$Country[alldata$Country=="Republic of Korea"] = "Korea, Rep."
alldata$Country[alldata$Country=="Iran (Islamic Republic of)"] = "Iran, Islamic Rep."
alldata$Country[alldata$Country=="Micronesia (Federated States of)"] = "Micronesia, Fed. Sts."
alldata$Country[alldata$Country=="Republic of Moldova"] = "Moldova"
alldata$Country[alldata$Country=="United Kingdom of Great Britain and Northern Ireland"] = "United Kingdom"
alldata$Country[alldata$Country=="Venezuela (Bolivarian Republic of)"] = "Venezuela, RB"
alldata$Country[alldata$Country=="United States of America"] = "United States"

# Join WGI and Measles datasets.
alldata = full_join(alldata,wgi,by=c("Country"="CountryTerritory_","Year"))

# Fix names
alldata$Country[alldata$Country=="Brunei Darussalam"] = "Brunei"
alldata$Country[alldata$Country=="Côte d'Ivoire"] = "Cote d'Ivoire"
alldata$Country[alldata$Country=="Korea, Rep."] = "South Korea"
alldata$Country[alldata$Country=="Korea, Dem. Rep."] = "Korea"
alldata$Country[alldata$Country=="Democratic Republic of the Congo"] = "Democratic Republic of Congo"
alldata$Country[alldata$Country=="Iran, Islamic Rep."] = "Iran"
alldata$Country[alldata$Country=="Lao People's Democratic Republic"] = "Laos"
alldata$Country[alldata$Country=="Micronesia, Fed. Sts."] = "Micronesia (country)"
alldata$Country[alldata$Country=="Russian Federation"] = "Russia"
alldata$Country[alldata$Country=="Syrian Arab Republic"] = "Syria"
alldata$Country[alldata$Country=="Timor-Leste"] = "Timor"
alldata$Country[alldata$Country=="Venezuela, RB"] = "Venezuela"
alldata$Country[alldata$Country=="Hong Kong SAR, China"] = "Hong Kong"

# Read Human Development Index data and correct for countries.
hdi = read.csv("VaccData/human-development-index.csv")
hdi$Entity[hdi$Entity=="Cape Verde"] = "Cabo Verde"
hdi$Entity[hdi$Entity=="Czechia"] = "Czech Republic"

# Make year a number
alldata$Year = as.numeric(alldata$Year)

# Join HDI data.
alldata = full_join(alldata,hdi,by=c("Country" = "Entity","Year"))

# Omit NA's.
allclean = na.omit(alldata)

# Read in GSCI scores. We didn't do anything with this.
data6 = read_xlsx("VaccData/GSCI_Scores_2020.xlsx",skip=3)
colnames(data6)=gsub("[^A-Za-z]","",colnames(data6))
categories = read_xlsx("VaccData/GSCI_Scores_2020.xlsx")[1,]
categories = na.omit(as.vector(t(as.matrix(categories[1,]))))
categories=c(rep(categories[2:7],each=2))
colnames(data6)[-1] = paste(categories,colnames(data6)[-1],sep="_") 
