rm(list = ls())
setwd("C:/Users/Asus S512JF-EJ014T/Downloads")


library(tidyverse) 
library(AER)
library("lmtest")
library("readxl")

data.0 <- read_excel("datasus4.xlsx")
summary(data.0)

#Too much NAs, don't you think?
library(dplyr)
data <- data.0 %>% filter_at(vars(1:22),all_vars(!is.na(.)))

#data at a Glance
str(data)
summary(data)

attach(data) #attaching variables to the search Path

#examples
CompanyName
TRBC_SectorName

#Aggregation at SECTOR and COUNTRY level -> turn them into factors
# -> Summary Table within Sectors, Country 
data%>%
  mutate(TRBC_SectorName <- as.factor(TRBC_SectorName))%>%
  mutate(Country_Headquarters <- as.factor(Country_Headquarters))%>%
  group_by(TRBC_SectorName, Country_Headquarters)

#Data according to years of disclosure
data_2021 <- data%>%
  mutate(TRBC_SectorName <- as.factor(TRBC_SectorName))%>%
  mutate(Country_Headquarters <- as.factor(Country_Headquarters))%>%
  group_by(TRBC_SectorName)%>%
  select(5, 8, 11, 14, 17, 20)

data_2020 <- data%>%
  mutate(TRBC_SectorName <- as.factor(TRBC_SectorName))%>%
  mutate(Country_Headquarters <- as.factor(Country_Headquarters))%>%
  group_by(TRBC_SectorName)%>%
  select(6,9,12,15,18,21)

data_2019 <- data%>%
  mutate(TRBC_SectorName <- as.factor(TRBC_SectorName))%>%
  mutate(Country_Headquarters <- as.factor(Country_Headquarters))%>%
  group_by(TRBC_SectorName)%>%
  select(7,10,13,16,19,22)

#Composing tavble of summary statistics : 2019
esg_mean_2019 <- tapply(ESGScore2019, TRBC_SectorName, mean)
esg_sd_2019 <- tapply(ESGScore2019,TRBC_SectorName, sd)
esg_obs_2019 <- tapply(ESGScore2019, TRBC_SectorName, length)
esg_2019_min <- tapply(ESGScore2019, TRBC_SectorName, min)
esg_2019_Max <- tapply(ESGScore2019, TRBC_SectorName, max)
stats_esg_2019 <- cbind(N = esg_obs_2019, AVG = esg_mean_2019, SD = esg_sd_2019, min = esg_2019_min, Max = esg_2019_Max)
stats_esg_2019

co2_mean_2019 <- tapply(CO2Emissions2019, TRBC_SectorName, mean) 
co2_sd_2019 <- tapply(CO2Emissions2019, TRBC_SectorName, sd)  
co2_obs_2019 <- tapply(CO2Emissions2019, TRBC_SectorName, length)
co2_min_2019 <- tapply(CO2Emissions2019, TRBC_SectorName, min)
co2_Max_2019 <-  tapply(CO2Emissions2019, TRBC_SectorName, max) 
stats_Co2_2019 <- cbind(N = co2_obs_2019, AVG = co2_mean_2019, SD = co2_sd_2019, min = co2_min_2019, Max = co2_Max_2019)
stats_Co2_2019

roe_mean_2019 <- tapply(ROE2019, TRBC_SectorName, mean)
roe_sd_2019 <- tapply(ROE2019, TRBC_SectorName, sd)
roe_obs_2019 <- tapply(ROE2019, TRBC_SectorName, length)
roe_min_2019 <- tapply(ROE2019, TRBC_SectorName, min)
roe_max_2019 <- tapply(ROE2019, TRBC_SectorName,max)  
stats_roe_2019 <- cbind(N = roe_obs_2019, AVG = roe_mean_2019, SD = roe_sd_2019, min = roe_min_2019, Max = roe_max_2019)

#2020
esg_mean_2020 <- tapply(ESGScore2020, TRBC_SectorName, mean)
esg_sd_2020 <- tapply(ESGScore2020,TRBC_SectorName, sd)
esg_obs_2020 <- tapply(ESGScore2020, TRBC_SectorName, length)
esg_2020_min <- tapply(ESGScore2020, TRBC_SectorName, min)
esg_2020_Max <- tapply(ESGScore2020, TRBC_SectorName, max)
stats_esg_2020 <- cbind(N = esg_obs_2019, AVG = esg_mean_2019, SD = esg_sd_2019, min = esg_2019_min, Max = esg_2019_Max)


#C02 Emissions
co2_mean_2020 <- tapply(CO2Emissions2020, TRBC_SectorName, mean) 
co2_sd_2020 <- tapply(CO2Emissions2020, TRBC_SectorName, sd)  
co2_obs_2020 <- tapply(CO2Emissions2020, TRBC_SectorName, length)
co2_min_2020 <- tapply(CO2Emissions2020, TRBC_SectorName, min)
co2_Max_2020 <-  tapply(CO2Emissions2020, TRBC_SectorName, max) 
stats_Co2_2020 <- cbind(N = co2_obs_2020, AVG = co2_mean_2020, SD = co2_sd_2020, min = co2_min_2020, Max = co2_Max_2020)
stats_Co2_2020

roe_mean_2020 <- tapply(ROE2020, TRBC_SectorName, mean)
roe_sd_2020 <- tapply(ROE2020, TRBC_SectorName, sd)
roe_obs_2020 <- tapply(ROE2020, TRBC_SectorName, length)
roe_min_2020 <- tapply(ROE2020, TRBC_SectorName, min)
roe_max_2020 <- tapply(ROE2020, TRBC_SectorName,max)  
stats_roe_2020 <- cbind(N = roe_obs_2020, AVG = roe_mean_2019, SD = roe_sd_2019, min = roe_min_2019, Max = roe_max_2019)


#2021 
esg_mean_2021 <- tapply(ESGScore2020, TRBC_SectorName, mean)
esg_sd_2021 <- tapply(ESGScore2020,TRBC_SectorName, sd)
esg_obs_2021 <- tapply(ESGScore2020, TRBC_SectorName, length)
esg_2021_min <- tapply(ESGScore2020, TRBC_SectorName, min)
esg_2021_Max <- tapply(ESGScore2020, TRBC_SectorName, max)
stats_esg_2021 <- cbind(N = esg_obs_2019, AVG = esg_mean_2019, SD = esg_sd_2019, min = esg_2019_min, Max = esg_2019_Max)


co2_mean_2021<- tapply(CO2Emissions2021, TRBC_SectorName, mean) 
co2_sd_2021 <- tapply(CO2Emissions2021, TRBC_SectorName, sd)  
co2_obs_2021 <- tapply(CO2Emissions2021, TRBC_SectorName, length)
co2_min_2021 <- tapply(CO2Emissions2021, TRBC_SectorName, min)
co2_Max_2021 <-  tapply(CO2Emissions2021, TRBC_SectorName, max) 
stats_Co2_2021 <- cbind(N = co2_obs_2021, AVG = co2_mean_2019, SD = co2_sd_2019, min = co2_min_2019, Max = co2_Max_2019)

#CO2 Emission
co2_mean_2021<- tapply(CO2Emissions2021, TRBC_SectorName, mean) 
co2_sd_2021 <- tapply(CO2Emissions2021, TRBC_SectorName, sd)  
co2_obs_2021 <- tapply(CO2Emissions2021, TRBC_SectorName, length)
co2_min_2021 <- tapply(CO2Emissions2021, TRBC_SectorName, min)
co2_Max_2021 <-  tapply(CO2Emissions2021, TRBC_SectorName, max) 
stats_Co2_2021 <- cbind(N = co2_obs_2021, AVG = co2_mean_2021, SD = co2_sd_2021, min = co2_min_2021, Max = co2_Max_2021)
stats_Co2_2021

roe_mean_2021<- tapply(ROE2021, TRBC_SectorName, mean)
roe_sd_2021 <- tapply(ROE2021, TRBC_SectorName, sd)
roe_obs_2021 <- tapply(ROE2021, TRBC_SectorName, length)
roe_min_2021 <- tapply(ROE2021, TRBC_SectorName, min)
roe_max_2021 <- tapply(ROE2021, TRBC_SectorName,max)  
stats_roe_2021 <- cbind(N = roe_obs_2021, AVG = roe_mean_2019, SD = roe_sd_2019, min = roe_min_2019, Max = roe_max_2019)



#Splitting data according to Country 

data_2021_ITA <- by(data_2021 , Country_Headquarters=='Italy', summary)
data_2021_ITA

data_2021_GER <- by(data_2021, Country_Headquarters == 'Germany', summary)
data_2021_GER

data_2021_FRA <- by(data_2021, Country_Headquarters=='France', summary)
data_2021_FRA




################### Another kind of table

stats_2021 <- cbind(N = lapply(data_2021[ ,2:7], length),
      AVG = lapply(data_2021[ ,2:7],mean), 
      St.D =lapply(data_2021[ ,2:7], sd),
       min = lapply(data_2021[ ,2:7],min),
       Max = lapply(data_2021[ ,2:7],max),
       Med = lapply(data_2021[ ,2:7],median))
stats_2021

stats_2020 <- cbind(N = lapply(data_2020[ ,2:7], length),
                    AVG = lapply(data_2020[ ,2:7],mean), 
                    St.D =lapply(data_2020[ ,2:7], sd),
                    min = lapply(data_2020[ ,2:7],min),
                    Max = lapply(data_2020[ ,2:7],max),
                    Med = lapply(data_2020[ ,2:7],median))
stats_2020

stats_2019 <- cbind(N = lapply(data_2019[ ,2:7], length),
                    AVG = lapply(data_2019[ ,2:7],mean), 
                    St.D =lapply(data_2019[ ,2:7], sd),
                    min = lapply(data_2019[ ,2:7],min),
                    Max = lapply(data_2019[ ,2:7],max),
                    Med = lapply(data_2019[ ,2:7],median))
stats_2021


####################
#NEED TO FIX SOME VALUES? BOOOOOO
####################



#### *** COMPARE VARIABLES -> Correlation *** #### 
#2019
view(cor_data_2019 <- cor(num_data_2019 <- data_2019[ ,2:7]))
cor_data_2019_CEP <- cor(ESGScore2019, CO2Emissions2019)
view(cor_2019_CEPvsCFP <- cor(data_2019[ , c(2, 3, 7)]))
cor_data_2019
cor_2019_CEPvsCFP

#2020
view(cor_data_2020 <- cor(num_data_2020 <- data_2020[ ,2:7]))
cor_data_2020_CEP <- cor(ESGScore2020, CO2Emissions2020)
view(cor_2020_CEPvsCFP <- cor(data_2020[ , c(2, 3, 7)]))
cor_data_2020
cor_2020_CEPvsCFP

#2021
view(cor_data_2021 <- cor(num_data_2021 <- data_2021[ ,2:7]))
cor_data_2021_CEP <- cor(ESGScore2021, CO2Emissions2021)
view(cor_2021_CEPvsCFP <- cor(data_2021[ , c(2, 3, 7)]))
cor_data_2021
cor_2021_CEPvsCFP

#IT SEEMS TO BE NO CORRELATION AT ALLL :(((((

                         

                             #### *** REGRESSION *** ####

# -> How much CEP (ESG score ; CO2 Emiss) influence CFP (ROE)  ???
# NULL HP = There is NO RELEVANCE in Environmental Indicators for Returns on Equity

# yearly check

#2019
summary(data_2019)
view(data_2019)

summary(lm_2019 <- lm(ROE2019 ~ ESGScore2019 + log(CO2Emissions2019), data = data_2019)) 
#A unit ESG increase translates into an expected increase in ROE of 0.06
#An increase of 1% in Emissions translates into an expected decrease in ROE of 0.0004
#No STATISTICAL RELEVANCE -> p-value = c(0.133, 0.874) -> R^2 = 0.005 -> DO NOT REJECT NULL HP

#Graphic representation : ROE and ESG, ROE and CO2 Emissions 
ggplot(data_2019,aes(ESGScore2019, ROE2019))+geom_point()+
  geom_smooth(method='lm',formula=y~x)

ggplot(data_2019,aes(log(CO2Emissions2019), ROE2019))+geom_point()+
  geom_smooth(method='lm',formula=y~x)
  

#Even within some sectors we do not find relevance -> DO NOT REJECT NULL HP

#adding CONTROL VARIABLES, log taken because of large differences in distributions 
summary(lm_2019a <- lm(ROE2019 ~ ESGScore2019 + log(CO2Emissions2019), data = data_2019))

summary(lm_2019b <- lm(ROE2019 ~ ESGScore2019+
                         log(CO2Emissions2019) + 
                         log(TotalRevenue2019) + 
                         log(TotalAssets2019) +
                         log(EmployeesAv2019), data = data_2019)) 
#un-restricted model : checking for structural variables produces statistical significance
 # in terms of C02 Emissions with an expected value of -0.0075293
# R-squared 0.04 still little explanatory of how much ROE can change as dependent variable


#2020
summary(data_2020)
view(data_2020)

summary(lm_2020 <- lm(ROE2020 ~ ESGScore2020 + log(CO2Emissions2020), data = data_2020)) 
#A unit ESG increase translates into an expected increase in ROE of 0.05
#An increase of 1% in Emissions translates into an expected decrease in ROE of 0.0015
#No STATISTICAL RELEVANCE -> p-value = c(0.31, 0.60) -> R^2 = 0.002 -> DO NOT REJECT NULL HP

#Graphic representation : ROE and ESG, ROE and CO2 Emissions 
ggplot(data_2020,aes(ESGScore2020, ROE2020))+geom_point()+
  geom_smooth(method='lm',formula=y~x)

ggplot(data_2020,aes(log(CO2Emissions2020), ROE2020))+geom_point()+
  geom_smooth(method='lm',formula=y~x)

#Is it possible to determine some relevance within particular sectors?
summary(lm_2020_trial1 <- lm(ROE2020 ~ ESGScore2020 + log(CO2Emissions2020), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Industrials")))

summary(lm_2020_trial2 <- lm(ROE2020 ~ ESGScore2020 + log(CO2Emissions2020), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Financials")))

summary(lm_2020_trial3 <- lm(ROE2020 ~ ESGScore2020 + log(CO2Emissions2020), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Healthcare")))

summary(lm_2020_trial4 <- lm(ROE2020 ~ ESGScore2020 + log(CO2Emissions2020), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Technology")))

summary(lm_2020_trial5 <- lm(ROE2020 ~ ESGScore2020 + log(CO2Emissions2020), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Basic Materials")))

#Even within some sectors we do not find relevance -> DO NOT REJECT NULL HP

#adding CONTROL VARIABLES, log taken because of large differences in distributions 
summary(lm_2020a <- lm(ROE2020 ~ ESGScore2020 + log(CO2Emissions2020), data = data_2020))

summary(lm_2020b <- lm(ROE2020 ~ ESGScore2020+
                         log(CO2Emissions2020) + 
                         log(TotalRevenue2020) + 
                         log(TotalAssets2020) +
                         log(EmployeesAv2020), data = data_2020)) 
#un-restricted model : checking for structural variables produces statistical significance
# in terms of C02 Emissions with an expected value of -0.012
# R-squared 0.05817 still little explanatory of how much ROE can change as dependent variable

#2021
summary(data_2021)
view(data_2021)

summary(lm_2021 <- lm(ROE2021 ~ ESGScore2021 + log(CO2Emissions2021), data = data_2021)) 
#A unit ESG increase translates into an expected increase in ROE of 0.04
#An increase of 1% in Emissions translates into an expected decrease in ROE of 0.0017
#No STATISTICAL RELEVANCE -> p-value = c(0.621, 0.712) -> R^2 = 0.0013 -> DO NOT REJECT NULL HP

#Graphic representation : ROE and ESG, ROE and CO2 Emissions 
ggplot(data_2021,aes(ESGScore2021, ROE2021))+geom_point()+
  geom_smooth(method='lm',formula=y~x)

ggplot(data_2021,aes(log(CO2Emissions2021), ROE2021))+geom_point()+
  geom_smooth(method='lm',formula=y~x)

#Is it possible to determine some relevance within particular sectors?
summary(lm_2021_trial1 <- lm(ROE2021 ~ ESGScore2021 + log(CO2Emissions2021), 
                             data = data_2021 %>% filter(TRBC_SectorName=="Industrials")))

summary(lm_2021_trial2 <- lm(ROE2021 ~ ESGScore2021 + log(CO2Emissions2021), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Financials")))

summary(lm_2021_trial3 <- lm(ROE2021 ~ ESGScore2021 + log(CO2Emissions2021), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Healthcare")))

summary(lm_2021_trial4 <- lm(ROE2021 ~ ESGScore2021 + log(CO2Emissions2021), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Technology")))

summary(lm_2021_trial5 <- lm(ROE2021 ~ ESGScore2021 + log(CO2Emissions2021), 
                             data = data_2020 %>% filter(TRBC_SectorName=="Basic Materials")))

#Even within some sectors we do not find relevance -> DO NOT REJECT NULL HP

#adding CONTROL VARIABLES, log taken because of large differences in distributions 
summary(lm_2021a <- lm(ROE2021 ~ ESGScore2021 + log(CO2Emissions2021), data = data_2021))

summary(lm_2021b <- lm(ROE2021 ~ ESGScore2021+
                         log(CO2Emissions2021) + 
                         log(TotalRevenue2021) + 
                         log(TotalAssets2021) +
                         log(EmployeesAv2021), data = data_2021)) 
#un-restricted model : checking for structural variables produces statistical significance
# in terms of C02 Emissions with an expected value of -0.012
# R-squared 0.05817 still little explanatory of how much ROE can change as dependent variable



