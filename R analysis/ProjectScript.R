# Created by Diep Phan on 9/20/2021 (perovicj@beloit.edu)
# Last modified on 12/11/2021 by Jagos Perovic
# Purpose: Analyze data for for final project in ECON380

# Topics covered: 
# 1. setting up work space, loading files
# 2. manipulating data
# 3. producing basic descriptive statistics
# 4. visualization
# 5. regressions


#***************** SET UP WORKING SPACE ********************#
getwd()
setwd("C:/Users/Jagos/Documents/ECON 380/R analysis")
getwd()
library(tidyverse)
library(Hmisc)
library(statip)
library(stargazer)
library(coefplot)
library(jtools)
library(sandwich)
library(ggstance)
library(broom.mixed)
library(huxtable)
library(plyr)
library(plm)
library(corrplot)
library(zoo)
library(lmtest)
library(plm)
library(readxl)
library(writexl)
library(ggthemes)
migr <- read.csv("immigration.csv")
pat <- read.csv("patents.csv")
rnd <- read.csv('r&d.csv')
pop <- read.csv('pop.csv')
popden <- read.csv('pop_dens.csv')
hrst <- read.csv('HRST.csv')
tranvol <- read.csv('tranvol.csv')
httrade <- read.csv("httrade.csv")
inFDI <- read.csv("InFDI.csv")
httex <- read.csv("httex.csv")
gdp <- read.csv("gdp.csv")
gdpcap <- read.csv("gdpcap.csv")
emp <- read.csv("emp.csv")


#Dropping unecessarry columns, changing column names, and filtering 
#Filtering results to have total values for countries as some tables such as rnd have spending by sector. Everything is total and national level after filtering.
emp <- select(emp,-DATAFLOW, -LAST.UPDATE, -freq, -OBS_FLAG, -nace_r2, -unit, -indic_sb)
emp <- dplyr::rename(emp, emp = OBS_VALUE)

gdpcap <-subset(gdpcap, gdpcap$geo != "EU27_2020" & gdpcap$geo != "EU27_2007")

gdpcap <- select(gdpcap,-DATAFLOW, -LAST.UPDATE, -freq, -OBS_FLAG, -na_item, -ppp_cat)
gdpcap <- dplyr::rename(gdpcap, gdpcap = OBS_VALUE)

gdp <- subset(gdp, gdp$unit != "CP_EUR_HAB" & gdp$geo != "EA19" & gdp$unit != "CP_MEUR")
gdp <- select(gdp, -DATAFLOW, -LAST.UPDATE, -freq, -unit, -OBS_FLAG, -na_item)
gdp <- dplyr::rename(gdp, gdp = OBS_VALUE)

httex <- select(httex, -DATAFLOW, -LAST.UPDATE, -freq, -unit, -OBS_FLAG)
httex <- dplyr::rename(httex, httex = OBS_VALUE)

inFDI <- select(inFDI, -DATAFLOW, -LAST.UPDATE, -freq, -OBS_FLAG, -entity, -indic_bp,-fdi_item,-partner)
inFDI <- dplyr::rename(inFDI, inFDI = OBS_VALUE)

migr <- subset(migr, migr$agedef != "REACH")
migr <- select(migr, -DATAFLOW,-LAST.UPDATE,-age,-agedef, -citizen, -sex,-unit,-OBS_FLAG,-freq,)
migr <- dplyr::rename(migr, migr = OBS_VALUE)

httrade <- subset(httrade, httrade$unit != "PC_TOT" & httrade$partner != "EXT_EU28"
                  & httrade$partner != "INT_EU28" & httrade$stk_flow != "EXP")
httrade <- select(httrade, -DATAFLOW,-LAST.UPDATE,-freq,-stk_flow,-unit,-OBS_FLAG,-partner)
httrade <- dplyr::rename(httrade, httrade = OBS_VALUE)

pat <- subset(pat, pat$unit != "P_MHAB")
pat <- select(pat,-DATAFLOW,-LAST.UPDATE,-OBS_FLAG,-freq,-unit)
pat <- dplyr::rename(pat, pat = OBS_VALUE)

rnd <- subset(rnd, rnd$sectperf != 'GOV' & rnd$sectperf != 'HES' & rnd$sectperf != 'PNP' & rnd$sectperf != 'BES')
rnd <- select(rnd,-DATAFLOW,-LAST.UPDATE,-freq,-OBS_FLAG,-sectperf,-unit)
rnd <- dplyr::rename(rnd, rnd = OBS_VALUE)


hrst <- select(hrst,-DATAFLOW,-LAST.UPDATE,-OBS_FLAG,-unit,-age,-category,-freq,-sex)
hrst <- dplyr::rename(hrst, hrst = OBS_VALUE)

pop <- select(pop, -DATAFLOW,-LAST.UPDATE,-OBS_FLAG,-indic_de,-freq)
pop <- dplyr::rename(pop, pop = OBS_VALUE)

popden <- select(popden,-DATAFLOW,-LAST.UPDATE,-OBS_FLAG,-unit,-freq)
popden <- dplyr::rename(popden, popden = OBS_VALUE)

tranvol <- select(tranvol,-DATAFLOW,-LAST.UPDATE,-OBS_FLAG,-freq,-unit)
tranvol <- dplyr::rename(tranvol, tranvol = OBS_VALUE)


#***************** Merging all the datasets ********************#
data <- merge(pat, migr, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data2 <- merge(data, emp, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data3 <- merge(data2, gdp, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data4 <- merge(data3, gdpcap, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data5 <- merge(data4, httex, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data6 <- merge(data5, httrade, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data7 <- merge(data6, inFDI, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data8 <- merge(data7, pop, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data9 <- merge(data8, popden, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data10 <- merge(data9, rnd, by=c("geo", "TIME_PERIOD"), all.x=TRUE)
data11 <- merge(data10, tranvol, by=c("geo", "TIME_PERIOD"), all.x=TRUE)

df <- data11
df <- subset(df, df$geo != "EU27_2020" & df$geo != "EU28" & df$geo != "IL" & df$geo != "IN" & df$geo != "KR" &
               df$geo != "SG" & df$geo != "US" & df$geo != "ZA" & df$geo != "AU" & df$geo != "BR" & df$geo != "CA" &
               df$geo !=  "CN_X_HK" & df$geo !=  "HK" & df$geo !=  "JP" &
               df$geo !=  "TW" & df$geo !=  "MX" & df$geo !=  "NZ" )   #Most of these countries are trading partners and showed up in the dataset. They were removed as they are not European. For example NZ is New Zealand.
#Putting in better columnn names
df <- dplyr::rename(df, country = geo)
df <- dplyr::rename(df, year = TIME_PERIOD)
df <- dplyr::rename(df, httim = httrade)  
summary(df)


#***************** Visualization ********************#
#Creating correlation plot
corrdf <- select(df, -country)
res <- cor(corrdf, use = "complete.obs")
#corrplot(res, method = 'color', order = 'alphabet')
corrplot(res, method = 'number')

#Scatterplots & Histograms
ggplot(df, aes(x=migr, y=pat)) + geom_point() + geom_smooth(method=lm, se=FALSE)+ theme_stata()+
  xlim(5000, 60000) + ylim(0, 1500) + ggtitle("Immigration vs Patents")+ 
  labs(y="Number of Patents", x = "Number of Immigrants")

hist(df$pat, xlab="Number of Patents", 
     main ="Histogram of Number Patent Applications")

hist(df$migr, xlab="Number of Immigrants", 
     main ="Histogram of Number of Immigrants")


ggplot(df, aes(x=migr, y=gdp)) + geom_point() + geom_smooth(method=lm, se=FALSE) + theme_stata()+
  xlim(0, 500000) + ylim(0,3e+06) + ggtitle("Immigration vs GDP")+ 
  labs(y="GDP", x = "Number of Immigrants")

ggplot(df, aes(x=pat)) + geom_histogram(color="black", fill="lightblue", bins=55)+theme_stata()+
  ggtitle("Histogram of Number Patent Applications")+ 
  labs(y="Frequency", x = "Number of Patents")+ xlim(0,6000)

ggplot(df, aes(x=migr)) + geom_histogram(color="black", fill="lightblue", bins=55)+theme_stata()+
  ggtitle("Histogram of Number of Immigrants")+ 
  labs(y="Frequency", x = "Number of Immigrants")+ xlim(0,750000)



#OLS Regression & Results Reporting
m1 <- lm(log10(pat)~log10(migr), df)
m2 <- lm(log10(pat)~log10(migr)+log10(rnd), df)
m3 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop), df)
m4 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden), df)
m5 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol), df)
m6 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim), df)
m7 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+ log10(gdp), df)
m8 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+ log10(gdp)+log10(gdpcap), df)           
m9 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+ log10(gdp)+log10(gdpcap)+log10(httex), df)
m10 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+ log10(gdp)+log10(gdpcap)+log10(httex)+log10(emp), df)  
m11 <- lm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+ log10(gdp)+log10(gdpcap)+log10(httex)+log10(emp)+log10(inFDI), df) 
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11, title="ols", align=TRUE, type="text", out="text_results.txt")
coefplot(m11, intercept=FALSE, outerCI=1.96)

summ(m11, robust = "HC1", confint = TRUE)

par(mfrow = c(2, 2))
plot(m11)
coeftest(m11, vcov = vcovHC(m11, "HC1")) #Heteroskedacisity test

#Colinearity test
x=df[,4:14]
y=df[3]
omcdiag(x,y)


#Pannel Data Regression

pdata <- pdata.frame(df, index=c('country', 'year'))
fixed_effect1 <- plm(log10(pat)~log10(migr),
                    data=pdata, model = 'within')
fixed_effect2 <- plm(log10(pat)~log10(migr)+log10(rnd),
                    data=pdata, model = 'within')
fixed_effect3 <- plm(log10(pat)~log(migr)+log10(rnd)+log10(pop),
                    data=pdata, model = 'within')                    
fixed_effect4 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden),
                    data=pdata, model = 'within')
fixed_effect5 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol),
                    data=pdata, model = 'within')
fixed_effect6 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim),
                    data=pdata, model = 'within')
fixed_effect7 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp),
                    data=pdata, model = 'within')
fixed_effect8 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp)+log10(gdpcap),
                    data=pdata, model = 'within')
fixed_effect9 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp)+log10(gdpcap)+log10(httex),
                    data=pdata, model = 'within')
fixed_effect10 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp)+log10(gdpcap)+log10(httex)+log10(emp),
                    data=pdata, model = 'within')
fixed_effect11 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp)+log10(gdpcap)+log10(httex)+log10(emp)+log10(inFDI),
                    data=pdata, model = 'within')
stargazer(fixed_effect1,fixed_effect2,fixed_effect3,fixed_effect4,fixed_effect5,,fixed_effect6,fixed_effect7,fixed_effect8,fixed_effect9,fixed_effect10,fixed_effect11, title = "Fixed Effect Results", align = TRUE, type='text', out='fixed_effect_results')
par(mfrow = c(2, 2))
plot(fixed_effect11)
coeftest(fixed_effect11, vcov = vcovHC(fixed_effect11, "HC1")) #Heteroskedacisity test



random_effect1 <- plm(log10(pat)~log10(migr),
                    data=pdata, model = 'random')
random_effect2 <- plm(log10(pat)~log10(migr)+log10(rnd),
                    data=pdata, model = 'random')
random_effect3 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop),
                    data=pdata, model = 'random')                   
random_effect4 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden),
                    data=pdata, model = 'random')
random_effect5 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol),
                    data=pdata, model = 'random')
random_effect6 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim),
                    data=pdata, model = 'random')
random_effect7 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp),
                    data=pdata, model = 'random')
random_effect8 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp)+log10(gdpcap),
                    data=pdata, model = 'random')
random_effect9 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp)+log10(gdpcap)+log10(httex),
                    data=pdata, model = 'random')
random_effect10 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp)+log10(gdpcap)+log10(httex)+log10(emp),
                    data=pdata, model = 'random')
random_effect11 <- plm(log10(pat)~log10(migr)+log10(rnd)+log10(pop)+log10(popden)+log10(tranvol)+log10(httim)+log10(gdp)+log10(gdpcap)+log10(httex)+log10(emp)+log10(inFDI),
                    data=pdata, model = 'random')
stargazer(random_effect1,random_effect2,random_effect3,random_effect4,random_effect5,random_effect6,random_effect7,random_effect8,random_effect9,random_effect10,random_effect11, title = "Random Effect Regression Results", align = TRUE, type='text', out='random_effect_results')

par(mfrow = c(2, 2))
plot(random_effect11)
coeftest(random_effect11, vcov = vcovHC(random_effect11, "HC1")) #Heteroskedacisity test


#Hausman Test
phtest(fixed_effect11, random_effect11)

