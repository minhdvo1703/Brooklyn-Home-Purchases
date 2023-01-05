#Name: Minh Vo
#MSCA 31007 - Final Assignment Part I
#Date: "11/30/2022"


library(dplyr)
library(plyr)
library(readr)
library(stringr)
library(leaps)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(tidyverse)
library(lmtest)
library(lubridate)
library(zoo)

# Part 1 #
######################## STEP 1: Import and prepare the data for analysis ###########################
setwd("C:/Users/Minh Dieu Vo/OneDrive - The University of Chicago/House Data")
#####  Bring the data into R 

###2016 data###
d2016 <- read.csv('2016_brooklyn.csv',skip = 4, header = TRUE)
names(d2016) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block',
                  'lot','easement','bldclasscurr','address','aptnum','zip',
                  'resunits','comunits','totunits','landsqft','grosssqft',
                  'yrbuilt','taxclasssale','bldclasssale','price','date')
str(d2016)
#Remove the hyphens/commas & Convert data types to integer
d2016$resunits <- as.integer(gsub("-","",d2016$resunits))
d2016$comunits <- as.integer(gsub("-","0",d2016$comunits))
d2016$totunits <- as.integer(gsub("-","",d2016$totunits))
d2016$price <- gsub(",","",d2016$price)
d2016$price <- as.integer(gsub("-","",d2016$price))
d2016$landsqft <- gsub(",","",d2016$landsqft)
d2016$landsqft <- as.integer(gsub("-","",d2016$landsqft))
d2016$grosssqft <- gsub(",","",d2016$grosssqft)
d2016$grosssqft <- as.integer(gsub("-","",d2016$grosssqft))

#Remove space from columns: neighborhood, bldclasscat, taxclasscurr, bldclasscurr, bldclasssale, address, aptnum
d2016$neighborhood <- trimws(d2016$neighborhood,which = c("both"))
d2016$bldclasscat <- trimws(d2016$bldclasscat,which = c("both"))
d2016$taxclasscurr <- trimws(d2016$taxclasscurr,which = c("both"))
d2016$bldclasscurr <- trimws(d2016$bldclasscurr,which = c("both"))
d2016$bldclasssale <- trimws(d2016$bldclasssale,which = c("both"))
d2016$address <- trimws(d2016$address,which = c("both"))
d2016$aptnum <- trimws(d2016$aptnum,which = c("both"))

#Reformat date column (Convert 4-digit year to 2-digit year)
#d2016$date <- gsub( "([0-9]{2})([0-9]{2})$" , "\\2" , d2016$date)

###2017 data###
d2017 <- read.csv('2017_brooklyn.csv',skip = 4, header = TRUE)
names(d2017) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block',
                  'lot','easement','bldclasscurr','address','aptnum','zip',
                  'resunits','comunits','totunits','landsqft','grosssqft',
                  'yrbuilt','taxclasssale','bldclasssale','price','date')
str(d2017)
#Remove the commas & Convert data types to integer
d2017$price <- as.integer(gsub(",","",d2017$price))
d2017$landsqft <- gsub(",","",d2017$landsqft)
d2017$landsqft <- as.integer(gsub("-","",d2017$landsqft))
d2017$grosssqft <- gsub(",","",d2017$grosssqft)
d2017$grosssqft <- as.integer(gsub("-","",d2017$grosssqft))

#Remove space from columns: neighborhood, bldclasscat, taxclasscurr, bldclasscurr, bldclasssale, address, aptnum
d2017$neighborhood <- trimws(d2017$neighborhood,which = c("both"))
d2017$bldclasscat <- trimws(d2017$bldclasscat,which = c("both"))
d2017$taxclasscurr <- trimws(d2017$taxclasscurr,which = c("both"))
d2017$bldclasscurr <- trimws(d2017$bldclasscurr,which = c("both"))
d2017$bldclasssale <- trimws(d2017$bldclasssale,which = c("both"))
d2017$address <- trimws(d2017$address,which = c("both"))
d2017$aptnum <- trimws(d2017$aptnum,which = c("both"))

###2018 data###
d2018 <- read.csv('2018_brooklyn.csv',skip = 4, header = TRUE)
names(d2018) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block',
                  'lot','easement','bldclasscurr','address','aptnum','zip',
                  'resunits','comunits','totunits','landsqft','grosssqft',
                  'yrbuilt','taxclasssale','bldclasssale','price','date')
str(d2018)
#Remove the commas & Convert data types to integer
d2018$resunits <- as.integer(d2018$resunits)
d2018$totunits <- as.integer(gsub(",","",d2018$totunits))
d2018$price <- gsub("[$]","",d2018$price) #remove the "$" in price
d2018$price <- gsub(",","",d2018$price)
d2018$price <- as.integer(d2018$price)
d2018$landsqft <- as.integer(gsub(",","",d2018$landsqft))
d2018$grosssqft <- as.integer(gsub(",","",d2018$grosssqft))

#Remove space from columns: neighborhood, bldclasscat, taxclasscurr, bldclasscurr, bldclasssale, address, aptnum
d2018$neighborhood <- trimws(d2018$neighborhood,which = c("both"))
d2018$bldclasscat <- trimws(d2018$bldclasscat,which = c("both"))
d2018$taxclasscurr <- trimws(d2018$taxclasscurr,which = c("both"))
d2018$bldclasscurr <- trimws(d2018$bldclasscurr,which = c("both"))
d2018$bldclasssale <- trimws(d2018$bldclasssale,which = c("both"))
d2018$address <- trimws(d2018$address,which = c("both"))
d2018$aptnum <- trimws(d2018$aptnum,which = c("both"))

###2019 data###
d2019 <- read.csv('2019_brooklyn.csv',skip = 4, header = TRUE)
names(d2019) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block',
                  'lot','easement','bldclasscurr','address','aptnum','zip',
                  'resunits','comunits','totunits','landsqft','grosssqft',
                  'yrbuilt','taxclasssale','bldclasssale','price','date')
str(d2019)
#Remove the commas & Convert data types to integer
d2019$price <- as.integer(gsub(",","",d2019$price))
d2019$landsqft <- as.integer(gsub(",","",d2019$landsqft))
d2019$grosssqft <- as.integer(gsub(",","",d2019$grosssqft))

#Remove the space
d2019$neighborhood <- trimws(d2019$neighborhood,which = c("both"))
d2019$bldclasscat <- trimws(d2019$bldclasscat,which = c("both"))
d2019$taxclasscurr <- trimws(d2019$taxclasscurr,which = c("both"))
d2019$bldclasscurr <- trimws(d2019$bldclasscurr,which = c("both"))
d2019$bldclasssale <- trimws(d2019$bldclasssale,which = c("both"))
d2019$address <- trimws(d2019$address,which = c("both"))
d2019$aptnum <- trimws(d2019$aptnum,which = c("both"))

###2020 data###
d2020 <- read.csv('2020_brooklyn.csv',skip = 6, header = TRUE)
names(d2020) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block',
                  'lot','easement','bldclasscurr','address','aptnum','zip',
                  'resunits','comunits','totunits','landsqft','grosssqft',
                  'yrbuilt','taxclasssale','bldclasssale','price','date')
d2020 <- d2020[-1,]
str(d2020)
#Remove the commas & Convert data types to integer
d2020$price <- as.integer(gsub(",","",d2020$price))
d2020$landsqft <- as.integer(gsub(",","",d2020$landsqft))
d2020$grosssqft <- as.integer(gsub(",","",d2020$grosssqft))

#Remove the space
d2020$neighborhood <- trimws(d2020$neighborhood,which = c("both"))
d2020$bldclasscat <- trimws(d2020$bldclasscat,which = c("both"))
d2020$taxclasscurr <- trimws(d2020$taxclasscurr,which = c("both"))
d2020$bldclasscurr <- trimws(d2020$bldclasscurr,which = c("both"))
d2020$bldclasssale <- trimws(d2020$bldclasssale,which = c("both"))
d2020$address <- trimws(d2020$address,which = c("both"))
d2020$aptnum <- trimws(d2020$aptnum,which = c("both"))

#combining data
homedata <- rbind(d2016,d2017,d2018,d2019,d2020)
str(homedata)

####Join the data and make it usable for analysis
newhomedata <- filter(homedata, startsWith(homedata$bldclasssale, "A") | startsWith(homedata$bldclasssale, "R"))
newhomedata <- filter(newhomedata, newhomedata$totunits == 1 & newhomedata$resunits == 1)
newhomedata <- filter(newhomedata, newhomedata$grosssqft > 0 & !is.na(newhomedata$price))
str(newhomedata)

# convert data types to Factor & date to Date
newhomedata$neighborhood <- as.factor(newhomedata$neighborhood)
newhomedata$zip <- as.factor(newhomedata$zip)
newhomedata$yrbuilt <- as.factor(newhomedata$yrbuilt)
newhomedata$bldclasscat <- as.factor(newhomedata$bldclasscat)
newhomedata$taxclasscurr <- as.factor(newhomedata$taxclasscurr)
newhomedata$taxclasssale <- as.factor(newhomedata$taxclasssale)
newhomedata$bldclasssale <- as.factor(newhomedata$bldclasssale)
#newhomedata$block <- as.factor(newhomedata$block)
#newhomedata$lot <- as.factor(newhomedata$lot)
#newhomedata$address <- as.factor(newhomedata$address)
#newhomedata$aptnum <- as.factor(newhomedata$aptnum)
newhomedata$date <- mdy(newhomedata$date)
######################## STEP 2: EDA and feature engineering ##################################
  ## 2.1 Exploratory data analysis

# number of unique values in each column
sapply(lapply(newhomedata,unique),length) 

#drop rows with sales price = 0
newhomedata <- subset(newhomedata, newhomedata$price > 0)

#detect and remove outliers in sales price (NOT DOING THIS)
#boxplot(sqrt(newhomedata$price))
#quartiles <- quantile(newhomedata$price, probs=c(.25, .75), na.rm = FALSE)
#IQR <- IQR(newhomedata$price)
#Lower <- quartiles[1] - 1.5*IQR
#Upper <- quartiles[2] + 1.5*IQR 
#newhomedata <- subset(newhomedata, newhomedata$price > Lower & newhomedata$price < Upper)

#### Trying grouping factors to decrease model df
# create a new variable grouping zipcode by region name 
#(Source: https://bklyndesigns.com/brooklyn-zip-codes/)
newhomedata2 <- newhomedata %>%
  mutate(
    neighborname = case_when(
      zip %in% c(11213, 11216, 11225, 11233, 11238, 11226, 11218, 11203) ~ "Central Brooklyn",
      zip %in% c(11212, 11233, 11236, 11207, 11208, 11239) ~ "Eastern Brooklyn",
      zip %in% c(11205, 11206, 11221, 11237, 11222, 11211, 11249) ~ "Northern Brooklyn",
      zip %in% c(11201, 11251, 11205) ~ "Northwestern Brooklyn",
      zip %in% c(11217, 11231, 11201, 11215) ~ "South Brooklyn",
      zip %in% c(11234, 11224, 11229, 11235, 11210, 11230, 11223) ~ "Southern Brooklyn",
      zip %in% c(11209, 11220, 11204, 11214, 11218, 11219, 11228, 11232) ~ "Southwestern Brooklyn")) %>%
  group_by(neighborname)
newhomedata2$neighborname <- factor(newhomedata2$neighborname)

# grouping zip code by median household income 
#(Source: ACS 5-year survey in 2020 (2015-2020) - https://www.point2homes.com/US/Neighborhood/NY/New-York-City-Demographics.html)
#zip_medincome <- read.csv('brooklyn income by zip.csv',header = TRUE)
#str(zip_medhhic)
#zip_medincome <- zip_medincome %>%
#  mutate(
#    zip_medincome = case_when(
#      between(median.income, 20000, 40000) ~ "zip1",
#      between(median.income, 40000, 60000) ~ "zip2",
#      between(median.income, 60000, 80000) ~ "zip3",
#      between(median.income, 80000, 100000) ~ "zip4",
#      median.income > 100000 ~ "zip5")) %>%
#  group_by(zip_medincome)
#zip_medincome$zip_medincome <- factor(zip_medincome$zip_medincome)
#newhomedata2 <- merge(newhomedata2,zip_medincome,by = 'zip', all=TRUE)



####***grouping by the sqrt sales price
newhomedata$sqrt_price <- sqrt(newhomedata$price) # calculate square root of price and add to the dataset
newhomedata <- newhomedata %>%
  mutate(
    group = case_when(
      sqrt_price < 600 ~ "group1",
      between(sqrt_price, 600, 800) ~ "group2",
      between(sqrt_price, 800, 1000) ~ "group3",
      between(sqrt_price, 1000, 1200) ~ "group4",
      between(sqrt_price, 1200, 1400) ~ "group5",
      sqrt_price > 1400 ~ "group6")) %>%
  group_by(group)
newhomedata$group <- factor(newhomedata$group)


  ## 2.2 Pre-modeling and feature engineering
#using the newhomedata2 with added explanatory variables from external sources
# calculate correlation to understand the relationship between price and other numeric variables
# price and grosssqft have the highest correlation although no correlation coefficient > .5
newhomedata_numeric <- newhomedata2[,c("price","landsqft","grosssqft","block", "lot","population","number.of.households","median.income","average.income")]
cor(newhomedata_numeric)
hist.data.frame(newhomedata_numeric)

lm1 <- lm(price~neighborname+zipgroup+bldclasssale+grosssqft+landsqft+median.income+average.income+population,data = newhomedata2)
summary(lm1) #R^2=0.4039;df=29;RMSE=337288.1
sqrt(mean((newhomedata2$price-lm1$fitted.values)^2))

lm2 <- lm(price~neighborname*median.income+zipgroup*average.income+population+grosssqft,data = newhomedata2)
summary(lm2) #R^2=0.4205;df=23;RMSE=332567.6
sqrt(mean((newhomedata2$price-lm2$fitted.values)^2))

dwtest(lm2,alternative = "two.sided") #p-value = 0.4795
bptest(lm2)
ks.test(lm2$residuals/summary(lm2)$sigma,pnorm)

lm3 <- lm(price~neighborname*grosssqft+median.income*zipgroup+bldclasssale,data = newhomedata2)
summary(lm3) #R^2=0.4119;df=40;RMSE=335030.6
sqrt(mean((newhomedata2$price-lm3$fitted.values)^2))

lm4 <- lm(price~neighborname*population+median.income*zipgroup+grosssqft*bldclasssale,data = newhomedata2)
summary(lm) #R^2=0.4455;df=49;RMSE=325320.6
sqrt(mean((newhomedata2$price-lm$fitted.values)^2))

#----------------------------------------------------------------------------------------# 

#Using the newhomedata containing groups by squart root of sales price
lm_1 <- lm(price~group+grosssqft+landsqft+bldclasssale,data = newhomedata)
summary(lm_1)   #R^2=0.7595;df=21;RMSE=431976.8
summary(lm_1)$adj.r.squared
sqrt(mean((newhomedata$price-lm_1$fitted.values)^2))
#all p-values < 2.2e-16
dwtest(lm_1,alternative = "two.sided")
bptest(lm_1)
ks.test(lm_1$residuals/summary(lm_1)$sigma,pnorm)

lm_2 <- lm(price~group*grosssqft+landsqft+bldclasssale,data = newhomedata)
summary(lm_2)  #R^2=0.7971;df=26;RMSE=396833.4
summary(lm_2)$adj.r.squared
sqrt(mean((newhomedata$price-lm_2$fitted.values)^2))

lm_3 <- lm(price~group+grosssqft*bldclasssale,data = newhomedata)
summary(lm_3)  #R^2=0.7884;df=33;RMSE=405221.7
summary(lm_3)$adj.r.squared
sqrt(mean((newhomedata$price-lm_3$fitted.values)^2))
dwtest(lm_3,alternative = "two.sided") #0.4795
#p-values < 2.2e-16
bptest(lm_3)
ks.test(lm_3$residuals/summary(lm_3)$sigma,pnorm)

lm_4 <- lm(price~group*grosssqft+landsqft*bldclasssale,data = newhomedata)
summary(lm_4)  #R^2=0.8012;df=39;RMSE=392741.7
summary(lm_4)$r.squared
sqrt(mean((newhomedata$price-lm_4$fitted.values)^2))

lm_5 <- lm(price~group*grosssqft+landsqft+bldclasssale,data = newhomedata)
summary(lm_5)  #R^2=0.7969;df=25;RMSE=397025.1
summary(lm_5)$adj.r.squared
sqrt(mean((newhomedata$price-lm_5$fitted.values)^2))

lm_6 <- lm(price~group+grosssqft+group*grosssqft,data = newhomedata)
summary(lm_6) #R^2=0.7895;df=11;RMSE=404165.7
summary(lm_6)$adj.r.squared
sqrt(mean((newhomedata$price-lm_6$fitted.values)^2))

##test models##
lm <- lm(price~zipgroup*grosssqft,data = newhomedata)
summary(lm)
sqrt(mean((newhomedata$price-lm$fitted.values)^2))
######################## STEP 3: Submit model and datasets ############################
saveRDS(list(model=lm(price~group*grosssqft+landsqft+bldclasssale,data = newhomedata)), file='minhvo.RDS')






# Part 2 #
### add quarter column to dataset
newhomedata4 <- newhomedata
#newhomedata4$quarter <- quarters(newhomedata4$date)
#newhomedata4$quarter <- as.factor(newhomedata4$quarter)
newhomedata4$yr_quarter <-paste0(year(newhomedata4$date),'/0',quarter(newhomedata4$date))
newhomedata4$quarter <- quarter(newhomedata4$date)
newhomedata4$quarter <- as.factor(newhomedata4$quarter)
newhomedata4$yr_quarter <- as.factor(newhomedata4$yr_quarter)
#newhomedata4$yr_mon <-paste0(year(newhomedata4$date),'/',month(newhomedata4$date))
#newhomedata4$yr_mon <- as.factor(newhomedata4$yr_mon)
#newhomedata4$yr_quarter <- as.yearqtr(newhomedata$date)
newhomedata4$yr_mon <- as.yearmon(newhomedata$date)
newhomedata4$yr_month <-paste0(year(newhomedata4$date),'/',month(newhomedata4$date))

price_change <- newhomedata4 %>% 
  group_by(yr_quarter) %>% 
  #filter(quarter == 3) %>%
  summarise_at(vars(price),list(mean_price = mean))
ggplot(data = filter(price_change, yr_month),aes(yr_month,mean_price))+
  geom_smooth(method = loess) +
  labs(title = "Average Home Sale Price")

homeprice_Q34_2020 <- newhomedata4 %>% 
  group_by(yr_mon) %>% 
  filter(date >= '2020-07-01' & date <= '2020-12-31') %>%
  summarise_at(vars(price),list(mean_price = mean))
ggplot(data = homeprice_Q34_2020,aes(yr_mon,mean_price))+
  #geom_line() + 
  geom_smooth(method = loess) +
  labs(title = "Average Home Sale Price during Q3/2020 and Q4/2020")

homeprice_month <- newhomedata4 %>% 
  group_by(yr_mon) %>% 
  filter(date >= '2020-07-01' & date <= '2020-12-31') %>%
  summarise_at(vars(price),list(mean_price = mean))
ggplot(data = homeprice_Q34_2020,aes(yr_mon,mean_price))+
  #geom_line() + 
  geom_smooth(method = loess) +
  labs(title = "Average Home Sale Price during Q3/2020 and Q4/2020")


Q2020 <- newhomedata4 %>% 
  group_by(yr_quarter) %>% 
  filter(date >= '2020-01-01' & date < '2021-01-01') %>%
  summarise_at(vars(price),list(avgprice_month = mean))
#plot the home sales price from Q3 to Q4/2020
ggplot(data = Q2020,aes(yr_quarter,avgprice_month))+geom_line()



#homeprice_Q34_2020 <- filter(newhomedata4, date >= '2020-07-01' & date < '2021-01-01')
lm_q34 <- lm(price~yr_quarter*grosssqft++group+bldclasssale+landsqft,data = newhomedata4)
summary(lm_q34)
sqrt(mean((newhomedata4$price-lm_q34$fitted.values)^2))
anova(lm_q34)

plot(lm_q34$fitted.values)


#plot the home sales price prediction using grosssqft of Q3 vs Q4
ggplot()+
  geom_smooth(data = filter(newhomedata4,date >= '2020-07-01' & date < '2020-10-01'),aes(grosssqft,sqrt(price)), method = "lm") +
  geom_smooth(data = filter(newhomedata4,date >= '2020-10-01' & date < '2021-01-31'),aes(grosssqft,sqrt(price)), color = "red", method = "lm") +
  labs(title = "Home Sale Price by Gross Square Feet between Q3/2020 and Q4/2020", subtitle = "Blue: Q3/2020,Red: Q4/2020") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5))


