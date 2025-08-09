library(data.table)
library(ggplot2)
library(readxl)
library(lfe)
library(stringr)
library(stargazer)
library(lubridate)
library(segmented)
library(cowplot)

setwd("/Users/liujiawen/Downloads/RA_work/KasuwaGo/price/Other data")
price3<-data.table(read_excel("FEWS_NET_Staple_Food_Price_Data.xlsx"))
price3<-price3[country=="Nigeria"&(product=="Cowpeas (Brown)"|product=="Cowpeas (White)"|product=="Rice (5% Broken)"|product=="Rice (Milled)"),]
market1 <- data.table(read_excel("marketfew.xls"))

grow<-data.table(read_excel("growing season.xlsx"))

#merge two price data set
price3[,`:=`(date = as.POSIXct(period_date, format = "%Y-%m-%d"))][, `:=`(month= month(date))]
price3<-price3[!is.na(value),]
price3[,value:=as.numeric(value)]
price3 = price3[order(market,product,month),]
str(price3)
price3[unit=="kg",price:=value][unit=="100_kg",price:=value/100][unit=="50_kg",price:=value/50]
price3 = setkey(market1[,.(market,loc,Urban)],market)[setkey(price3,market)]


#################
#Calculate the months since harvest
price3 = merge(price3,grow[,.(Crops1,StartHarvest,EndHarvest,second.harvest,second.harvest2,Location)], by.x =c("product","loc") ,by.y=c("Crops1","Location"))

price3[,`:=`(month=month(month), harvestmonth = month(as.Date(EndHarvest,format="%m/%d/%y")), harvestmonth2 = month(as.Date(second.harvest2,format="%m/%d/%y")))] #why not end-harvest or in-between

price3[month>=harvestmonth,monthsinceharvest:=month-harvestmonth][month<harvestmonth,monthsinceharvest:=12+month-harvestmonth]

#Construct the price index for each market, month, and crop
#the twelve average monthly prices divided by the median observed price.
#Remove some extrem price that potentially entered errorly
#A common definition of an outlier is a value that is more than 1.5 times the Interquartile Range (IQR) below the first quartile or above the third quartile. 
price3<-price3 %>%
  group_by(product,month) %>%
  mutate(IQR = IQR(price, na.rm = TRUE),  # calculate IQR for each group
         Q1 = quantile(price, 0.25, na.rm = TRUE),  # calculate first quartile
         Q3 = quantile(price, 0.75, na.rm = TRUE)) %>%  # calculate third quartile
  filter((price >= (Q1 - 1.5 * IQR)) & (price <= (Q3 + 1.5 * IQR))) %>%  # filter outliers
  dplyr::select(-c(IQR, Q1, Q3))  # drop auxiliary columns

price3<-data.table(price3)

price4 = price3[!is.na(price),avep:=mean(price),by=.(month,product,market)]
price4 = price4[!is.na(price),medp:=median(price),by=.(product,market)]
price4 = price4[,pindex:=avep/medp]
price4_r<-price4[price_type=="Retail",]
price4_w<-price4[price_type=="Wholesale",]

price5<-price4_r

fit = lm(pindex ~ date, data=price5)
# Next, generate predicted values from this model
price5[,predicted:=predict(fit)]
# Detrend the data by subtracting the predicted values from the original data
price5[,detrended:=pindex-predicted]


#Regression
price5_avg = price5[,.(detrended = mean(detrended)),by=.(monthsinceharvest,itemgrp)]
segmented.fit  =  segmented(fit, seg.Z = ~monthsinceharvest, psi=3)
summary(segmented.fit)



#Using piecewise regression to fit
ggplot(price5, aes(x = monthsinceharvest, y = detrended)) +
  geom_point(data = price5_avg, aes(x = monthsinceharvest, y = detrended), color = "black") + # regression line
  scale_x_continuous(breaks = seq(from = min(price5$monthsinceharvest), to = max(price5$monthsinceharvest), by = 1)) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, df = 2, degree = 1),level=0.95) + theme_bw() + facet_wrap(~itemgrp) + 
  labs(x = "Months Since Harvest", y = "Price Index")

ggsave("SellingPriceSegItemGrp1.pdf", width =8 , height = 4)


