#This code is to generate figure 2
library(data.table)
library(ggplot2)
library(readxl)
library(lfe)
library(stringr)
library(stargazer)
library(lubridate)
library(segmented)
library(cowplot)

setwd("/Volumes/Extreme SSD/Joyce/My research/Plosone_seasonality/Data")

price4<-fread("price_dt.csv")
#Remove some extreme price that potentially entered by mistakes
#A common definition of an outtlier is a value that is more than 1.5 times the Interquartile Range (IQR) below the first quartile or above the third quartile. 
price4<-price4 %>%
  group_by(itemid0,monthsinceharvest) %>%
  mutate(IQR = IQR(price, na.rm = TRUE),  # calculate IQR for each group
         Q1 = quantile(price, 0.25, na.rm = TRUE),  # calculate first quartile
         Q3 = quantile(price, 0.75, na.rm = TRUE)) %>%  # calculate third quartile
  filter((price >= (Q1 - 1.5 * IQR)) & (price <= (Q3 + 1.5 * IQR))) %>%  # filter outliers
  dplyr::select(-c(IQR, Q1, Q3))  # drop auxiliary columns

price4<-data.table(price4)

price4 = price4[!is.na(price),avep:=mean(price),by=.(monthsinceharvest,itemid0,market)]
price4 = price4[!is.na(price),medp:=median(price),by=.(itemid0,market)]
price4 = price4[,pindex:=avep/medp]
price5 = unique(price4[,.(pindex,monthsinceharvest,market,itemid0,itemname0,loc,Urban)]) 
price5 = price5[pindex<4&!is.na(pindex)&!is.na(monthsinceharvest),]
fit = lm(pindex ~ monthsinceharvest, data=price5)
# Next, generate predicted values from this model
price5[,predicted:=predict(fit)]
# Detrend the data by subtracting the predicted values from the original data
price5[,detrended:=pindex-predicted]
price5[itemid0<4, itemgrp:="Cowpea"][itemid0==4, itemgrp:="Local Rice"][itemid0==5, itemgrp:="Long-grain Rice"][itemid0>5, itemgrp:="Other Items"]# and specific items  to itemgrps

#Regression
price5_avg = price5[,.(detrended = mean(detrended)),by=.(monthsinceharvest,itemgrp)]
segmented.fit  =  segmented(fit, seg.Z = ~monthsinceharvest, psi=3)
summary(segmented.fit)



#Using piecewise regression to fit
ggplot(price5[itemgrp!="Other Items"], aes(x = monthsinceharvest, y = detrended)) +
  geom_point(data = price5_avg[itemgrp!="Other Items"], aes(x = monthsinceharvest, y = detrended), color = "black") + # regression line
  geom_smooth(data = subset(price5[itemgrp!="Other Items"],itemgrp=="Cowpea"), 
              method = lm, 
              formula = y ~ splines::bs(x, df = 2, degree = 1,knots = c(5)), 
              se = TRUE, 
              fullrange = FALSE, 
              level = 0.95) +
  geom_smooth(data = subset(price5[itemgrp!="Other Items"],itemgrp=="Local Rice"), 
              method = lm, 
              formula = y ~ splines::bs(x, df = 2, degree = 1, knots = c(3,5,9)), 
              se = TRUE, 
              fullrange = FALSE, 
              level = 0.95) +
  geom_smooth(data = subset(price5[itemgrp!="Other Items"],itemgrp=="Long-grain Rice"), 
              method = lm, 
              formula = y ~ splines::bs(x, df = 2, degree = 1,knots = c(5)), 
              se = TRUE, 
              fullrange = FALSE, 
              level = 0.95) +
  scale_x_continuous(breaks = seq(from = min(price5$monthsinceharvest), to = max(price5$monthsinceharvest), by = 1)) +
  facet_wrap(~itemgrp , ncol = 3) +
  labs(x = "Months Since Harvest", y = "Price Index")+theme_bw()

 ggsave("SellingPriceSegItemGrp.pdf", width =8 , height = 4)



 

