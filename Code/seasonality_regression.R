#This code replicate table 3 as well as the stationary test in the appendix
library(data.table)
library(ggplot2)
library(fixest)
library(xtable)
library(stargazer)
library(readxl)
library(plm)


#import our data
price4_1<-fread("/Volumes/Extreme SSD/Joyce/My research/Plosone_seasonality/Data/price_season_analysis1.csv")
#The definition of our main variables in regression
#You don't need to run it again.
#setkey(price4,mkt1,mkt2,itemname0,week)
#price4[,pricediff:=log(price)-log(price2)]
#price4[,l1week:=shift(week)+1,by=.(mkt1,mkt2,itemid0)]
#price4[,l1pricediff:=shift(pricediff),by=.(mkt1,mkt2,itemid0)]
#price4[,y:=pricediff-l1pricediff]

#price4_1[,w1 := l1pricediff*samezone]
#price4_1[,w2 := l1pricediff*(Distance-mean(Distance))]
#price4_1[,w3 := l1pricediff*(Time-mean(Time))]
#price4_1[,hh:=0][harvest==1&harvest2==1,hh:=1][,hn:=0][(harvest==1&harvest2==0)|(harvest==0&harvest2==1),hn:=1][,nn:=0][harvest==0&harvest2==0,nn:=1]
#price4_1[hh==1,season:="hh"][hn==1,season:="hn"][nn==1,season:="nn"]


# #Market Integration in different seasons
price4_0<-price4_1[(direct_connection==1|indirect_connection==1)]
rice1.fe <- feols(y~factor(season):l1pricediff+w1+w2+w3| pairid+week, price4_0[itemname0=="Rice (local – no stone)"])
r1.base <- summary(rice1.fe)
summary(rice1.fe)

linearHypothesis(rice1.fe, "factor(season)hh:l1pricediff= factor(season)hn:l1pricediff")
linearHypothesis(rice1.fe, "factor(season)hh:l1pricediff= factor(season)nn:l1pricediff")
linearHypothesis(rice1.fe, "factor(season)hn:l1pricediff= factor(season)nn:l1pricediff")

#half-life
rice2.fe <-feols(y~factor(season):l1pricediff+w1+w2+w3| pairid+week, price4_0[itemname0=="Rice (long grain)"])
r2.base <- summary(rice2.fe)

linearHypothesis(rice2.fe, "factor(season)hh:l1pricediff= factor(season)hn:l1pricediff")
linearHypothesis(rice2.fe, "factor(season)hh:l1pricediff= factor(season)nn:l1pricediff")
linearHypothesis(rice2.fe, "factor(season)hn:l1pricediff= factor(season)nn:l1pricediff")


cowpea1.fe <- feols(y~factor(season):l1pricediff+w1+w2+w3| pairid+week, price4_0[itemname0=="Brown beans (cowpeas)"])
c1.base <- summary(cowpea1.fe)

linearHypothesis(cowpea1.fe,  "factor(season)hh:l1pricediff= factor(season)hn:l1pricediff")
linearHypothesis(cowpea1.fe, "factor(season)hh:l1pricediff= factor(season)nn:l1pricediff")
linearHypothesis(cowpea1.fe, "factor(season)hn:l1pricediff= factor(season)nn:l1pricediff")



cowpea2.fe <- feols(y~factor(season):l1pricediff+w1+w2+w3| pairid+week, price4_0[itemname0=="White beans (cowpeas)"])
c2.base <- summary(cowpea2.fe)

linearHypothesis(cowpea2.fe,  "factor(season)hh:l1pricediff= factor(season)hn:l1pricediff")
linearHypothesis(cowpea2.fe, "factor(season)hh:l1pricediff= factor(season)nn:l1pricediff")
linearHypothesis(cowpea2.fe, "factor(season)hn:l1pricediff= factor(season)nn:l1pricediff")

linear_tab_1weeklag_bothconnect_ruraltrural <- etable(r1.base,r2.base,c1.base,c2.base)
xtable(linear_tab_1weeklag_bothconnect_ruraltrural)



#stationary test

#Local rice
test_data<-price4_1[itemname0=="Rice (local – no stone)",.(pairid,week,pricediff)]
test_data<-unique(test_data)
sum(is.na(test_data$pricediff))
pairid_counts <- test_data %>%
  group_by(pairid) %>%
  summarise(week_count = n_distinct(week)) %>%
  ungroup()

balanced_subset <- test_data  %>%
  filter(pairid %in% pairid_counts$pairid[pairid_counts$week_count >=6])

balanced_subset<-pdata.frame(balanced_subset,index=c("pairid","week"))

purtest(balanced_subset$pricediff,pmax = 1, exo = "intercept", test = "madwu")

#Long grain rice
test_data<-price4_1[itemname0=="Rice (long grain)",.(pairid,week,pricediff)]
test_data<-unique(test_data)
sum(is.na(test_data$pricediff))
pairid_counts <- test_data %>%
  group_by(pairid) %>%
  summarise(week_count = n_distinct(week)) %>%
  ungroup()

balanced_subset <- test_data  %>%
  filter(pairid %in% pairid_counts$pairid[pairid_counts$week_count >=6])

balanced_subset<-pdata.frame(balanced_subset,index=c("pairid","week"))

purtest(balanced_subset$pricediff,pmax = 1, exo = "intercept", test = "madwu")

#Brown beans (cowpeas)
test_data<-price4_1[itemname0=="Brown beans (cowpeas)",.(pairid,week,pricediff)]
test_data<-unique(test_data)
sum(is.na(test_data$pricediff))
pairid_counts <- test_data %>%
  group_by(pairid) %>%
  summarise(week_count = n_distinct(week)) %>%
  ungroup()

balanced_subset <- test_data  %>%
  filter(pairid %in% pairid_counts$pairid[pairid_counts$week_count >=6])

balanced_subset<-pdata.frame(balanced_subset,index=c("pairid","week"))

purtest(balanced_subset$pricediff,pmax = 1, exo = "intercept", test = "madwu")

#White beans (cowpeas)
test_data<-price4_1[itemname0=="White beans (cowpeas)",.(pairid,week,pricediff)]
test_data<-unique(test_data)
sum(is.na(test_data$pricediff))
pairid_counts <- test_data %>%
  group_by(pairid) %>%
  summarise(week_count = n_distinct(week)) %>%
  ungroup()

balanced_subset <- test_data  %>%
  filter(pairid %in% pairid_counts$pairid[pairid_counts$week_count >=6])

balanced_subset<-pdata.frame(balanced_subset,index=c("pairid","week"))

purtest(balanced_subset$pricediff,pmax = 1, exo = "intercept", test = "madwu")

