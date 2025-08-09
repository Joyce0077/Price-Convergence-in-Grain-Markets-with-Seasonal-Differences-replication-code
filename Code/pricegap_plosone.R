#This code is to generate summmary Table 2 and Appendix Table 4 
library(lfe)
library(stargazer)
library(readr)
library(data.table)
library(ggplot2)
library(readxl)
library(lfe)
library(stringr)
library(stargazer)
library(lubridate)
library(segmented)
library(dplyr)

setwd("")
marketpair1<-fread("marketpair_plosone.csv")

#######Appendix Table 4################
##################
fe_model1 <- felm(pricegap ~ logDistance+log(size1)+log(size2)  | week+itemgrp+Zone  | 0 |0, data = marketpair1[Distance!=0&Time!=0,])
summary(fe_model1)
fe_model2 <- felm(pricegap ~ logTime+log(size1)+log(size2) |  week+itemgrp+Zone | 0 |0, data = marketpair1[Distance!=0&Time!=0,])
summary(fe_model2)
fe_model3 <- felm(pricegap ~ logDistance + logTime+log(size1)+log(size2) |  week+itemgrp+Zone | 0 |0, data = marketpair1[Distance!=0&Time!=0,])
summary(fe_model3)
fe_model4 <- felm(pricegap ~ logDistance + logTime+log(size1)+log(size2) |  week+Zone | 0 |0, data = marketpair1[Distance!=0&Time!=0&itemgrp=="Cowpea",])
summary(fe_model4)
fe_model5 <- felm(pricegap ~ logDistance + logTime+log(size1)+log(size2) |  week+Zone | 0 |0, data = marketpair1[Distance!=0&Time!=0&itemgrp=="Local Rice",])
summary(fe_model5)
fe_model6 <- felm(pricegap ~ logDistance + logTime+log(size1)+log(size2) |  week+Zone | 0 |0, data = marketpair1[Distance!=0&Time!=0&itemgrp=="Long-grain Rice",])
summary(fe_model6)

#first stage
stargazer(fe_model1,fe_model2 ,fe_model3,fe_model4,fe_model5 ,fe_model6  ,type = "latex")


#########Summary Table 2################
library(dplyr)
library(knitr)

summary_table <- marketpair1[!is.na(pricegap)&!is.na(Distance)&!is.na(Time),] %>%
  group_by(connection) %>%
  summarise(
    pricegap= mean(pricegap),
    distance= mean(Distance),
    time=mean(Time),
    observations = n()
  )

summary_table_latex <- kable(summary_table, format = "latex", booktabs = TRUE)
print(summary_table_latex)




