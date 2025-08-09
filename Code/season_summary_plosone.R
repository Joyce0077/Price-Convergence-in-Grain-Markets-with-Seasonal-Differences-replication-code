#This code replicates Table 1
library(data.table)
library(ggplot2)
library(cowplot)
library(xtable)
library(readxl)
library(igraph)
library(dplyr)
library(knitr)
library(reshape2)

setwd("")
price4<-fread("price_dt.csv")


price4[itemname0=="Rice (local – no stone)", grp:="Local Rice"][itemname0=="Rice (long grain)", grp:="long-grain Rice"][itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)", grp:="cowpea"]
length(unique(price4[grp=="cowpea"]$market))
length(unique(price4[grp=="Local Rice"]$market))
length(unique(price4[grp=="long-grain Rice"]$market))
summary_table0 <- price4[!is.na(price)&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)"),] %>%
  group_by(grp) %>%
  summarise(
    average = mean(price),
  )
summary_table0 <- price4[!is.na(price)&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)"),] %>%
  group_by(grp) %>%
  summarise(
    average = mean(price),
  )
summary_table0$Variable="Average Price"

summary_table1 <- price4[!is.na(price)&Urban==1&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)"),] %>%
  group_by(grp) %>%
  summarise(
    average = mean(price),
  )
summary_table1$Variable="Average Price Urban"

summary_table2<- price4[!is.na(price)&Urban==0&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)"),] %>%
  group_by(grp) %>%
  summarise(
    average = mean(price),
  )
summary_table2$Variable="Average Price Rural"


summary_table3 <- price4[loc=="North"&!is.na(price)&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)"),] %>%
  group_by(grp) %>%
  summarise(
    average = mean(price)
  )
summary_table3$Variable="Average Price North"

summary_table4 <- price4[loc=="South"&!is.na(price)&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)"),] %>%
  group_by(grp) %>%
  summarise(
    average = mean(price)
  )
summary_table4$Variable="Average Price South"

price4<-merge(price4, grow[,.(Cid,StartHarvest,EndHarvest,second.harvest.s=second.harvest,second.harvest.e=second.harvest,Location)], by.x = c("itemid0","loc"), by.y = c("Cid","Location"))
price4[,harvest:=0][(month>=month(as.Date(StartHarvest.y,format="%m/%d/%y"))&month<=month(as.Date(EndHarvest.y,format="%m/%d/%y")))|(month>=month(as.Date(second.harvest.s,format="%m/%d/%y"))&month<=month(as.Date(second.harvest.e,format="%m/%d/%y"))),harvest:=1]

summary_table4_1 <- price4[harvest==1&!is.na(price)&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)"),] %>%
  group_by(grp) %>%
  summarise(
    average = mean(price)
  )
summary_table4_1$Variable="Average Price Harvest"

summary_table4_2 <- price4[harvest==0&!is.na(price)&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)"),] %>%
  group_by(grp) %>%
  summarise(
    average = mean(price)
  )
summary_table4_2$Variable="Average Price Non Harvest"


library(tidyverse)
marketpair1<-fread("price_season_analysis1.csv")

marketpair1[,pgap:=abs(price-price2)]
marketpair1[itemname0=="Rice (local – no stone)", grp:="Local Rice"][itemname0=="Rice (long grain)", grp:="long-grain Rice"][itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)", grp:="cowpea"]


marketpair2<-marketpair1[(direct_connection==1|indirect_connection==1)&(itemname0=="Rice (local – no stone)"|itemname0=="Rice (long grain)"|itemname0=="Brown beans (cowpeas)"|itemname0=="White beans (cowpeas)")]

summary_table6 <- marketpair2[hh==1] %>%
  group_by(grp) %>%
  summarise(
    average = mean(pgap)
  )

summary_table6$Variable="Average Inter-Market-Gap (Harvest - Harvest)"

summary_table7 <- marketpair2[hn==1] %>%
  group_by(grp) %>%
  summarise(
    average  = mean(pgap)
  )

summary_table7$Variable="Average Inter-Market-Gap (Harvest - NonHarvest)"

summary_table8 <- marketpair2[nn==1] %>%
  group_by(grp) %>%
  summarise(
    average  = mean(pgap)
  )
summary_table8$Variable="Average Inter-Market-Gap (NonHarvest - NonHarvest)"




summary_table<-rbind(summary_table0,summary_table1,summary_table2,summary_table3,summary_table4_1,summary_table4_2,summary_table6,summary_table7,summary_table8)
#summary_table <- summary_table[complete.cases(summary_table), !duplicated(colnames(summary_table))]
summary_tablew<-pivot_wider(summary_table,names_from = "grp", values_from = "average")
summary_tablew<-data.table(summary_tablew)[,.(Variable,`cowpea`,`Local Rice`,`long-grain Rice`)]
xtable::xtable(summary_tablew)

