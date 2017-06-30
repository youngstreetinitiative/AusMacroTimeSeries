## Loading ABS Labour Mobility data for test post

library(tidyverse)
library(scales)

LabMobABS <- read.csv("file:///C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/LabourMobility.csv")

LabMobIndustryABS <- read.csv("file:///C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/Labour Mobility By Industry.csv")

ReasonCeasedEmpABS <- read.csv("file:///C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/Reasons Ceasing Employment.csv")

ABSLabourMobility <- merge(LabMobABS,merge(LabMobIndustryABS,ReasonCeasedEmpABS,all = T),all = T) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% arrange(Date)

LabMobABS <- read.csv("https://raw.githubusercontent.com/youngstreetinitiative/brusselssproutsideas/master/Data/LabourMobility.csv?token=AWGs-0Swr2R_efVrTtNE5FY1epy8Kuyqks5ZXJSBwA%3D%3D")

LabMobIndustryABS <- read.csv("https://raw.githubusercontent.com/youngstreetinitiative/brusselssproutsideas/master/Data/Labour%20Mobility%20By%20Industry.csv?token=AWGs-0kHmTZS7oyZmygIunKuHwU2rjd0ks5ZXJQ-wA%3D%3D")

ReasonCeasedEmpABS <- read.csv("https://raw.githubusercontent.com/youngstreetinitiative/brusselssproutsideas/master/Data/Reasons%20Ceasing%20Employment.csv?token=AWGs-8buS4tZltJD7XxtdbnPkNDP_FzDks5ZXJSowA%3D%3D")

ABSLabourMobility <- merge(LabMobABS,merge(LabMobIndustryABS,ReasonCeasedEmpABS,all = T),all = T) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% arrange(Date)

## Overall number changing employer
ABSLabourMobility <- ABSLabourMobility %>%
  mutate(PropChangedEmp=ChangedEmp/Employed)

ggplot(subset(ABSLabourMobility,!is.na(PropChangedEmp)),aes(x=Date,y=PropChangedEmp))+
  geom_line() + 
  scale_y_continuous(limits=c(0,0.16),breaks=c(seq(0,0.16,0.02)),labels=scales::percent)+
  scale_x_date(breaks = date_breaks("2 years"),labels=date_format("%Y"))+
  ylab("Proportion of Workers Employed at Survey")+xlab("Year")

## Reasons left job
ReasonCeasedEmp <- ReasonCeasedEmpABS %>% select(-CeasedEmp) %>%
  gather(key=ReasonLeft,value=Ceased,-c(Date,EmpSomeTime)) %>%
  mutate(PropReasonLeft=Ceased/EmpSomeTime,
         Date=as.Date(Date,format="%d/%m/%Y"),
         ReasonLeft=as.factor(ReasonLeft))

ggplot(subset(ReasonCeasedEmp,ReasonLeft=="JobLoser"|ReasonLeft=="Jobleaver"),
              aes(x=Date,y=PropReasonLeft,colour=ReasonLeft))+
    geom_line() + 
    scale_y_continuous(limits=c(0,0.14),breaks=c(seq(0,0.14,0.02)),labels=scales::percent)+
    scale_x_date(breaks = date_breaks("2 years"),labels=date_format("%Y"))+
  ylab("Proportion of Workers Employed during year")+xlab("Year")
  


ggplot(subset(ReasonCeasedEmp,ReasonLeft!="JobLoser" & ReasonLeft!="Jobleaver"),
       aes(x=Date,y=PropReasonLeft,colour=ReasonLeft))+
  geom_line() + 
  scale_y_continuous(limits=c(0,0.1),breaks=c(seq(0,0.1,0.02)),labels=scales::percent)+
  scale_x_date(breaks = date_breaks("2 years"),labels=date_format("%Y"))+
  ylab("Proportion of Workers Employed during year")+xlab("Year")
  

## Change of employer by industry

ChangeEmpIndustry <- LabMobIndustryABS %>% 
  mutate(Date=as.Date(Date,format="%d/%m/%Y"),
         Agriculture=Agriculture_ChangeEmp/Agriculture_Count,
         MiningUtilities=MiningUtilities_ChangeEmp/MiningUtilities_Count,
         Manufacturing=Manufacturing_ChangeEmp/Manufacturing_Count,
         Construction=Construction_ChangeEmp/Construction_Count,
         WholesaleRetail=WholesaleRetail_ChangeEmp/WholesaleRetail_Count,
         TransportStorageCommunications=TransportStorageCommunications_ChangeEmp/
           TransportStorageCommunications_Count,
         FinanceBusinessServ=FinanceBusinessServ_ChangeEmp/FinanceBusinessServ_Count,
         CommunityServ=CommunityServ_ChangeEmp/CommunityServ_Count,
         PublicAdmin=PublicAdmin_ChangeEmp/PublicAdmin_Count,
         EntertainmentHotelsPersonalServ=EntertainmentHotelsPersonalServ_ChangeEmp/
           EntertainmentHotelsPersonalServ_Count) %>% 
  select(Date,Agriculture,MiningUtilities,Manufacturing,Construction,WholesaleRetail,
         TransportStorageCommunications,FinanceBusinessServ,CommunityServ,
         PublicAdmin,EntertainmentHotelsPersonalServ) %>%
  gather(key=Industry,value=Proportion,-Date)

ggplot(ChangeEmpIndustry %>% filter(Industry %in% c("Agriculture","MiningUtilities",
                                                    "Manufacturing","TransportStorageCommunications",
                                                    "Construction")),
       aes(x=Date,y=Proportion,colour=Industry))+
  geom_line() + 
  scale_y_continuous(limits=c(0,0.3),breaks=c(seq(0,0.3,0.05)),labels=scales::percent)+
  scale_x_date(breaks = date_breaks("2 year"),labels=date_format("%Y"))+
  ylab("Proportion of Workers Employed at Survey")+xlab("Year")


ggplot(ChangeEmpIndustry %>% filter(Industry %in% c("WholesaleRetail",
                                                    "FinanceBusinessServ","CommunityServ",
                                                    "PublicAdmin","EntertainmentHotelsPersonalServ")),
       aes(x=Date,y=Proportion,colour=Industry))+
  geom_line() + 
  scale_y_continuous(limits=c(0,0.3),breaks=c(seq(0,0.3,0.05)),labels=scales::percent)+
  scale_x_date(breaks = date_breaks("2 year"),labels=date_format("%Y"))+
  ylab("Proportion of Workers Employed at Survey")+xlab("Year")
  