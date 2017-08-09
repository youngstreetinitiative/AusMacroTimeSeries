macro_loader()
library(scales)
library(tidyverse)
library(stringr)
library(lubridate)
library(zoo)

# spreadform <- spreadform %>%
#   mutate(WPI_growth = (Productivity_hourly - lag(Productivity_hourly, 4))/lag(Productivity_hourly, 4))
# spreadform$Productivity_hourly

#  labour force simple geom lines

RPIbase <- spreadform$WPI.real[spreadform$Date=="2000-01-01"]
Prodbase <- spreadform$Productivity_hourly[spreadform$Date=="2000-01-01"]
spreadform <- spreadform %>%
  mutate(WPI.real2=(WPI.real/RPIbase)*100)

Productivity <-Productivity %>%
  mutate(Productivity_hourly2=(Productivity_hourly/Prodbase)*100)


WPIandProd_real<- ggplot(data = spreadform )              +
  ylab('Index')                                 +
  geom_line(aes(x=Date, y=WPI.real2 , col='WPIreal'),  size=1, alpha=.5)  +
  geom_line(data = Productivity, aes(x=Date, y=Productivity_hourly2 , col='Productivity_hourly'),  size=1, alpha=.5) +
  theme(legend.position=c(.15,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(breaks = pretty_breaks(15),limits = as.Date(c('1/1/1997', '1/6/2017'),format="%d/%m/%Y"))

WPIandProd_real
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/WPIandProd_real.png", WPIandProd_real)

WPI_growth<- ggplot(data = spreadform )              +
  ylab('WPI_growth')                                 +
  geom_line(aes(x=Date, y=wage.growth ),  size=1, alpha=.5, col="darkblue")  +
  theme(legend.position=c(.15,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                   colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_y_continuous(labels=scales::percent,limits=c(0,0.045))+
  scale_x_date(breaks = pretty_breaks(15),limits = as.Date(c('1/1/1997', '1/6/2017'),format="%d/%m/%Y"))

WPI_growth
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/WPI_growth.png", WPI_growth)

AverageWeekly<- ggplot(data = `Average Weekly Earnings` )              +
  ylab('average weekly earnings (AUD)')                                 +
  geom_line(aes(x=Date, y=Total),  size=1, alpha=.5, col ="orange")  +
  #geom_line(aes(x=bin_id, y=(wage.Index-CPI), col = "Real Wage"),  size=1, alpha=.5) +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1994', '1/6/2017'),format="%d/%m/%Y"))
AverageWeekly
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/AverageWeekly.png", AverageWeekly)


underemp<- ggplot(data = LabourForceAggregates )              +
  ylab('Underemployed (%)')                                 +
  geom_line(aes(x=Date, y=UnderEmpRate_Trend ),  size=1, alpha=.5,col= "purple")  +
  #geom_line(aes(x=bin_id, y=(wage.Index-CPI), col = "Real Wage"),  size=1, alpha=.5) +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1990', '1/6/2017'),format="%d/%m/%Y"))
underemp
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/underemp.png", underemp)



prod<- ggplot(data = Productivity )              +
  ylab('Productivity (Index)')                                 +
  geom_line(aes(x=Date, y=Productivity_hourly ),  size=1, alpha=.5, col='darkgreen')  +
  #geom_line(aes(x=bin_id, y=(wage.Index-CPI), col = "Real Wage"),  size=1, alpha=.5) +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1990', '1/6/2017'),format="%d/%m/%Y"))
prod
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/prod.png", prod)


underutil<- ggplot(data = LabourForceAggregates )              +
  ylab('Underutilisation (Percent)')                                 +
  geom_line(aes(x=Date, y=UnderutilisationRate_Trend ),  size=1, alpha=.5, col="red")  +
  #geom_line(aes(x=bin_id, y=(wage.Index-CPI), col = "Real Wage"),  size=1, alpha=.5) +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1990', '1/6/2017'),format="%d/%m/%Y"))
underutil
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/underutil.png", underutil)


EmploymentByIndustry$PT_ratio<-EmploymentByIndustry$Total_EmpPT/EmploymentByIndustry$Total_EmpTotal

part_time<- ggplot(data = EmploymentByIndustry )              +
  ylab('ratio employed part time')                                 +
  geom_line(aes(x=Date, y=rollmean(PT_ratio,4,na.pad = T)),  size=1, alpha=.5,col ="darkred")  +
  #geom_line(aes(x=Date, y=Total_EmpFT , col='Total_EmpFT'),  size=1, alpha=.5)  +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1990', '1/6/2017'),format="%d/%m/%Y"))+
  scale_y_continuous(labels=scales::percent,limits=c(0.1,0.4))
part_time
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/part_time.png", part_time)


################################################### more complex plots
industry <- filter(longform, Cats %in% c("Agriculture_EmpTotal","Mining_EmpTotal","Manufacturing_EmpTotal","Utilities_EmpTotal",
                                          "Construction_EmpTotal","Wholesale_EmpTotal","Retail_EmpTotal","Hospitality_EmpTotal",
                                          "Transport_EmpTotal","Media_EmpTotal","Financial_EmpTotal","RentalRealEstate_EmpTotal",
                                          "ProfScientificTech_EmpTotal","Administration_EmpTotal","PublicAdmin_EmpTotal","Education_EmpTotal","HealthSocial_EmpTotal",
                                          "ArtsRec_EmpTotal","OtherServices_EmpTotal"))

industry2 <- filter(longform, Cats %in% c("Agriculture_EmpTotal","Mining_EmpTotal","Manufacturing_EmpTotal","Utilities_EmpTotal",
                               "Construction_EmpTotal","Wholesale_EmpTotal","Retail_EmpTotal","Hospitality_EmpTotal",
                               "Transport_EmpTotal","Media_EmpTotal","Financial_EmpTotal","RentalRealEstate_EmpTotal",
                               "ProfScientificTech_EmpTotal","Administration_EmpTotal","PublicAdmin_EmpTotal","Education_EmpTotal","HealthSocial_EmpTotal",
                               "ArtsRec_EmpTotal","OtherServices_EmpTotal",
                               "Agriculture_EmpPT","Mining_EmpPT","Manufacturing_EmpPT","Utilities_EmpPT",
                               "Construction_EmpPT","Wholesale_EmpPT","Retail_EmpPT","Hospitality_EmpPT",
                               "Transport_EmpPT","Media_EmpPT","Financial_EmpPT","RentalRealEstate_EmpPT",
                               "ProfScientificTech_EmpPT","Administration_EmpPT","PublicAdmin_EmpPT","Education_EmpPT","HealthSocial_EmpPT",
                               "ArtsRec_EmpPT","OtherServices_EmpPT"))


industry_S <- spread(industry2,Cats,Total)

industry_S_C <- spread(industry2,Cats,Total)


industry_S_C$PrimaryIndusty_PT <- industry_S_C$Agriculture_EmpPT + industry_S_C$Mining_EmpPT + industry_S_C$Utilities_EmpPT
industry_S_C$SecondaryIndusty_PT <- industry_S_C$Manufacturing_EmpPT + industry_S_C$Construction_EmpTotal + industry_S_C$Wholesale_EmpPT + industry_S_C$Transport_EmpPT
industry_S_C$TertiaryIndusty_PT <- industry_S_C$Retail_EmpPT + industry_S_C$Hospitality_EmpPT + industry_S_C$Media_EmpPT + industry_S_C$ArtsRec_EmpPT
industry_S_C$BusnessServices_PT <- industry_S_C$Financial_EmpPT + industry_S_C$RentalRealEstate_EmpPT + industry_S_C$ProfScientificTech_EmpPT +
                                                            industry_S_C$Administration_EmpPT + industry_S_C$OtherServices_EmpPT
industry_S_C$Govdominated_PT <-   industry_S_C$PublicAdmin_EmpPT + industry_S_C$Education_EmpPT + industry_S_C$HealthSocial_EmpPT

industry_S_C$PrimaryIndusty_Total <- industry_S_C$Agriculture_EmpTotal + industry_S_C$Mining_EmpTotal + industry_S_C$Utilities_EmpTotal
industry_S_C$SecondaryIndusty_Total <- industry_S_C$Manufacturing_EmpTotal + industry_S_C$Construction_EmpTotal + industry_S_C$Wholesale_EmpTotal + industry_S_C$Transport_EmpTotal
industry_S_C$TertiaryIndusty_Total <- industry_S_C$Retail_EmpTotal + industry_S_C$Hospitality_EmpTotal + industry_S_C$Media_EmpTotal + industry_S_C$ArtsRec_EmpTotal
industry_S_C$BusnessServices_Total <- industry_S_C$Financial_EmpTotal + industry_S_C$RentalRealEstate_EmpTotal + industry_S_C$ProfScientificTech_EmpTotal +
                                                            industry_S_C$Administration_EmpTotal + industry_S_C$OtherServices_EmpTotal
industry_S_C$Govdominated_Total <-   industry_S_C$PublicAdmin_EmpTotal + industry_S_C$Education_EmpTotal + industry_S_C$HealthSocial_EmpTotal

industry_S_C[2:39] <- NULL

industry_S_C$PrimaryIndusty_part_time <- industry_S_C$PrimaryIndusty_PT/industry_S_C$PrimaryIndusty_Total
industry_S_C$SecondaryIndusty_part_time <- industry_S_C$SecondaryIndusty_PT/industry_S_C$SecondaryIndusty_Total
industry_S_C$TertiaryIndustry_part_time <- industry_S_C$TertiaryIndusty_PT/industry_S_C$TertiaryIndusty_Total
industry_S_C$BusnessServices_part_time <- industry_S_C$BusnessServices_PT/industry_S_C$BusnessServices_Total
industry_S_C$GovDominated_part_time <- industry_S_C$Govdominated_PT/industry_S_C$Govdominated_Total

industry_S_C[2:11] <- NULL




industry_S_C <- gather(industry_S_C,Industry, Proportion, -bin_id )
industry_S_C$bin_id <- as.Date(industry_S_C$bin_id)
colnames(industry_S_C)[1] <- "Date"
part_time_ratio<-ggplot(industry_S_C, aes(x = Date, y = Proportion, group = Industry)) +
  geom_line(colour='blue')+
  facet_wrap(~Industry)+
  scale_y_continuous(labels=scales::percent,limits=c(0,1))+
  scale_x_date(breaks = pretty_breaks(10),limits = as.Date(c('1/1/1990', '1/1/2017'),format="%d/%m/%Y"))
part_time_ratio
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/part_time_ratio.png", part_time_ratio)


ggplotly(part_time_ratio)



x_S <- spread(x,Category,Value)
x2 <- x %>%
  group_by(Category) %>%
  mutate(Roll_mean = rollmean(Value,4,fill=NA))

c<- ggplot(data = x2 )           +
  ylab('PT_ratio')                 +
  geom_line(aes(x=bin_id, y=Roll_mean, col=Category, alpha=.5, linetype = Category))
c

colnames(industry) <- c("Date","sector","Employed")
jobs<-ggplot(industry, aes(x = Date, y = Employed, fill = sector)) +
  geom_area(col="black", size=0.2, alpha=.4)+
  ylab("number employed (000's)")
jobs
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/jobs.png", jobs)

test2 <- Consumption %>%
 gather(Consumption_Catergory,ConsumptionDollars,-Date) %>%
 filter(Consumption_Catergory != "consumtpion.TOTAL") %>%
  group_by(ConsumptionDollars) %>%
  mutate(Total = sum(ConsumptionDollars),
         ConsumptionPerc = ConsumptionDollars/Total)

consumption<-ggplot(test2, aes(x = Date, y = ConsumptionDollars, fill = Consumption_Catergory)) + geom_area(position = "fill",colour="black", size=0.2, alpha=.4)
consumption
ggsave("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Houses and wages/Jackson graphs/consumption.png", consumption)





######################################################

#ARRRR and DEEEEE
######################################################





randD<- ggplot(data = Research and Development )              +
  ylab('Underemployed (%)')                                 +
  geom_line(aes(x=Date, y=UnderEmpRate_Trend ),  size=1, alpha=.5,col= "purple")  +
  #geom_line(aes(x=bin_id, y=(wage.Index-CPI), col = "Real Wage"),  size=1, alpha=.5) +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1990', '1/6/2017'),format="%d/%m/%Y"))
underemp








