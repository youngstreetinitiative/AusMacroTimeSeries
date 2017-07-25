final$HousePriceIndex8Capitals

library(tidyverse)
library(stringr)
macro_loader()
a<-ggplot(data = select(all_done,Cats, CPI )) +
  ylab('percent') +
  geom_line(aes(x=bin_id, y=Total, col = Cats),  size=1, alpha=.5) +
  theme(legend.position = "none")


growth_rate<-function(x){
  x/lag(x,-1) - 1
}

growth_rate<-function(x,y){
  diff(x$y)/x$y[-nrow(x$y),] * 100
}

df <- df %>%
  mutate(perc_growth = (x - lag(x, 4))/lag(x, 4))

HouseIndex$HousePriceIndex8Capitals


gdp_growth <- c(NA, diff(testdata$gdp)/ testdata$gdp[-4])
rpi_index_growth <- c(NA, diff(testdata$rpi_index)/ testdata$rpi_index[-1])
cpi_index_growth <- c(NA, diff(testdata$cpi_index)/ testdata$cpi_index[-1])

growth_rate<-function(x,y){c(NA,(x$y-x$y[-4]/x$y[-4]))
}

HouseIndex <- HouseIndex %>%
  mutate(growthrate = (HousePriceIndex8Capitals - lag(HousePriceIndex8Capitals, 4))/lag(HousePriceIndex8Capitals, 4))


growth_rate(HouseIndex,HousePriceIndex8Capitals)

HouseIndex$housegrowth4 <- c(NA, diff(HouseIndex$HousePriceIndex8Capitals)/ HouseIndex$HousePriceIndex8Capitals[-4])


growth_rate(HouseIndex,HousePriceIndex8Capitals)

a<- ggplot(data = quarterlymacrodata )              +
  ylab('index')                                 +
  geom_line(aes(x=Date, y=RBA.cash.rate , col='cashrate'),  size=1, alpha=.5)  +
  geom_line(aes(x=Date, y=(wage.growth)*1, col = "Real Wage"),  size=1, alpha=.5) +
  geom_line(data = HouseIndex, aes(x=Date,y = housegrowth*1 , col='house index'), size=1, alpha=.5) +
  xlim(as.Date(c('1/1/1990', '1/1/2020'),format="%d/%m/%Y"))+
  theme(legend.position=c(.1,.85))


a<- ggplot(data = quarterlymacrodata )              +
  ylab('index')                                 +
  geom_line(aes(x=Date, y=RBA.cash.rate , col='cashrate'),  size=1, alpha=.5)  +
  geom_line(aes(x=Date, y=(wage.growth)*100, col = "Real Wage"),  size=1, alpha=.5) +
  geom_line(data = HouseIndex, aes(x=Date,y = growthrate*100 , col='house index'), size=1, alpha=.5) +
  geom_line(data = PopulationStats, aes(x=Date,y=NetOverseasMigration/8 , col='net migrants'), size=1, alpha=.5) +
  xlim(as.Date(c('1/1/1980', '1/1/2020'),format="%d/%m/%Y"))+
  theme(legend.position=c(.1,.85))


library(plotly)

ggplotly(a)

cpt.mean(data, penalty="MBIC", pen.value=0, method="AMOC", Q=5, test.stat="Normal", class=TRUE, param.estimates=TRUE,minseglen=1)

macro_loader()

?filter
