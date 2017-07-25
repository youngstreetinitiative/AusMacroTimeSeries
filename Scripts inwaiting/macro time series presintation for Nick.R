
macro_loader()
library(scales)
library(tidyverse)
library(stringr)
library(lubridate)
HouseIndex$HousePriceIndex8Capitals

HouseIndex$Date <- as.Date(HouseIndex$Date)

a<- ggplot(data = HouseIndex )              +
  ylab('index')                                 +
  geom_line(aes(x=Date, y=HousePriceIndexSydney , col='Sydney'),  size=1, alpha=.5)  +
  geom_line(aes(x=Date, y=(HousePriceIndexAdelaide), col = "Adelaide"),  size=1, alpha=.5) +
  xlim(as.Date(c('1/1/1990', '1/1/2020'),format="%d/%m/%Y"))+
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                 colour = "grey")) +
  scale_x_date(breaks = pretty_breaks(25))
a

library(plotly)

ggplotly(a)

final$PopulationAusChange


fit <- lm(log(HousePriceIndex8Capitals) ~ log(wage.Index) + log(PopulationAusChange), data=final)
summary(fit)















