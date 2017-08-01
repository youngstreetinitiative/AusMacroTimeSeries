
macro_loader()
library(scales)
library(tidyverse)
library(stringr)
library(lubridate)
HouseIndex$HousePriceIndex8Capitals

HouseIndex$Date <- as.Date(HouseIndex$Date)
final$consumtpion.vehicles
final$bin_id <- as.Date(final$bin_id)


a<- ggplot(data = final )              +
  ylab('index')                                 +
  geom_line(aes(x=bin_id, y=HousePriceIndexSydney , col='Sydney'),  size=1, alpha=.5)  +
#  geom_line(aes(x=bin_id, y=(HousePriceIndexAdelaide), col = "Adelaide"),  size=1, alpha=.5) +
  geom_line(aes(x=bin_id, y=(consumtpion.TOTAL)/((PopulationAus)/35), col = "vehicle expenditure(index)"),  size=1, alpha=.5) +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                 colour = "grey")) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1990', '1/1/2020'),format="%d/%m/%Y"))
a







library(plotly)

ggplotly(a)

final$PopulationAusChange


fit <- lm(log(HousePriceIndex8Capitals) ~ log(wage.Index) + log(PopulationAusChange), data=final)
summary(fit)


quarterlymacrodata$wage.Index

b<- ggplot(data = quarterlymacrodata )              +
  ylab('index')                                 +
  geom_line(aes(x=Date, y=RBA.cash.rate , col='cashrate'),  size=1, alpha=.5)  +
  geom_line(aes(x=Date, y=(wage.Index-CPI), col = "Real Wage"),  size=1, alpha=.5) +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey")) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1999', '1/1/2020'),format="%d/%m/%Y"))
b

?scale_x_date

a










