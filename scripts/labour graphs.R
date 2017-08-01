macro_loader()
library(scales)
library(tidyverse)
library(stringr)
library(lubridate)

a<- ggplot(data = LabourForceAggregates )              +
  ylab('index')                                 +
  geom_line(aes(x=Date, y=RBA.cash.rate , col='cashrate'),  size=1, alpha=.5)  +
  geom_line(aes(x=Date, y=(wage.growth)*1, col = "Real Wage"),  size=1, alpha=.5) +
  geom_line(data = HouseIndex, aes(x=Date,y = housegrowth*1 , col='house index'), size=1, alpha=.5) +
  xlim(as.Date(c('1/1/1997', '1/1/2020'),format="%d/%m/%Y"))+
  theme(legend.position=c(.1,.85))


spreadform$WPI.real

class(spreadform$wage.Index)

# spreadform <- spreadform %>%
#   mutate(WPI_growth = (Productivity_hourly - lag(Productivity_hourly, 4))/lag(Productivity_hourly, 4))
# spreadform$Productivity_hourly



b<- ggplot(data = spreadform )              +
  ylab('Index')                                 +
  geom_line(aes(x=Date, y=WPI.real2 , col='WPIreal'),  size=1, alpha=.5)  +
  geom_line(data = Productivity, aes(x=Date, y=Productivity_hourly2 , col='Productivity_hourly'),  size=1, alpha=.5) +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(limits = as.Date(c('1/1/1997', '1/6/2017'),format="%d/%m/%Y"))

b

spreadform <- spreadform %>%
  mutate(WPI_growth = (wage.Index - lag(wage.Index, 4))/lag(wage.Index, 4))

RPIbase <- spreadform$WPI.real[spreadform$Date=="2000-01-01"]
Prodbase <- spreadform$Productivity_hourly[spreadform$Date=="2000-01-01"]
spreadform <- spreadform %>%
  mutate(WPI.real2=(WPI.real/RPIbase)*100)

Productivity <-Productivity %>%
   mutate(Productivity_hourly2=(Productivity_hourly/Prodbase)*100)





b<- ggplot(data = spreadform )              +
  ylab('WPI')                                 +
  geom_line(aes(x=Date, y=Total , col='WPI'),  size=1, alpha=.5)  +
  #geom_line(aes(x=bin_id, y=(wage.Index-CPI), col = "Real Wage"),  size=1, alpha=.5) +
  theme(legend.position=c(.1,.85),panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                  colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_date(breaks = pretty_breaks(20),limits = as.Date(c('1/1/1997', '1/6/2017'),format="%d/%m/%Y"))







b







colnames(LabourForceAggregates)







