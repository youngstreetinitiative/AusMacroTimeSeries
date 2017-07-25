macro_loader()
final$Non.rural.commodity.prices.AU

fit <- lm(Mining ~ Non.rural.commodity.prices.AU + CPI , data=final)
summary(fit) # show results


final$Mining

ggplot(data = quarterly_macro ) +
  ylab('percent') +
  geom_line(aes(x=Date, y=realwagegrowth, col = "Real Wage"),  size=1, alpha=.5) +
  geom_line(aes(x=Date, y=GDP.growth.nominal , col = "GDP GNom"),  size=1, alpha=.5) +
  geom_hline(yintercept = 0) +
  theme(legend.position=c(.1,.85))
