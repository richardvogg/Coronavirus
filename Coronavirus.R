devtools::install_github("RamiKrispin/coronavirus")


library(coronavirus)
library(dplyr)
library(ggplot2)

data("coronavirus")

compare_first_days <- coronavirus %>%
  filter(type=="confirmed") %>%
  #filter(Country.Region%in%c('Chile','Spain','Italy','Germany'),type=="confirmed") %>%
  #filter(Province.State%in%c("Hubei","")) %>%
  group_by(date,country,type) %>%
  summarise(cases=sum(cases)) %>%
  group_by(country) %>%
  mutate(total_cases=cumsum(cases)) %>%
  filter(total_cases>100) %>% #Official start of the coronavirus arrival
  mutate(days_since_start=date-min(date),
         curr_max=max(total_cases)) %>%
  filter(#days_since_start<40, #in case you just want to plot the first days
         curr_max>40)

compare_first_days %>%
  filter(country%in%c('Chile','Brazil','Peru','Ecuador','Argentina','Colombia')) %>%
  #filter(country%in%c('Chile','Germany','Spain','Italy','Brazil','Iran')) %>%
  ggplot(aes(x=days_since_start,y=total_cases,col=country))+
  geom_line(size=0.8)+
  scale_color_manual(values=c("red","green","blue","orange","brown","black"))
  scale_y_log10()
  #scale_size_manual(values=c(rep(0.5,2),2))
