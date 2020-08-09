options(scipen=10)
devtools::install_github("RamiKrispin/coronavirus")


library(coronavirus)
library(dplyr)
library(ggplot2)

data("coronavirus")

compare_first_days <- coronavirus %>%
  filter(type=="confirmed") %>%
  group_by(date,country,type) %>%
  summarise(cases=sum(cases)) %>%
  group_by(country) %>%
  mutate(total_cases=cumsum(cases)) %>%
  filter(total_cases>100) %>% #Official start of the coronavirus arrival
  mutate(days_since_start=date-min(date),
         curr_max=max(total_cases)) %>%
  filter(#days_since_start<40, #in case you just want to plot the first days
         curr_max>400) #to keep the dataset lighter, remove countries with few cases

compare_first_days %>%
  #filter(country%in%c('Chile','Brazil','Peru','Ecuador','Argentina','Colombia')) %>%
  filter(country%in%c('Chile','Germany','Spain','Italy','Brazil','Iran')) %>%
  ggplot(aes(x=days_since_start,y=total_cases,col=country))+
  geom_line(size=0.8)+
  scale_color_manual(values=c("red","green","blue","orange","brown","black"))
  #scale_y_log10()
  #scale_size_manual(values=c(rep(0.5,2),2))




#New cases daily

coronavirus %>% 
  filter(country%in%c("Germany","Chile"),
         type%in%c("confirmed","death")) %>% 
  ggplot(aes(x=date,y=cases))+
  geom_segment(aes(xend=date,yend=0))+
  geom_hline(yintercept=1000,col="red")+
  facet_grid(type~country,scales="free")
