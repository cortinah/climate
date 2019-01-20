library(tsibble)
library(tidyverse)
library(lubridate)
library(magrittr)
library(patchwork)

if (!'fable' %in% installed.packages()) devtools::install_github("tidyverts/fable")
library(fable)

## Read min temp file and wrangle into tsible
raw <- read_csv('./data.ECAD/ECA_blend_tn/TN_STAID000011.txt', skip=20)
raw %<>% select(-STAID, -SOUID) %>% rename(date=DATE, min_temp=TN, qual=Q_TN)

raw %<>% mutate(min_temp = replace(min_temp, min_temp <= -9999, NA)) %>%
         mutate(min_temp = replace(min_temp, qual==1 | qual==9, NA))
raw %<>% mutate(min_temp = min_temp*0.1, date = ymd(date)) %>% select(-qual)

min_temp <- raw %>% as_tsibble(index=date)

## Read max temp file and wrangle into tsible
raw <- read_csv('./data.ECAD/ECA_blend_tx/TX_STAID000011.txt', skip=20)
raw %<>% select(-STAID, -SOUID) %>% rename(date=DATE, max_temp=TX, qual=Q_TX)

raw %<>% mutate(max_temp = replace(max_temp, max_temp <= -9999, NA)) %>%
         mutate(max_temp = replace(max_temp, qual==1 | qual==9, NA))
raw %<>% mutate(max_temp=max_temp*0.1, date = ymd(date)) %>% select(-qual)

max_temp <- raw %>% as_tsibble(index=date)


krem <- left_join(min_temp, max_temp)
rm(max_temp, min_temp, raw)

krem %<>% mutate(month = month(date))
krem %<>% mutate(mean_temp = (max_temp+min_temp)/2 )

krem_narrow <- gather(krem, key='type', value='Temp', min_temp, max_temp, mean_temp) %>% as_tibble()

krem_range <- krem_narrow %>% group_by(type, month) %>% summarize(mean=mean(Temp,na.rm=T), max=max(Temp,na.rm=T), min=min(Temp,na.rm=T))

months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

theme_set(theme_bw(base_size = 14))

ggplot(krem_range, aes(x=month, y=mean, color=type)) +
  geom_pointrange(data=subset(krem_range, type=='min_temp'), aes(ymin=min, ymax=max)) +
  geom_pointrange(data=subset(krem_range, type=='max_temp'), aes(ymin=min, ymax=max), position=position_nudge(0.2)) +
  scale_x_continuous('Month',breaks=1:12,labels=months) +
  scale_y_continuous('Temperature Cº', breaks=seq(-25,35,5)) +scale_color_discrete(breaks=c('max_temp','min_temp'),labels=c('Daily high','Daily low')) +
  guides(color=guide_legend(title = "Range")) + theme(legend.position = c(0.12,0.88)) +
  geom_line(data=subset(krem_range, type=='min_temp')) +geom_line(data=subset(krem_range, type=='max_temp'), position=position_nudge(0.2)) +
  labs(title='Kremsmünster, Austria: 1876-2018 Temperature Range', caption='Source: https://www.ecad.eu/')
                                
krem_range <- krem_narrow %>% filter(date < as.Date('2000-01-01')) %>% group_by(type, month) %>% summarize(mean=mean(Temp,na.rm=T), max=max(Temp,na.rm=T), min=min(Temp,na.rm=T)) 

p1 <- ggplot(krem_range, aes(x=month, y=mean, color=type)) +
  geom_pointrange(data=subset(krem_range, type=='min_temp'), aes(ymin=min, ymax=max)) +
  geom_pointrange(data=subset(krem_range, type=='max_temp'), aes(ymin=min, ymax=max), position=position_nudge(0.2)) +
  theme_bw(base_size = 14) +scale_x_continuous('Month',breaks=1:12,labels=months) +
  scale_y_continuous('Temperature Cº', limits=c(-30,40), breaks=seq(-30, 40, 5)) +scale_color_discrete(breaks=c('max_temp','min_temp'),labels=c('Daily high','Daily low')) +
  guides(color=guide_legend(title = "Range")) + theme(legend.position = c(0.15,0.87)) +
  geom_line(data=subset(krem_range, type=='min_temp')) +geom_line(data=subset(krem_range, type=='max_temp'), position=position_nudge(0.2)) +
  ggtitle('1876-2000')


krem_range <- krem_narrow %>% filter(date >= as.Date('2000-01-01')) %>% group_by(type, month) %>% summarize(mean=mean(Temp,na.rm = T), max=max(Temp,na.rm = T), min=min(Temp,na.rm = T))

p2 <- ggplot(krem_range, aes(x=month, y=mean, color=type)) +
  geom_pointrange(data=subset(krem_range, type=='min_temp'), aes(ymin=min, ymax=max)) +
  geom_pointrange(data=subset(krem_range, type=='max_temp'), aes(ymin=min, ymax=max), position=position_nudge(0.2)) +
  theme_bw(base_size = 14) +scale_x_continuous('Month',breaks=1:12,labels=months) +
  scale_y_continuous('Temperature Cº', limits=c(-30, 40), breaks=seq(-30, 40, 5)) +scale_color_discrete(breaks=c('max_temp','min_temp'),labels=c('Daily high','Daily low')) +
  guides(color=guide_legend(title = "Range")) + theme(legend.position = c(0.15,0.9)) +
  geom_line(data=subset(krem_range, type=='min_temp')) +geom_line(data=subset(krem_range, type=='max_temp'), position=position_nudge(0.2)) +
  ggtitle('2000-2018')

p1 + p2 + plot_annotation(title='Kremsmünster, Austria Daily Temperature Range', caption='Source: https://www.ecad.eu/')


## Temperature anomaly based on 100-year average
krem_narrow %<>% mutate(day=yday(date))
krem_range <- krem_narrow %>% filter(type=='mean_temp', date < as.Date('1976-01-01')) %>% group_by(day) %>% summarize(centurymean=mean(Temp, na.rm=T))

anomaly <- krem_narrow %>% filter(type=='mean_temp') %>% left_join(krem_range) %>% mutate(anomaly=Temp-centurymean) %>% select(date, anomaly)

anomaly %<>% mutate(year = year(date)) %>% as_tsibble(index=date) %>%
             index_by(year) %>% summarize(anom=mean(anomaly, na.rm=T))

ggplot(anomaly, aes(x=year, y=anom)) +geom_line(alpha=0.8)  +
  scale_x_continuous('Year', breaks=seq(1880, 2020, 20)) +
  scale_y_continuous('Temperature Anomaly Cº', limits=c(-2, 4), breaks=seq(-2, 4, 0.5))  +
  geom_smooth(size=1.5, color='red') +
  labs(title='Kremsmünster, Austria: 1876-2018 Temperature Anomaly', subtitle = 'Baseline period: 1876-1976. Loess trend in red.',
          caption='Source: https://www.ecad.eu/')

rm(months, krem_range, p1, p2, anomaly)

krem_narrow %<>% mutate(year=year(date), winter=ifelse(month>6, 1, 0)) %>%
  mutate(year=year+winter) %>% select(-winter)

krem_high <- krem_narrow %>% filter(type=='max_temp') %>% select(date, Temp, year)

krem_narrow %<>% filter(type=='mean_temp') %>% select(date, Temp, year)

## Freezing days
num_freeze_days <- krem_narrow %>% group_by(year) %>% filter(Temp<=0) %>% summarize(num_freezing=n()) 
num_freeze_days %<>% filter(year<=2018)

ggplot(num_freeze_days, aes(x=year, y=num_freezing)) +geom_line(alpha=0.8)  +
  scale_x_continuous('Year', breaks=seq(1880, 2020, 20)) +
  scale_y_continuous('Number of Freezing Days', limits=c(0, 110), breaks=seq(0, 110, 20))  +
  geom_smooth(size=1.5, color='dodgerblue') +
  labs(title='Kremsmünster, Austria: 1876-2017 Number of Freezing Days', subtitle = 'Number of days per year with daily mean temp < 0 Cº. Loess trend in blue.',
       caption='Source: https://www.ecad.eu/')

## Hot days
num_hot_days <- krem_high %>% group_by(year) %>% filter(Temp >= 30) %>% summarize(num_hot=n()) 

ggplot(num_hot_days, aes(x=year, y=num_hot)) +geom_line(alpha=0.7)  +
  scale_x_continuous('Year', breaks=seq(1880,2020,10)) +
  scale_y_continuous('Number of Hot Days', limits=c(0, 40), breaks=seq(0, 40, 10) , oob=scales::rescale_none)  +
  geom_smooth(size=1.5, color='red') +
  labs(title='Kremsmünster, Austria: 1876-2018 Number of Hot Days', subtitle = 'Number of days per year with daily high temp >= 30 Cº. Loess trend in red',
       caption='Source: https://www.ecad.eu/')


## Annual excess degrees
excess_degrees <- krem_high %>% mutate(excess_heat=ifelse(Temp-30 > 0, Temp-30, 0)) %>% group_by(year) %>% summarize(excess_degrees_total=sum(excess_heat))

ggplot(excess_degrees, aes(x=year, y=excess_degrees_total)) +geom_line(alpha=0.7)  +
  scale_x_continuous('Year', breaks=seq(1880, 2020, 20)) +
  scale_y_continuous('Annual Excess Degrees', limits=c(0, 140), breaks=seq(0, 140, 20), oob=scales::rescale_none) +
  geom_smooth(size=1.5, color='red') +
  labs(title='Kremsmünster, Austria: 1876-2018 Annual Excess Degree Days', subtitle = 'Annual sum of (daily high - 30 Cº). Loess trend in red',
       caption='Source: https://www.ecad.eu/')
