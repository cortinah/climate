library(xts)
library(tidyverse)
library(lubridate)
library(magrittr)
library(patchwork)

x <- read.table('./data.ECAD/ECA_blend_tn/TN_STAID000011.txt', header=TRUE, skip=20, sep=",")
x %<>% select(3:5)
colnames(x) <-c('DATE', 'TEMP', 'QUAL')
x %<>% mutate(TEMP=TEMP*0.1)

x[x$TEMP <= -999, 'TEMP'] <- NA
x[x$QUAL==1 | x$QUAL==9, 'TEMP'] <- NA

min_temp <- xts(x$TEMP, order.by = ymd(x$DATE))

###
x <- read.table('./data.ECAD/ECA_blend_tx/TX_STAID000011.txt', header=TRUE, skip=20, sep=",")

x %<>% select(3:5)
colnames(x) <-c('DATE', 'TEMP', 'QUAL')
x %<>% mutate(TEMP=TEMP*0.1)

x[x$TEMP <= -999, 'TEMP'] <- NA
x[x$QUAL==1 | x$QUAL==9, 'TEMP'] <- NA

max_temp <- xts(x$TEMP, order.by = ymd(x$DATE))


krem <- merge(min_temp, max_temp)
krem <- na.omit(krem)
rm(max_temp, min_temp,x)


krem <- fortify.zoo(krem)
krem$month <- month(krem$Index)
krem$mean_temp <- (krem$min_temp + krem$max_temp)/2

krem <- gather(krem, key='type', value='Temp', min_temp, max_temp, mean_temp)

ggplot(krem, aes(x=month, group=month, y=Temp)) +
  geom_boxplot(data=subset(krem, type=='min_temp'), outlier.shape = NA, color='dodgerblue',width=0.3) +theme_minimal() +
  geom_boxplot(data=subset(krem, type=='max_temp'), outlier.shape = NA, color='red', width=0.3)

krem %>% group_by(type, month) %>% summarize(mean=mean(Temp), max=max(Temp), min=min(Temp)) -> krem_range

months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

ggplot(krem_range, aes(x=month, y=mean, color=type)) +
  geom_pointrange(data=subset(krem_range, type=='min_temp'), aes(ymin=min, ymax=max)) +
  geom_pointrange(data=subset(krem_range, type=='max_temp'), aes(ymin=min, ymax=max), position=position_nudge(0.2)) +
  theme_bw(base_size = 14) +scale_x_continuous('Month',breaks=1:12,labels=months) +
  scale_y_continuous('Temperature Cº', breaks=seq(-25,35,5)) +scale_color_discrete(breaks=c('max_temp','min_temp'),labels=c('Daily high','Daily low')) +
  guides(color=guide_legend(title = "Range")) + theme(legend.position = c(0.1,0.9)) +
  geom_line(data=subset(krem_range, type=='min_temp')) +geom_line(data=subset(krem_range, type=='max_temp'), position=position_nudge(0.2)) +
  labs(title='Kremsmünster, Austria: 1876-2018 Temperature Range', caption='Source: https://www.ecad.eu/')
                                
krem %>% filter(Index<as.Date('2000-01-01')) %>% group_by(type, month) %>% summarize(mean=mean(Temp), max=max(Temp), min=min(Temp)) -> krem_range

p1 <- ggplot(krem_range, aes(x=month, y=mean, color=type)) +
  geom_pointrange(data=subset(krem_range, type=='min_temp'), aes(ymin=min, ymax=max)) +
  geom_pointrange(data=subset(krem_range, type=='max_temp'), aes(ymin=min, ymax=max), position=position_nudge(0.2)) +
  theme_bw(base_size = 14) +scale_x_continuous('Month',breaks=1:12,labels=months) +
  scale_y_continuous('Temperature Cº', limits=c(-30,40), breaks=seq(-30, 40, 5)) +scale_color_discrete(breaks=c('max_temp','min_temp'),labels=c('Daily high','Daily low')) +
  guides(color=guide_legend(title = "Range")) + theme(legend.position = c(0.15,0.9)) +
  geom_line(data=subset(krem_range, type=='min_temp')) +geom_line(data=subset(krem_range, type=='max_temp'), position=position_nudge(0.2)) +
  ggtitle('1876-2000')


krem %>% filter(Index>as.Date('2000-01-01')) %>% group_by(type, month) %>% summarize(mean=mean(Temp), max=max(Temp), min=min(Temp)) -> krem_range

p2 <- ggplot(krem_range, aes(x=month, y=mean, color=type)) +
  geom_pointrange(data=subset(krem_range, type=='min_temp'), aes(ymin=min, ymax=max)) +
  geom_pointrange(data=subset(krem_range, type=='max_temp'), aes(ymin=min, ymax=max), position=position_nudge(0.2)) +
  theme_bw(base_size = 14) +scale_x_continuous('Month',breaks=1:12,labels=months) +
  scale_y_continuous('Temperature Cº', limits=c(-30, 40), breaks=seq(-30, 40, 5)) +scale_color_discrete(breaks=c('max_temp','min_temp'),labels=c('Daily high','Daily low')) +
  guides(color=guide_legend(title = "Range")) + theme(legend.position = c(0.15,0.9)) +
  geom_line(data=subset(krem_range, type=='min_temp')) +geom_line(data=subset(krem_range, type=='max_temp'), position=position_nudge(0.2)) +
  ggtitle('2000-2018')

p1+p2  +plot_annotation(title='Kremsmünster, Austria Temperature Range', caption='Source: https://www.ecad.eu/')


## Temperature Anomaly
krem$day <- yday(krem$Index)

krem %>% filter(type=='mean_temp', Index<as.Date('1976-01-01')) %>% group_by(day) %>% summarize(centurymean=mean(Temp)) -> krem_range

krem %>% filter(type=='mean_temp') %>% left_join(krem_range) %>% mutate(anomaly=Temp-centurymean) %>% select(Index, anomaly) -> anomaly

anomaly$year <- year(anomaly$Index)

anomaly %<>% group_by(year) %>% summarize(anom=mean(anomaly))

ggplot(anomaly, aes(x=year, y=anom)) +geom_line(alpha=0.8) +theme_bw(base_size = 14) +
  scale_x_continuous('Year', breaks=seq(1880,2020,10)) +
  scale_y_continuous('Temperature Anomaly Cº', limits=c(-2, 4), breaks=seq(-2, 4, 0.5))  +
  geom_smooth(size=1.5, color='red') +
  labs(title='Kremsmünster, Austria: 1876-2018 Temperature Anomaly', subtitle = 'Baseline period: 1876-1976. Loess trend in red.',
          caption='Source: https://www.ecad.eu/')

rm(months, krem_range, p1, p2, anomaly)

krem %<>% mutate(year=year(Index)) %>% mutate(freezing=ifelse(month>6, 1, 0)) %>%
  mutate(year=year+freezing) %>% select(-month, -freezing)

krem_high <- krem %>% filter(type=='max_temp') %>% select(Index, Temp, month)

krem %<>% filter(type=='mean_temp') %>% select(Index, Temp, month)

## Freezing days
krem %>% group_by(year) %>% filter(Temp<=0) %>% summarize(num_freezing=n()) -> num_freeze_days
num_freeze_days %<>% filter(year<=2018)

ggplot(num_freeze_days, aes(x=year, y=num_freezing)) +geom_line(alpha=0.8) +theme_bw(base_size = 14) +
  scale_x_continuous('Year', breaks=seq(1880,2020,10)) +
  scale_y_continuous('Number of Freezing Days', limits=c(0, 110), breaks=seq(0, 110, 20))  +
  geom_smooth(size=1.5, color='dodgerblue') +
  labs(title='Kremsmünster, Austria: 1876-2018 Number of Freezing Days', subtitle = 'Number of days per year with daily mean temp < 0 Cº. Loess trend in blue.',
       caption='Source: https://www.ecad.eu/')

lfit <- lm(num_freezing ~ year, data=num_freeze_days)
library(segmented)
sfit <- segmented(lfit, seg.Z = ~ year)
summary(sfit)

## Hot days
krem_high %>% group_by(year) %>% filter(Temp >= 30) %>% summarize(num_hot=n()) -> num_hot_days
num_hot_days %<>% filter(year<=2018)

ggplot(num_hot_days, aes(x=year, y=num_hot)) +geom_line(alpha=0.7) +theme_bw(base_size = 14) +
  scale_x_continuous('Year', breaks=seq(1880,2020,10)) +
  scale_y_continuous('Number of Hot Days', limits=c(0, 40), breaks=seq(0, 40, 10) , oob=scales::rescale_none)  +
  geom_smooth(size=1.5, color='red') +
  labs(title='Kremsmünster, Austria: 1876-2018 Number of Hot Days', subtitle = 'Number of days per year with daily high temp >= 30 Cº. Loess trend in red',
       caption='Source: https://www.ecad.eu/')


## Annual excess degrees
krem_high %>% mutate(excess_heat=ifelse(Temp-30>0, Temp-30, 0)) %>% group_by(year) %>% summarize(excess_degrees_total=sum(excess_heat)) -> excess_degrees

ggplot(excess_degrees, aes(x=year, y=excess_degrees_total)) +geom_line(alpha=0.7) +theme_bw(base_size = 14) +
  scale_x_continuous('Year', breaks=seq(1880,2020,10)) +
  scale_y_continuous('Annual Excess Degrees', limits=c(0, 140), breaks=seq(0, 140, 20) , oob=scales::rescale_none)  +
  geom_smooth(size=1.5, color='red') +
  labs(title='Kremsmünster, Austria: 1876-2018 Annual Excess Degree Days', subtitle = 'Annual sum of (daily high - 30 Cº). Loess trend in red',
       caption='Source: https://www.ecad.eu/')
