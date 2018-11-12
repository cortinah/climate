library(tidyverse)
library(patchwork)
library(zoo)

file_url <- 'http://cdiac.ess-dive.lbl.gov/ftp/trends/co2/vostok.icecore.co2'
download.file(file_url,'vostok.txt')
vostok <- read_table2("vostok.txt", col_names = FALSE, skip = 21)
colnames(vostok) <- c('depth','age_ice','age_air','co2')

file_url <- 'http://cdiac.ess-dive.lbl.gov/ftp/trends/temp/vostok/vostok.1999.temp.dat'
download.file(file_url,'paleotemp.txt')
paleotemp <- read_table2("paleotemp.txt", col_names = FALSE, skip = 60)
colnames(paleotemp) <- c('depth','age_ice','deutirium','temp')


a <- ggplot(vostok,aes(x=age_ice,y=co2)) +geom_line(size=1, col='red2') +scale_x_reverse(lim=c(420000,0)) +theme_bw(base_size = 14)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +labs(y=expression(CO[2]*' concentration' ))

b <- ggplot(paleotemp,aes(x=age_ice,y=temp)) +geom_line(aes(y=rollmean(temp, 8, na.pad=TRUE)), size=1, col='blue') +scale_x_reverse(lim=c(420000,0),
            labels = scales::unit_format(unit='',scale = 1e-3)) +labs(x='Millennia before present', 
            y='Temperature (C)') +theme_bw(base_size = 14)

a / b + plot_annotation(title = "Paleoclimate: Ice Core Records", caption = "Source: Carbon Dioxide Information Analysis Center (CDIAC)\nhttp://cdiac.ess-dive.lbl.gov/trends/co2/vostok.html",
      subtitle='420,000 years from the Vostok ice core, Antarctica.', theme = theme(  plot.title = element_text(size = 20)))


file_url <- 'ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt'
download.file(file_url,'maunaloa.txt')
maunaloa <- read_table2("maunaloa.txt", col_names = FALSE, skip = 72)
colnames(maunaloa) <- c('year','month', 'date','average','interpolated','trend','days')
maunaloa$date <- as.Date(as.yearmon(paste(maunaloa$year,maunaloa$month,sep='-')))

ggplot(maunaloa, aes(x=date, y=average)) +geom_line(alpha=0.5) +
  scale_x_date(name=NULL, date_breaks = '10 years',date_labels = '%Y') +
  scale_y_continuous(lim=c(300,410),breaks=seq(300,400,20)) +theme_bw(base_size = 14) +
  geom_line(aes(y=trend), size=1, col='red2') +labs(title='The Keeling Curve', subtitle=expression('Mauna Loa '*CO[2]*' monthly mean ppm'),
  y=expression(CO[2]*' concentration in air' ),caption='Source: NOAA/ESRL and Scripps Institution of Oceanography.\nhttps://www.esrl.noaa.gov/gmd/ccgg/trends/data.html')


file_url <- 'https://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.csv'
download.file(file_url,'gisstemp.csv')
gisstemp <- read_csv("gisstemp.csv", skip = 1)
gisstemp <- gisstemp[,c('Year','J-D')]
colnames(gisstemp) <- c('date','annmean')
gisstemp$date <- as.Date(as.yearmon(gisstemp$date))
gisstemp$annmean <- as.numeric(gisstemp$annmean)

ggplot(gisstemp, aes(x=date, y=annmean)) +geom_line(alpha=0.6, aes(color='Annual mean')) +
  scale_x_date(name=NULL, lim=c(as.Date('1878-01-01'),as.Date('2020-01-01')), date_breaks='10 years', date_labels='%Y') +
  scale_y_continuous() +theme_bw(base_size = 14) + geom_smooth(size=1.1, se=F, span=0.2,aes(color='Loess smoothing')) +
  labs(title='Global Land-Ocean Temperature Index (LOTI)', subtitle='Global surface temperature relative to 1951-1980 average',
  y='Temperature Anomaly (C)', caption='Source: NASA Goddard Institute for Space Studies\nhttps://data.giss.nasa.gov/gistemp/') +
  scale_color_manual(name=NULL, values=c('black','blue')) +theme(legend.position = c(0.13,0.89),legend.background=element_blank())
  

