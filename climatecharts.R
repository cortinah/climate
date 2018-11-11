library(tidyverse)
library(patchwork)
library(zoo)

file_url <- 'http://cdiac.ess-dive.lbl.gov/ftp/trends/co2/vostok.icecore.co2'
download.file(file_url,'vostok.txt')
vostok <- read_table2("vostok.txt", col_names = FALSE, skip = 21)
colnames(vostok) <- c('depth','age_ice','age_air','co2')
rm(file_url)


file_url <- 'http://cdiac.ess-dive.lbl.gov/ftp/trends/temp/vostok/vostok.1999.temp.dat'
download.file(file_url,'temp.txt')
temp <- read_table2("temp.txt", col_names = FALSE, skip = 60)
colnames(temp) <- c('depth','age_ice','deutirium','temp')
rm(file_url)

a <- ggplot(vostok,aes(x=age_ice,y=co2)) +geom_line(size=1, col='red2') +scale_x_reverse(lim=c(420000,0)) +theme_bw(base_size = 14)+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +labs(y=expression(CO[2]*' concentration' ))

b <- ggplot(temp,aes(x=age_ice,y=temp)) +geom_line(aes(y=rollmean(temp, 8, na.pad=TRUE)), size=1, col='blue') +scale_x_reverse(lim=c(420000,0),
            labels = scales::unit_format(unit='',scale = 1e-3)) +labs(x='Millennia before present', 
            y='Temperature (C)') +theme_bw(base_size = 14)

a / b + plot_annotation(title = "Paleoclimate: Ice Core Records", caption = "Source: Carbon Dioxide Information Analysis Center (CDIAC)",
      subtitle='420,000 years from the Vostok ice core, Antarctica.', theme = theme(  plot.title = element_text(size = 20)))
