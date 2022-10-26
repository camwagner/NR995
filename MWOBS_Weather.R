library(readxl)
library(dplr)
library(lubridate)
library("ggplot2")
require(ggplot2)
require(scales)
require(gridExtra)
require(lubridate)
library(reshape2)
library(tidyr)
library(openair)
library(measurements)
library(tidyverse)

x<- read_xlsx("~/Desktop/Thesis/MWO Datafiles/Cameron_Wagner hourly temp-wind-precip.xlsx", sheet = "Import to R")
View(x)
date<- (x$`date`)
date <- as.Date(date)
temp<- (x$`temp`)
temp <- as.matrix(temp)
precip<- (x$`precip`)
precip <- as.matrix(precip)
cum_precip<- (x$`cum_precip`)
cum_precip <- as.matrix(cum_precip)
dir<- (x$`deg`)
dir <- as.matrix(dir)
speed<- (x$`wind_speed`)
speed <- as.matrix(speed)
snow<- (x$`snow`)
snow <- as.matrix(snow)
cum_snow<- (x$`cum_snow`)
cum_snow <- as.matrix(cum_snow)



mydata <- data.frame(date, temp, precip, dir, speed)

##Wind Speed + Direction##

windRose(mydata, ws = "speed", wd = "dir", ws2 = NA, wd2 = NA,
         ws.int = 20, angle = 10, type = "default", bias.corr = TRUE, cols
         = c("#00b009", "#f2eb27", "#ffae3d", "#bd1e19"), grid.line = NULL, width = 1, seg = NULL, auto.text
         = TRUE, breaks = 4, offset = 15, normalise = FALSE, max.freq =
           NULL, paddle = FALSE, key.header = NULL, key.footer = "(mph)",
         key.position = "bottom", key = TRUE, dig.lab = 2, statistic =
           "abs.count", pollutant = NULL, annotate = TRUE, angle.scale =
           45, border = TRUE, main = "Mount Washington Observatory 2021-2022 Winter Wind Speed and Direction")

ggplot(mydata, aes(date, speed, group = 1)) + 
  geom_line(color='#02ab07', size = .5) +
  geom_vline(xintercept = as.numeric(as.Date("2021-10-29")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-10-29"), label="\n10/29/21 (Baseline)", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-11-20"), label="\n11/20/21", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-15")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-12-15"), label="\n12/15/21", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-08")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-01-08"), label="\n1/8/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-21")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-01-21"), label="\n1/21/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-01"), label="\n2/1/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-24"), label="2/24/22\n", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-26")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-26"), label="\n2/26/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-09")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-09"), label="\n3/9/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-23")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-23"), label="\n3/23/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-30")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-30"), label="\n3/30/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-05")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-05"), label="\n4/5/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-18")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-18"), label="\n4/18/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-25")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-25"), label="\n4/25/22", y=20), colour="#424242", angle=90) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme(axis.text.x=element_text(angle=40, hjust=1,face="bold", size = "13"))+
  theme(axis.text.y=element_text(face="bold", size = "15"))+
  
  labs(x = "Date", 
       y = "Wind Speed (mph)",
       title = "Mount Washington Observatory 2021-2022 Winter Wind Speed")

   
##Precipitation and Snow##

df1 <- data.frame(precip, snow, date)
df2 <- tidyr::pivot_longer(df1, cols=c('precip', 'snow'), names_to='variable', 
                           values_to="value")
head(df2)
ggplot(df2, aes(x=date, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-29")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2021-10-29"), label="\n10/29/21", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2021-11-20"), label="\n11/20/21", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-21")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-01-21"), label="\n1/21/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-01"), label="\n2/1/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-24"), label="2/24/22\n", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-26")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-26"), label="\n2/26/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-09")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-09"), label="\n3/9/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-23")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-23"), label="\n3/23/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-30")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-30"), label="\n3/30/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-18")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-04-18"), label="\n4/18/22", y=10), colour="red", angle=90) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b %d")+
  theme(axis.text.x=element_text(angle=40, hjust=1)) +
  labs(x = "Date", 
       y = "Recorded Depth (inches)", 
       title = "Mount Washington Observatory 2021-2022 Winter Precipitation")

##Temperature##

ggplot(mydata, aes(date, temp, group = 1)) + 
  geom_line(color="steelblue")+
  geom_hline(yintercept=32, linetype="dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2021-10-29")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2021-10-29"), label="\n10/29/21", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2021-11-20"), label="\n11/20/21", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-21")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-01-21"), label="\n1/21/22", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-01"), label="\n2/1/22", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-24"), label="2/24/22\n", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-26")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-26"), label="\n2/26/22", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-09")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-09"), label="\n3/9/22", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-23")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-23"), label="\n3/23/22", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-30")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-30"), label="\n3/30/22", y=-15), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-18")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-04-18"), label="\n4/18/22", y=-15), colour="red", angle=90) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b %d")+
  theme(axis.text.x=element_text(angle=40, hjust=1)) +
  labs(x = "Date", 
       y = "Average Daily Temperature (deg F)", 
       title = "Mount Washington Observatory 2021-2022 Winter Temperature")

##Precipitation##

ggplot(mydata, aes(x=date, y=precip)) + 
  geom_line(color="#f7681b")+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-29")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2021-10-29"), label="\n10/29/21", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2021-11-20"), label="\n11/20/21", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-21")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-01-21"), label="\n1/21/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-01"), label="\n2/1/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-24"), label="2/24/22\n", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-26")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-26"), label="\n2/26/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-09")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-09"), label="\n3/9/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-23")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-23"), label="\n3/23/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-30")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-30"), label="\n3/30/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-18")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-04-18"), label="\n4/18/22", y=4), colour="red", angle=90) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b %d")+
  theme(axis.text.x=element_text(angle=40, hjust=1)) +

  labs(x = "Date", 
       y = "Total Daily Precipitation (inches)", 
       title = "Mount Washington Observatory 2021-2022 Winter Precipitation")

##Cumulative Precipitation##

ggplot(mydata, aes(date, cum_precip, group = 1)) + 
  geom_line(color="#7738d6")+
  geom_vline(xintercept = as.numeric(as.Date("2021-10-29")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2021-10-29"), label="\n10/29/21", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2021-11-20"), label="\n11/20/21", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-21")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-01-21"), label="\n1/21/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-01"), label="\n2/1/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-24"), label="2/24/22\n", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-26")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-02-26"), label="\n2/26/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-09")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-09"), label="\n3/9/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-23")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-23"), label="\n3/23/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-30")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-03-30"), label="\n3/30/22", y=4), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-18")), linetype= 1, color = "#fff785", size = 1)+
  geom_text(aes(x=as.Date("2022-04-18"), label="\n4/18/22", y=4), colour="red", angle=90) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b %d")+
  theme(axis.text.x=element_text(angle=40, hjust=1)) +
  
  labs(x = "Date", 
       y = "Total Precipitation (inches)", 
       title = "Mount Washington Observatory 2021-2022 Winter Cumulative Precipitation")

##Snow##

snow_in <- snow*2.54
ggplot(mydata, aes(date, snow_in, group = 1)) + 
  geom_line(color='#fc8803', size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2021-10-29")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-10-29"), label="\n10/29/21 (Baseline)", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-11-20"), label="\n11/20/21", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-15")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-12-15"), label="\n12/15/21", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-08")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-01-08"), label="\n1/8/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-21")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-01-21"), label="\n1/21/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-01"), label="\n2/1/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-24"), label="2/24/22\n", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-26")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-26"), label="\n2/26/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-09")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-09"), label="\n3/9/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-23")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-23"), label="\n3/23/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-30")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-30"), label="\n3/30/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-05")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-05"), label="\n4/5/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-18")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-18"), label="\n4/18/22", y=20), colour="#424242", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-25")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-25"), label="\n4/25/22", y=20), colour="#424242", angle=90) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme(axis.text.x=element_text(angle=40, hjust=1,face="bold", size = "13"))+
  theme(axis.text.y=element_text(face="bold", size = "15"))+
  
  labs(x = "Date", 
       y = "Snowfall (centimeters)",
       title = "Mount Washington Observatory 2021-2022 Winter Snow")

##Cumulative Snow##
cum_snow_in <- cum_snow*2.54
ggplot(mydata, aes(date, cum_snow_in, group = 1)) + 
  geom_line(color='#00058a', size = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2021-10-29")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-10-29"), label="\n10/29/21 (Baseline)", y=65), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-11-20"), label="\n11/20/21", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-15")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2021-12-15"), label="\n12/15/21", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-08")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-01-08"), label="\n1/8/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-21")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-01-21"), label="\n1/21/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-01")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-01"), label="\n2/1/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-24"), label="2/24/22\n", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-26")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-02-26"), label="\n2/26/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-09")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-09"), label="\n3/9/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-23")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-23"), label="\n3/23/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-30")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-03-30"), label="\n3/30/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-05")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-05"), label="\n4/5/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-18")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-18"), label="\n4/18/22", y=10), colour="red", angle=90) +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-25")), linetype= 1, color = "#424242", size = 1)+
  geom_text(aes(x=as.Date("2022-04-25"), label="\n4/25/22", y=10), colour="red", angle=90) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme(axis.text.x=element_text(angle=40, hjust=1,face="bold", size = "13"))+
  theme(axis.text.y=element_text(face="bold", size = "15"))+
  
  labs(x = "Date", 
       y = "Total Snowfall (centimeters)",
       title = "Mount Washington Observatory 2021-2022 Winter Cumulative Snow")

