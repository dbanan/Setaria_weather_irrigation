#Setaria_weather_data_180531
#5/31/18 

#weather data bring in SoyFACE and ISWS data
#bring in table of irrigation 

#May meeting action item: calculate background and awning P-PET

#6/10/18 migrate data and script to project 
#6/10/18 version control 



library(ggplot2)
library(reshape2)


setwd("/rsync/box/Setaria/2018 May data meeting/weather and timelines")
save.image("Setaria_weather_irrigation.Rdata") 
load("Setaria_weather_irrigation.Rdata")

######IRRIGATION RECORDS#######
#2015
irr_15DR<-read.csv("/rsync/box/Setaria/2018 May data meeting/weather and timelines/Setaria_2015_irrigation.csv", header=T, stringsAsFactors=FALSE)

#change NA per.awning to 0 
irr_15DR$per.awning[is.na(irr_15DR$per.awning)]<-0

#convert gallons applied to rainfall centimeters and millimeters 
#3785.41 cubic cm in a gallon, 89250 cm^2 is irrigated footprint 2015
irr_15DR$applied_cm<-(irr_15DR$per.awning*3785.41)/89250
irr_15DR$applied_mm<-irr_15DR$applied_cm*10

#calendar column 
irr_15DR$calendar<-as.Date(irr_15DR$date)

#2013
irr_13DR_truck<-read.csv("/de/github/dbanan/auth/Setaria_weather_irrigation/data/Setaria_2013_irrigation_truck.csv", header=T, stringsAsFactors=FALSE)
irr_13DR_drip<-read.csv("/de/github/dbanan/auth/Setaria_weather_irrigation/data/Setaria_2013_irrigation_drip.csv", header=T, stringsAsFactors=FALSE)

#average truck waterings to day rather than awning 
#combine with drip irrigation 
#convert to rainfall millimeters 

#2014
#irrigation data is spotty...due to flooding events and treatment reassignments? 


#######ISWS data##########
isws<-read.table("/rsync/box/Setaria/2018 May data meeting/weather and timelines/ISWS_weather_data/allstations/CMIDAY.txt",header=T,sep="\t", stringsAsFactors=FALSE)

#ditch last rows with data metadata and first row with units 
#keep columns of interest 
isws1<-isws[-c(1,10548:10557), c("year","month","day","sol_rad","max_air_temp","min_air_temp","avg_air_temp","max_rel_hum","min_rel_hum","avg_rel_hum","avg_dewpt_temp","precip","pot_evapot")]

#format to numeric 
isws2<-sapply(isws1, as.numeric)
isws3<-as.data.frame(isws2)

#convert negative precip to NA 
isws3$precip[(isws3$precip<0)]<-NA

#make a date column
isws3$calendar<-as.Date(paste(isws3$year, isws3$month, isws3$day, sep="-"))

#convert precip and pot_evapot to mm
isws3$pot_evapot_mm<-isws3$pot_evapot*25.4
isws3$precip_mm<-isws3$precip*25.4

#convert temperatures from F to C 
isws3$isws_air_temp_max_C<-(isws3$max_air_temp-32)*(5/9)
isws3$isws_air_temp_min_C<-(isws3$min_air_temp-32)*(5/9)
isws3$isws_air_temp_avg_C<-(isws3$avg_air_temp-32)*(5/9)


####WANT AWNING AND AMBIENT P-PET####
#ambient ppet (mm)
isws3$ppet_amb<-isws3$precip_mm-isws3$pot_evapot_mm


######JOIN ISWS AND IRRIGATION#####
#2015
#trim down to 2015 field season 
event_15DR<-data.frame(day=as.Date(c("2015-07-06", "2015-07-15", "2015-08-28", "2015-09-23")), 
                       DOY=c(187, 196, 240, 266),
                       event=c("DR sowing", "DR transplanting", "DR harvest start", "DR harvest end"))

#trim isws to experiment time frame  
isws_15DR<-subset(isws3, calendar>="2015-07-06" & calendar<="2015-09-23")

#join isws and irrigation data
isws_15DR<-merge(isws_15DR, irr_15DR, by=c("calendar"), all=TRUE)

#convert NA to 0 for applied_cm and applied_mm
isws_15DR$applied_cm[is.na(isws_15DR$applied_cm)]<-0
isws_15DR$applied_mm[is.na(isws_15DR$applied_mm)]<-0

#calculate awning treatment P-PET (mm) 
isws_15DR$ppet_wet<-isws_15DR$applied_mm-isws_15DR$pot_evapot_mm
isws_15DR$ppet_dry<-0-isws_15DR$pot_evapot_mm




plot(isws_15DR$calendar, isws_15DR$ppet_amb)
plot(isws_15DR$calendar, isws_15DR$ppet_wet)
plot(isws_15DR$calendar, isws_15DR$ppet_dry)



#now try 7 day running average 
#try some running averages stuff 

f7<-rep(1/7, 7)

y_amb<-as.data.frame(filter(isws_15DR$ppet_amb, f7, sides=2))
y_wet<-as.data.frame(filter(isws_15DR$ppet_wet, f7, sides=2))
y_dry<-as.data.frame(filter(isws_15DR$ppet_dry, f7, sides=2))

y_all<-cbind(y_amb, y_wet, y_dry)
colnames(y_all)<-c("running_amb","running_wet","running_dry")

isws_15DR1<-cbind(isws_15DR, y_all)

plot(isws_15DR1$calendar, isws_15DR1$running_wet, type="n")
lines(isws_15DR1$calendar, isws_15DR1$running_amb, col="grey")
lines(isws_15DR1$calendar, isws_15DR1$running_wet, col="blue")
lines(isws_15DR1$calendar, isws_15DR1$running_dry, col="red")




















#join SoyFACE and ISWS data
big_weather<-merge(isws3, ave24, by=c("calendar"))



ggplot(data=big_weather, aes(x=AirTemp_Max, y=isws_air_temp_max_C))+geom_point()
ggplot(data=big_weather, aes(x=AirTemp_Min, y=isws_air_temp_min_C))+geom_point()


ggplot(data=big_weather, aes(x=RH_Min, y=min_rel_hum))+geom_point()

ggplot(data=big_weather, aes(x=RAIN_CSI_Tot, y=RAIN_ETI_Tot))+geom_point()

ggplot(data=big_weather)+
  geom_line(aes(x=calendar, y=isws_air_temp_max_C), color="red")+
  geom_line(aes(x=calendar, y=AirTemp_Max), color="blue")

ggplot(data=big_weather)+
  geom_point(aes(x=calendar, y=min_rel_hum), color="red")+
  geom_point(aes(x=calendar, y=RH_Min), color="blue")

ggplot(data=big_weather)+
  geom_bar(aes(x=calendar, y=RAIN_ANDY_Tot), stat="identity", color="red")+
  geom_bar(aes(x=calendar, y=precip), stat="identity", color="blue")

