library(ggplot2)

## read-in data from NOAA Coastal Reef Watch, for full range of NSF sites during July 2019
temps <- read.csv("dhw_5km_8211_97f8_5ea5.csv",header=TRUE)[-1,]
## Subset dataset to look at only the SST and SSTANOMALY (difference compared to long-term mean)
temps2 <- temps[,c(1,2,3,7,8)]
## The variables are read in as factors - convert to numeric, then character and drop empty rows
temps2$CRW_SST <- as.character(temps2$CRW_SST)
temps2$CRW_SST <- as.numeric(temps2$CRW_SST)
temps2 <- na.omit(temps2)

temps2$CRW_SSTANOMALY <- as.character(temps2$CRW_SSTANOMALY)
temps2$CRW_SSTANOMALY <- as.numeric(temps2$CRW_SSTANOMALY)
temps2$latitude <- as.character(temps2$latitude)
temps2$latitude <- as.numeric(temps2$latitude)
temps2$longitude <- as.character(temps2$longitude)
temps2$longitude <- as.numeric(temps2$longitude)

## Convert times to POSIXct class in order to treat as dates (should be automatic)
temps2$time <- as.character(temps2$time)
temps2$time <- as.POSIXct(temps2$time,tz="")

#Subset for WA region using bounding lat and long 
WA <- temps2[temps2$latitude<48.775 & temps2$latitude>48.42501 & temps2$longitude>-123.225 & temps2$longitude< -122.725,]

# WA$time <- as.character(WA$time)
# WA$time <- as.POSIXct(WA$time,tz="")

# Pixels are identified by the coordinates of the mid-point 
lat <- unique(WA$latitude)
lat
long <- unique(WA$longitude)
long

# Use site coordinates to identify the correct pixels to match each site code
# Note the site E coordinates were not working for some reason so used the surrounding pixels to identify 
WA$SiteCode <- ifelse(WA$latitude==48.47500 & WA$longitude== -122.975,"A",
                      ifelse(WA$latitude==48.47500 & WA$longitude== -123.075, "B",
                             ifelse(WA$latitude==48.67501 & WA$longitude== -122.975, "C",
                                    ifelse(WA$latitude==48.72500 & WA$longitude== -123.075, "D",
                                           ifelse(WA$latitude<48.62501 & WA$latitude>48.52500
                                                  & WA$longitude< -122.875 & WA$longitude> -123.025, "E","NA")))))
WA.sites <- subset(WA,SiteCode!="NA")
ggplot(WA.sites,aes(x=SiteCode,y=CRW_SST))+geom_boxplot()+
  stat_summary(fun.y=mean,geom="point",shape=8,size=2)+
  xlab("Site Code")+
  ylab("SST (ºC)")+
  labs(title = "SST in July 2019 at Washington Sites",
       subtitle = "5-km resolution")+
  theme_bw()

ggplot(WA.sites,aes(x=SiteCode,y=CRW_SSTANOMALY))+geom_boxplot()+
  stat_summary(fun.y=mean,geom="point",shape=8,size=2)+
  xlab("Site Code")+
  ylab("SST Anomaly (ºC)")+
  labs(title = "SST Anomaly in July 2019 at Washington Sites",
       subtitle = "5-km resolution")+
  theme_bw()

WA.A <- WA[WA$latitude==48.47500 & WA$longitude==-122.975,]
WA.B <- WA[WA$latitude==48.47500 & WA$longitude==-123.075,]
WA.C <- WA[WA$latitude==48.67501 & WA$longitude==-122.975,]
WA.D <- WA[WA$latitude==48.72500 & WA$longitude==-123.075,]
WA.E <- WA[WA$latitude==48.57500 & WA$longitude==-122.925,]

WA$SiteCode <- ifelse(WA$latitude==48.47500 & WA$longitude== -122.975,"A",
                      ifelse(WA$latitude==48.47500 & WA$longitude== -123.075, "B",
                             ifelse(WA$latitude==48.67501 & WA$longitude== -122.975, "C",
                                    ifelse(WA$latitude==48.72500 & WA$longitude== -123.075, "D",
                                           ifelse(WA$latitude<48.62501 & WA$latitude>48.52500
                                                  & WA$longitude< -122.875 & WA$longitude> -123.025, "E","NA")))))
WA.sites <- subset(WA,SiteCode!="NA")
ggplot(WA.sites,aes(x=SiteCode,y=CRW_SST))+geom_boxplot()
ggplot(WA.sites,aes(x=SiteCode,y=CRW_SSTANOMALY))+geom_boxplot()

ggplot(WA.A,aes(x=time,y=CRW_SSTANOMALY))+geom_point()

subset(WA,longitude==-122.925 )
class(temps2$time)
class(temps2$CRW_SSTANOMALY)
class(temps2$latitude)
x <- findInterval(WA$latitude,48.4685835)
library(data.table)
dt = data.table(WA) 
setkey(dt, latitude) # sorts the data
dt[J(48.4685835), roll = "nearest"]

WA.A <- WA[WA$latitude ==48.4685,]
