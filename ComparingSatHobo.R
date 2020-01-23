# Compare satellite with HOBO temps
library(tidyverse)
library(ggplot2)
library(lubridate)
Month_order <- c("January","February","March","April","May","June","July","August","September","October","November","December")

sat <- read.csv("WA.NOAA.temps.csv")
sat$time <- as.POSIXct(sat$time,tz="")

WA_full <- read.csv("~/Box/projects/NSF WD/HOBOs/WD-temperature/WA.2019.alltemps.csv")
WA_full$DateObs <- as.character(WA_full$DateObs)
WA_full$DateObs <- as.POSIXct(WA_full$DateObs,format="%m/%d/%y   %H:%M")
WA_full$Day <- floor_date(WA_full$DateObs,unit = "day")
WA_full$Month <- floor_date(WA_full$DateObs,unit="month")
WA_full$Month.w <- as.character.Date(WA_full$Month,format="%B")
WA_full$Month.w <- ordered(WA_full$Month.w,levels=Month_order)

WA_summ <- WA_full %>%
  group_by(Region, SiteCode,SiteName,TidalHeight,Month,Day)%>%
  summarise(DailyMean=mean(TempC),DailyMax=max(TempC),
            DailySD=sd(TempC),
            DailySE=sd(TempC)/sqrt(length(TempC)))
WA_summ$Month.w <- as.character.Date(WA_summ$Month,format="%B")
WA_summ$Month.w <- ordered(WA_summ$Month.w,levels=Month_order)

combo <- inner_join(sat,WA_summ,by=c("SiteCode","time"="Day"))
rm(sig_table)
sig_table <- data.frame()

for(i in combo$SiteCode){
  model <- lm(DailyMean~CRW_SST,data=subset(combo,SiteCode==i))
  sig_table[i,"SiteCode"] <- i
  sig_table[i,"Pvalue"] <- as.character(signif(summary(model)$coef[2,4], 2))
  sig_table[i,"AdjR2"] <- as.character(signif(summary(model)$adj.r.squared, 2))
}

geom_text(data=disease_most_summ2,aes(x=Region,y=0.52,label=count),position=position_dodge(width=0.75))
  
ggplot(data=combo,aes(x=CRW_SST,y=DailyMean))+geom_point()+
  stat_smooth(method="lm",col="dark grey")+
  geom_text(data=sig_table,aes(x=12,y=14.5,label=paste("p =",Pvalue)))+
  geom_text(data=sig_table,aes(x=12,y=15,label=paste("Adj R2 =",AdjR2)))+
  facet_wrap(~SiteCode)+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

class(WA_summ$Day)
class(sat$time)
