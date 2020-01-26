# Compare satellite with HOBO temps
library(tidyverse)
library(ggplot2)
library(lubridate)
Month_order <- c("January","February","March","April","May","June","July","August","September","October","November","December")

sat <- read.csv("Sat_Temps_July_2019.csv")
sat$time <- as.POSIXct(sat$time,tz="")

all_summ <- read.csv("~/Box/projects/NSF WD/HOBOs/WD-temperature/Allsites.summJuly.2019.csv")
all_summ$Day <- as.POSIXct(all_summ$Day,tz="")
all_summ_ave <- all_summ %>%
  group_by(Region,SiteCode,Day) %>%
  summarise(DailyMean=mean(DailyMean))
combo <- inner_join(sat,all_summ_ave,by=c("Region","SiteCode","time"="Day"))

# for(i in length(unique(combo$SiteCode))){
#   model <- lm(DailyMean~CRW_SST,data=subset(combo,SiteCode==i))
#   sig_table[i,"SiteCode"] <- i
#   sig_table[i,"Pvalue"] <- as.character(signif(summary(model)$coef[2,4], 2))
#   sig_table[i,"AdjR2"] <- as.character(signif(summary(model)$adj.r.squared, 2))
# }

combo$Region.SiteCode <- as.factor(paste(combo$Region,combo$SiteCode))
rm(sig_table)
sig_table <- data.frame()
first_write <- 1
for(i in levels(combo$Region.SiteCode)){
  model <- lm(DailyMean~CRW_SST,data=combo[combo$Region.SiteCode==i,])
  sig_table_temp <- data.frame("Region.SiteCode"=i,
                               "Pvalue"=as.character(signif(summary(model)$coef[2,4], 2)),
                               "AdjR2"=as.character(signif(summary(model)$adj.r.squared, 2)),
                               "Slope"=as.character(signif(summary(model)$coef[2,1],2)))
  sig_table_temp <- separate(sig_table_temp,col=Region.SiteCode,into=c("Region","SiteCode"),sep=" ")
  if(first_write==1){
    sig_table <- sig_table_temp
    first_write <- 0
  } else{
    sig_table <- rbind(sig_table,sig_table_temp)
  }
}

sig_table_numeric <- sig_table
sig_table_numeric$Pvalue <- as.numeric(as.character(sig_table_numeric$Pvalue))
sig_table_numeric$AdjR2 <- as.numeric(as.character(sig_table_numeric$AdjR2))
sig_table_numeric$Slope <- as.numeric(as.character(sig_table_numeric$Slope))

ggplot(data=combo[combo$Region=="AK",],aes(x=CRW_SST,y=DailyMean))+geom_point()+
  stat_smooth(method="lm",col="dark grey")+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  geom_text(data=sig_table[sig_table$Region=="AK",],aes(x=13,y=18,label=paste("p =",Pvalue)))+
  geom_text(data=sig_table[sig_table$Region=="AK",],aes(x=13,y=18.5,label=paste("Adj R2 =",AdjR2)))+
  facet_wrap(~SiteCode)+
  xlab("Nighttime SST via satellite (ºC)")+
  ylab("Daily mean temp via HOBO logger (ºC)")+
  labs(title="Comparing satellite and in situ temperatures - Alaska",
       subtitle = "July 2019")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(data=combo[combo$Region=="BC",],aes(x=CRW_SST,y=DailyMean))+geom_point()+
  stat_smooth(method="lm",col="dark grey")+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  geom_text(data=sig_table[sig_table$Region=="BC",],aes(x=14.75,y=16,label=paste("p =",Pvalue)))+
  geom_text(data=sig_table[sig_table$Region=="BC",],aes(x=14.75,y=16.5,label=paste("Adj R2 =",AdjR2)))+
  facet_wrap(~SiteCode)+
  xlab("Nighttime SST via satellite (ºC)")+
  ylab("Daily mean temp via HOBO logger (ºC)")+
  labs(title="Comparing satellite and in situ temperatures - British Columbia",
       subtitle = "July 2019")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))


ggplot(data=combo[combo$Region=="WA",],aes(x=CRW_SST,y=DailyMean))+geom_point()+
  stat_smooth(method="lm",col="dark grey")+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  geom_text(data=sig_table[sig_table$Region=="WA",],aes(x=12,y=14.5,label=paste("p =",Pvalue)))+
  geom_text(data=sig_table[sig_table$Region=="WA",],aes(x=12,y=15,label=paste("Adj R2 =",AdjR2)))+
  facet_wrap(~SiteCode)+
  xlab("Nighttime SST via satellite (ºC)")+
  ylab("Daily mean temp via HOBO logger (ºC)")+
  labs(title="Comparing satellite and in situ temperatures - Washington",
       subtitle = "July 2019")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(data=combo[combo$Region=="OR",],aes(x=CRW_SST,y=DailyMean))+geom_point()+
  stat_smooth(method="lm",col="dark grey")+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  geom_text(data=sig_table[sig_table$Region=="OR",],aes(x=13,y=17.5,label=paste("p =",Pvalue)))+
  geom_text(data=sig_table[sig_table$Region=="OR",],aes(x=13,y=18,label=paste("Adj R2 =",AdjR2)))+
  facet_wrap(~SiteCode)+
  xlab("Nighttime SST via satellite (ºC)")+
  ylab("Daily mean temp via HOBO logger (ºC)")+
  labs(title="Comparing satellite and in situ temperatures - Oregon",
       subtitle = "July 2019")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(data=combo[combo$Region=="BB",],aes(x=CRW_SST,y=DailyMean))+geom_point()+
  stat_smooth(method="lm",col="dark grey")+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  geom_text(data=sig_table[sig_table$Region=="BB",],aes(x=11,y=17.5,label=paste("p =",Pvalue)))+
  geom_text(data=sig_table[sig_table$Region=="BB",],aes(x=11,y=18,label=paste("Adj R2 =",AdjR2)))+
  facet_wrap(~SiteCode)+
  xlab("Nighttime SST via satellite (ºC)")+
  ylab("Daily mean temp via HOBO logger (ºC)")+
  labs(title="Comparing satellite and in situ temperatures - Bodega Bay",
       subtitle = "July 2019")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(data=combo[combo$Region=="SD",],aes(x=CRW_SST,y=DailyMean))+geom_point()+
  stat_smooth(method="lm",col="dark grey")+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  geom_text(data=sig_table[sig_table$Region=="SD",],aes(x=19,y=26.5,label=paste("p =",Pvalue)))+
  geom_text(data=sig_table[sig_table$Region=="SD",],aes(x=19,y=27,label=paste("Adj R2 =",AdjR2)))+
  facet_wrap(~SiteCode)+
  xlab("Nighttime SST via satellite (ºC)")+
  ylab("Daily mean temp via HOBO logger (ºC)")+
  labs(title="Comparing satellite and in situ temperatures - San Diego",
       subtitle = "July 2019")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))
