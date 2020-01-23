## Extracting site-level temperatures from NOAA 5km satellite data

library(ggplot2)
library(tidyverse)

## Define a function to compare a value to a vector and find the closest value
## Use this to figure out which pixel to extract for a given eelgrass survey site
find_closest <- function(vector, value) {
  temp_vector <- vector - value
  index <- which.min(abs(temp_vector))
  return(index)
  
}

## Read-in data from NOAA Coastal Reef Watch, for full range of NSF sites during July 2019
temps <- read.csv("dhw_5km_f002_01e7_726c.csv",header=TRUE,colClasses = "character")[-1,]
## Subset dataset to look at only the SST and SSTANOMALY (difference compared to long-term mean)
temps2 <- temps[,c(1,2,3,7,8)]
## All columns were assigned class "character" - now convert to numeric, then drop empty rows
temps2$CRW_SST <- as.numeric(temps2$CRW_SST)
temps2$CRW_SSTANOMALY <- as.numeric(temps2$CRW_SSTANOMALY)
temps2$latitude <- as.numeric(temps2$latitude)
temps2$longitude <- as.numeric(temps2$longitude)
temps2 <- na.omit(temps2)
#### Convert times to POSIXct class in order to treat as dates (should be automatic)
temps2$time <- as.POSIXct(temps2$time,tz="")

## Identify full set of lat and long coordinates in the full data set
lat <- as.numeric(unique(temps2$latitude))
lat <- sort(lat)
long <- as.numeric(unique(temps2$longitude))
long <- sort(long)
## Read-in list of eelgrass site coordinates
site_list <- read.csv("site_lat_long.csv")

## Assign intial value to use in for loop
first_write <- 1
## For loop creates a new dataframe that stores the coordinates of the satellite pixels that are closest to each eelgrass site
for (x in 1:NROW(site_list)) {
  # Read site list
  temp_long <- site_list$Long[x]
  temp_lat <- site_list$Lat[x]
  temp_region <- site_list$Region[x]
  temp_sitecode <- site_list$SiteCode[x]
  
  # Compare to unique list
  long_index <- find_closest(long, temp_long)
  lat_index <- find_closest(lat, temp_lat)
  if ( (length(long_index)>0) & (length(lat_index)>0) ) {
    site_pixels_temp <- data.frame("Region" = temp_region, "SiteCode" = temp_sitecode, 
                                   "Lat" = lat[lat_index], "Long" = long[long_index])
    
    # Store in data frame
    if (first_write == 1) {
      site_pixels <- site_pixels_temp
      first_write <- 0
    } else {
      site_pixels <- rbind(site_pixels, site_pixels_temp)
    } 
  }
  
  
}

## Final step - join the satellite dataframe and the site pixel dataframe to extract only pixels that correspond to sites.
site_temps <- inner_join(temps2,site_pixels,by=c("latitude"="Lat","longitude"="Long"))
## Save with a useful filename!
write.csv(site_temps,"Sat_Temps_July_2019.csv",row.names=FALSE)
## Note - only 18 sites appear in the final output. 
## The remaining 13 sites are too close to land and are masked from the SST pixels.(One site, BB.A, has missing coordinates)
## So, SST pixels are available for 6 AK sites, 1 BC site, 5 WA sites, 2 BB, 2 OR, and 2 SD
