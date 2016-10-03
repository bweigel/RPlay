library(rgdal)
library(rgeos)
library(ggplot2)
library(dplyr)
library(maptools)

################################################################
################## load EU-shapefile of NUTS  areas ############
################################################################

dsn <- "/home/mori/Data/GIS/NUTS 2013/data/NUTS_RG_10M_2013.shp"
ogrListLayers(dsn)
EU <- readOGR(dsn, ogrListLayers(dsn)[1])

## plot EU
#plot(EU)

# read meta data
meta <- read.csv("~/Data/GIS/NUTS 2013/data/NUTS_AT_2013.csv", stringsAsFactors = F)

# subset EU data to GERMANY
SUBSET <- meta[which(meta$CNTR_CODE == "DE"),]$NUTS_ID
SUBSET <- match(SUBSET,EU$NUTS_ID) 
SUBSET <- SUBSET[!is.na(SUBSET)]
DE <- EU[SUBSET,]

# plot germany & NUTS1,2,3
plot(DE)
# plot Germany NUTS 2
plot(DE[DE$STAT_LEVL_ == 2,])

# make datafames for pretty plotting in ggplot
DE.pl0 <- fortify(DE[DE$STAT_LEVL_ == 0,], region = "NUTS_ID")
DE.pl1 <- fortify(DE[DE$STAT_LEVL_ == 1,], region = "NUTS_ID")
DE.pl2 <- fortify(DE[DE$STAT_LEVL_ == 2,], region = "NUTS_ID")
DE.pl3 <- fortify(DE[DE$STAT_LEVL_ == 3,], region = "NUTS_ID")

# plot Germany (NUTS 0) with Länder (NUTS 1) and Kreisen (NUTS 3)
ggplot() +
  geom_polygon(data=DE.pl0, aes(x=long, y=lat, group=group), fill="white", color=alpha("grey10", 1), size=0.75) +
  geom_polygon(data=DE.pl1, aes(x=long, y=lat, group=group), fill=NA, color=alpha("grey10", 1), size=0.5) +
  geom_polygon(data=DE.pl3, aes(x=long, y=lat, group=group), fill=NA, color=alpha("grey60", 0.6), size=0.5) +
  coord_map()

# plot Saxony-Anhalt
ggplot() +
  geom_polygon(data=DE.pl1 %>% filter(id == "DEE"), aes(x=long, y=lat, group=group), fill=NA, color=alpha("grey10", 1), size=0.5) +
  geom_polygon(data=DE.pl3 %>% filter(grepl("(DEE)", x = DE.pl3$id)), aes(x=long, y=lat, group=group), fill=NA, color=alpha("grey60", 0.6), size=0.5) +
  coord_map()

########################################################################################################################################
# load NUTS to PLZ mapping data (http://ec.europa.eu/eurostat/de/web/nuts/correspondence-tables/postcodes-and-nuts)#####################
########################################################################################################################################
library(stringr)

PLZ_NUTS <- read.table("~/Data/mapping_PLZ_de_NUTS-2010.txt", sep = ";", header=T, stringsAsFactors = F, colClasses = c("character", "character"))
# change last two digits oif PLZ to XX
PLZ_NUTS$CODE <- str_replace(PLZ_NUTS$CODE, "[[:digit:]]{2}$", "XX")

PLZ_NUTS <- distinct(PLZ_NUTS)
