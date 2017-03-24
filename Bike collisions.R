# Bicycle collisions in Seattle

# Dataset creation
  # Data taken from data.seattle.gov
  # Date taken: 03/05/2017
  # https://data.seattle.gov/d/v7k9-7dn4?category=Transportation&view_name=SDOT-Collisions
  # only want to look at bicycle collisions right now, so click "Filter" tab and 
  # set filter to "COLLISIONTYPE is Cycles"
  # then, click Export, and download as csv

# Libraries
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(ggmap)

# Read in data file
bikedat <- read_csv("SDOT_Collisions.csv", guess_max = 1500)

# data cleaning/review
str(bikedat)

# Convert incident date, incident time to date and time classes
bikedat$INCDATE <- mdy_hms(bikedat$INCDATE)
bikedat$INCDTTM <- parse_date_time(bikedat$INCDTTM, orders = "mdy IMS p")

# Extract latitude and longitude from shape variable
  # Lat: take substring of Shape variable
bikedat$Lat <- as.numeric(substr(bikedat$Shape, start = 2, stop = 18))
  # Long: Split Shape at ", " and unlist (lapply) to get 2nd element,
  # then take substring to trim the ")" and convert to numeric
bikedat$Long <- as.numeric(substr(lapply(strsplit(bikedat$Shape, ", "), '[', 2), start=0, stop = 13))


# Get Seattle map data
seamap <- get_map("Seattle", maptype = "roadmap", zoom = 11)
BGT <- get_map("Green Lake, Seattle", maptype = "roadmap", zoom=12)
  # Using Green Lake as search term so map is centered around Green Lake/north of ship canal
Ballard <- get_map("Ballard, Seattle", maptype = "roadmap", zoom=13)

# Basic map of all collisions, colored by year
ggmap(seamap) +
  geom_point(aes(x=Long, y=Lat, color=factor(year(INCDATE))), data = bikedat)

# Map of 2016 collisions, colored by time of day
ggmap(seamap) +
  geom_point(aes(x=Long, y=Lat, color=factor(LIGHTCOND)), data = bikedat[year(bikedat$INCDATE)==2016,])+
  labs(title = "Most Accidents in 2016 Happened During Daylight")

# Which accidents occurred on the Burke Gilman?
ggmap(BGT) +
  geom_point(aes(x=Long, y=Lat, color=factor(LIGHTCOND)), data = bikedat[grepl("BURKE GILMAN", bikedat$LOCATION),])+
  labs(title = "Burke Gilman Accidents")

ggmap(Ballard)+
  geom_point(aes(x=Long, y=Lat, color=factor(year(INCDATE))), data = bikedat[year(bikedat$INCDATE)>= 2012 & year(bikedat$INCDATE)<= 2016,])+
  labs(title = "NW Seattle Bike Accidents, 2012-2016")

# Create variable "Striker" to describe "Who Hit Who", which can be determined from SDOT collision code
  # dplyr's case_when() is useful for a more readable if-else syntax
bikedat <- bikedat %>% mutate(Striker = factor(case_when(.$SDOT_COLCODE >= 10 & .$SDOT_COLCODE <= 29 ~ "Vehicle in Operation",
                                    .$SDOT_COLCODE >= 30 & .$SDOT_COLCODE <= 49 ~ "Driverless Vehicle",
                                    .$SDOT_COLCODE >= 50 & .$SDOT_COLCODE <= 69 ~ "Cyclist",
                                    .$SDOT_COLCODE >= 70 & .$SDOT_COLCODE <= 76 ~ "Pedestrian or Non-Traffic Cyclist",
                                    .$SDOT_COLCODE >= 80 ~ "Pedestrian Struck",
                                    TRUE ~ as.character(NA))))

ggmap(Ballard)+
  geom_point(aes(x=Long, y=Lat, color=Striking), data = bikedat[year(bikedat$INCDATE)>= 2012 & year(bikedat$INCDATE)<= 2016,])+
  labs(title = "NW Seattle Bike Accidents, 2012-2016")

# How many accidents had fatalities each year?
bikedat %>% group_by(year(INCDATE)) %>% 
  summarise(nfatal = sum(FATALITIES)) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=nfatal, group=1))+
  geom_line()

