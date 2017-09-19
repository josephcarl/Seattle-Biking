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
library(plotly)

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
m.2016 <- ggmap(seamap) +
  geom_point(aes(x=Long, y=Lat, color=factor(LIGHTCOND)), data = bikedat[year(bikedat$INCDATE)==2016,])+
  labs(title = "Most Accidents in 2016 Happened During Daylight")
m.2016

  # Which accidents occurred on the Burke Gilman?
ggmap(BGT) +
  geom_point(aes(x=Long, y=Lat, color=factor(LIGHTCOND)), data = bikedat[grepl("BURKE GILMAN", bikedat$LOCATION),])+
  labs(title = "Burke Gilman Accidents",
       subtitle = "Not Many Accidents are Identified as being on the Burke Gilman")

ggmap(Ballard)+
  geom_point(aes(x=Long, y=Lat, color=factor(year(INCDATE))), data = bikedat[year(bikedat$INCDATE)>= 2012 & year(bikedat$INCDATE)<= 2016,])+
  labs(title = "NW Seattle Bike Accidents, 2012-2016",
       subtitle = "Most Accidents Occur on Arterial Streets")

# Create variable "Striker" to describe "Who Hit Who", which can be determined from SDOT collision code
  # dplyr's case_when() is useful for a more readable if-else syntax
bikedat <- bikedat %>% 
  mutate(Striker = factor(case_when(.$SDOT_COLCODE >= 10 & .$SDOT_COLCODE <= 29 ~ "Vehicle in Operation",
                                    .$SDOT_COLCODE >= 30 & .$SDOT_COLCODE <= 49 ~ "Driverless Vehicle",
                                    .$SDOT_COLCODE >= 50 & .$SDOT_COLCODE <= 69 ~ "Cyclist",
                                    .$SDOT_COLCODE >= 70 & .$SDOT_COLCODE <= 76 ~ "Pedestrian or Non-Traffic Cyclist",
                                    .$SDOT_COLCODE >= 80 ~ "Pedestrian Struck",
                                    TRUE ~ as.character(NA))))

ggmap(Ballard)+
  geom_point(aes(x=Long, y=Lat, color=Striker), data = bikedat[year(bikedat$INCDATE)>= 2012 & year(bikedat$INCDATE)<= 2016,])+
  labs(title = "NW Seattle Bike Accidents, 2012-2016",
       subtitle = "Most Accidents Occur on Arterial Streets")

# How many accidents had fatalities each year?
bikedat %>% group_by(year(INCDATE)) %>% 
  summarise(nfatal = sum(FATALITIES)) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=nfatal, group=1))+
  geom_line()

bikedat %>% group_by(year(INCDATE), Striker) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, fill=Striker))+
  geom_col()+
  labs(y="Number of Accidents",
       title="Accidents Have Increased Over Time",
       subtitle="Proportion of Accidents Caused by Vehicles Increasing")

bikedat %>% group_by(year(INCDATE), Striker) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, fill=Striker))+
  geom_col(position = "fill")+
  labs(y="Proportion of Accidents",
       title="Accidents Have Increased Over Time",
       subtitle="Proportion of Accidents Caused by Vehicles Increasing")

bikedat %>% group_by(year(INCDATE), JUNCTIONTYPE) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, fill=JUNCTIONTYPE))+
  geom_col(position = "fill")+
  labs(y="Proportion of Accidents",
       title="Largest Share of Accidents At Intersections")

bikedat %>% group_by(year(INCDATE), JUNCTIONTYPE) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, fill=JUNCTIONTYPE))+
  geom_col()+
  labs(y="Number of Accidents",
       title="Largest Share of Accidents At Intersections")

bikedat %>% group_by(year(INCDATE), JUNCTIONTYPE) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, group=JUNCTIONTYPE, color=JUNCTIONTYPE))+
  geom_line()+
  labs(y="Number of Accidents",
       title="Largest Share of Accidents At Intersections")


bikedat %>% group_by(year(INCDATE), LIGHTCOND) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, fill=LIGHTCOND))+
  geom_col()+
  labs(y="Proportion of Accidents",
       title="Most Accidents are During Daylight")

bikedat %>% group_by(year(INCDATE), LIGHTCOND) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, color=LIGHTCOND, group=LIGHTCOND))+
  geom_line()+
  labs(y="Number of Accidents",
       title="Most Accidents are During Daylight")


bikedat %>% group_by(year(INCDATE), LIGHTCOND) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, fill=LIGHTCOND))+
  geom_col(position = "fill")+
  labs(y="Proportion of Accidents",
       title="Most Accidents are During Daylight")

bikedat %>% group_by(year(INCDATE), ROADCOND) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, fill=ROADCOND))+
  geom_col(position = "fill")+
  labs(y="Proportion of Accidents",
       title="Most Accidents Have Dry Road Conditions")

bikedat %>% group_by(year(INCDATE), WEATHER) %>% 
  summarise(n = n()) %>% 
  rename(year = `year(INCDATE)`) %>% 
  ggplot(aes(x=year, y=n, fill=WEATHER))+
  geom_col(position = "fill")+
  labs(y="Proportion of Accidents",
       title="Most Accidents Have Clear Weather Conditions")

s.person <- ggplot(bikedat, aes(x=PERSONCOUNT, y=SERIOUSINJURIES, color=Striker))+
  geom_point(position = position_jitter(.10))+
  facet_grid(~ PEDCYLCOUNT)
s.person

plotly::ggplotly(s.person)
# 
plot_ly(bikedat, x = ~PERSONCOUNT, y = ~SERIOUSINJURIES, color = ~Striker, type = "scatter")

plotly::plot_ly(x=1:10, y=1:10)

ggplot(bikedat, aes(x=PEDCOUNT, y=PEDCYLCOUNT, color=Striker))+
  geom_point(position = position_jitter(.10))+
  facet_grid(~ PERSONCOUNT)


# Given that an accident occurs at an intersection, who is most likely to be the Striker?

# Construct a frequency table of striker and junction type
junc_strik <- bikedat %>% group_by(Striker, JUNCTIONTYPE) %>% 
  summarise(accid = n())

junc_strik_freq <- xtabs(accid ~ Striker + JUNCTIONTYPE, data = junc_strik)

# Is Striker type independent of JunctionType?
  # Use a chi-squared test
chisq.test(junc_strik_freq)
  # p-value = 0, reject Ho that striker is independent of junction type

junc_strike_probs <- as.data.frame(prop.table(junc_strik_freq, 2))


# Do the accidents with serious injuries/fatalities have anything in common? (location, time of day, striker, road conditions, etc)

# Does time of day affect # of crashes?
bikedat %>% mutate(HoD = hour(INCDTTM)) %>% 
  group_by(Striker, HoD) %>% 
  summarise(numCrash = n()) %>% 
  ggplot(., aes(x=HoD, y=numCrash, group=Striker, color=Striker))+
  geom_line()

bikedat %>% mutate(HoD = hour(INCDTTM)) %>% 
  group_by(JUNCTIONTYPE, HoD) %>% 
  summarise(numCrash = n()) %>% 
  ggplot(., aes(x=HoD, y=numCrash, group=JUNCTIONTYPE, color=JUNCTIONTYPE))+
  geom_line()

bikedat %>% mutate(HoD = hour(INCDTTM)) %>% 
  group_by(ROADCOND, HoD) %>% 
  summarise(numCrash = n()) %>% 
  ggplot(., aes(x=HoD, y=numCrash, group=ROADCOND, color=ROADCOND))+
  geom_line()

bikedat %>% mutate(HoD = hour(INCDTTM)) %>% 
  group_by(WEATHER, HoD) %>% 
  summarise(numCrash = n()) %>% 
  ggplot(., aes(x=HoD, y=numCrash, group=WEATHER, color=WEATHER))+
  geom_line()

bikedat %>% mutate(DoW = factor(weekdays(INCDTTM), levels=c("Monday", "Tuesday", "Wednesday",
                                                            "Thursday", "Friday", "Saturday", 
                                                            "Sunday"))) %>% 
  group_by(ROADCOND, DoW) %>% 
  summarise(numCrash = n()) %>% 
  ggplot(., aes(x=DoW, y=numCrash, group=ROADCOND, color=ROADCOND))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

bikedat %>% mutate(DoW = factor(weekdays(INCDTTM), levels=c("Monday", "Tuesday", "Wednesday",
                                                            "Thursday", "Friday", "Saturday", 
                                                            "Sunday"))) %>% 
  group_by(WEATHER, DoW) %>% 
  summarise(numCrash = n()) %>% 
  ggplot(., aes(x=DoW, y=numCrash, group=WEATHER, color=WEATHER))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

bikedat %>% mutate(DoW = factor(weekdays(INCDTTM), levels=c("Monday", "Tuesday", "Wednesday",
                                                            "Thursday", "Friday", "Saturday", 
                                                            "Sunday")),
                   HoD = hour(INCDTTM)) %>% 
  group_by(HoD, DoW) %>% 
  summarise(numCrash = n()) %>% 
  ggplot(., aes(x=HoD, y=DoW, fill=numCrash))+
  geom_raster()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_fill_gradient(low="lightgreen", high = "darkred", trans="sqrt")
