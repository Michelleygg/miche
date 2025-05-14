library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd('C:/Users/admin/Dropbox/Apps/GoodNotes 5/Files/Sem 4/DSA3101')

DelayedFlights <- read_csv('DelayedFlights.csv', show_col_types = FALSE)
airports <- read_csv('airports.csv', show_col_types = FALSE)

DelayedFlights <- na.omit(DelayedFlights)
DelayedFlights <- DelayedFlights %>% select("Year","Month","DayofMonth","DayOfWeek","ArrDelay","DepDelay",
                  "Origin","Dest","Distance","CarrierDelay","WeatherDelay",
                  "NASDelay","SecurityDelay","LateAircraftDelay") %>%
                  merge(airports, by.x='Origin', by.y='IATA') %>%
                  merge(airports, by.x='Dest', by.y='IATA')

flights <- function(x, y){
  df <- DelayedFlights %>% filter(CITY.x==x, Month==y)
  ls <- append(distinct(df, CITY.y)$CITY.y, x)
  df <- airports %>% filter(CITY %in% ls) %>%
    mutate(INFO = paste0(AIRPORT, " | ", CITY, ", ", STATE))
}

origin_dest <- function(x, y){
  df <- DelayedFlights %>% filter(CITY.x==x, Month==y) %>%
        group_by(Dest, Origin) %>%
        summarise_at(vars("ArrDelay", "DepDelay", "CarrierDelay","WeatherDelay",
                          "NASDelay","SecurityDelay","LateAircraftDelay"), mean) %>%
        ungroup()
  return(df)
}

dest_origin <- function(x, y){
  df <- a %>% filter(Origin==x, Month==y)
  origin_df <- airports %>% filter(IATA %in% distinct(df, Dest)$Dest) %>%
    mutate(INFO = paste0(AIRPORT, " | ", CITY, ", ", STATE))
  return(origin_df)
}

get_avg <- function(data){
  grp_origin <- group_by(a, Origin) %>% summarise(avg_numdelay=mean(LateAircraftDelay), .groups='drop') %>%
    as.data.frame() %>% merge(airports, by.x='Origin', by.y='IATA')
  df <- group_by(a, Dest) %>% summarise(avg_numdelay=mean(LateAircraftDelay), .groups='drop') %>%
    as.data.frame() %>% merge(grp_origin, by.x='Dest', by.y='Origin')
  colnames(df)[1] <- 'ABBREVIATION'
}

# Geographical map
MainStates <- map_data("world")
ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="lightskyblue1" ) +
  coord_cartesian(xlim = c(-180, -50), ylim = c(10, 75)) +
  labs(x = "Longitude", y = "Latitude", colour = "") +
  geom_point(data=df, aes(x=LONGITUDE, y=LATITUDE, size=avg_numdelay), 
             color = "brown", alpha = .4) + scale_size(name="Flight Delays Across US")
  
MainStates <- map_data("world")
ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="lightskyblue1" ) +
  coord_cartesian(xlim = c(-180, -50), ylim = c(10, 75)) +
  labs(x = "Longitude", y = "Latitude", colour = "") +
  geom_point(data=df, aes(x=LONGITUDE, y=LATITUDE, size=avg_numdelay), 
             color = "darkgreen", alpha = .4) + scale_size(name="Flight Delays Across US")

library(leaflet)
leafMap <- leaflet(data = df) %>%
  addTiles() %>%
  addMarkers(~LONGITUDE, ~LATITUDE, popup = ~AIRPORT, label = ~AIRPORT)
leafMap



origin_dest <- function(x, y, z){
  if (z=="Origin"){
    df <- DelayedFlights %>% filter(CITY.x==x, Month==y) %>%
      group_by(Origin, Dest) %>%
      summarise_at(vars("ArrDelay", "DepDelay", "CarrierDelay","WeatherDelay",
                        "NASDelay","SecurityDelay","LateAircraftDelay"), funs(round(mean(.),2)))
  }else{
    df <- DelayedFlights %>% filter(CITY.y==x, Month==y) %>%
      group_by(Origin, Dest) %>%
      summarise_at(vars("ArrDelay", "DepDelay", "CarrierDelay","WeatherDelay",
                        "NASDelay","SecurityDelay","LateAircraftDelay"), funs(round(mean(.),2)))
  }
  return(df)
}

b <- origin_dest('Yuma', '1', 'Origin') 
b <- b %>%
  subset(select=-c(Origin)) %>%
  gather(var, val , -Dest) 

ggplot(data = b, aes(x=var, y=val)) + geom_line(aes(color = as.factor(Dest), group = as.factor(Dest)))


flights <- function(x, y, z){
  if (z=="Origin"){
    df <- DelayedFlights %>% filter(CITY.x==x, Month==y)
    ls <- append(distinct(df, CITY.y)$CITY.y, x)
    df <- airports %>% filter(CITY %in% ls) %>%
      mutate(INFO = paste0(AIRPORT, " | ", CITY, ", ", STATE), 
             COLOR=ifelse(CITY==x,"orange","green"))
  }else{
    df <- DelayedFlights %>% filter(CITY.y==x, Month==y)
    ls <- append(distinct(df, CITY.x)$CITY.x, x)
    df <- airports %>% filter(CITY %in% ls) %>%
      mutate(INFO = paste0(AIRPORT, " | ", CITY, ", ", STATE), 
             COLOR=ifelse(CITY==x,"green","orange"))
  }
}

c <- flights('Yuma','1','Origin')










































