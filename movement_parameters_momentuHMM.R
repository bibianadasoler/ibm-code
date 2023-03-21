# movement parameters - step size and turning angle
# based on Statistical Methods Series: Movement Ecology - Theo Michelot momentuHMM 

# Packages ----
library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(sp)

# Importing and organizang data ----
# importando os dados da telemetria e selecionando colunas de interesse
telemetria = data %>%
  as_tibble() %>%
  filter(!ID %in% c("Yoki", 'Phoenix 1 error'),
         GPS.Fix.Attempt == "Resolved QFP") %>% # filter only data with better gps position
  arrange(ID, Acquisition.Time) %>%
  mutate(time = as.POSIXct(Acquisition.Time, format = "%Y.%m.%d %H:%M:%S"))  %>%
  dplyr::select(ID, GPS.Longitude, GPS.Latitude, time)
  #mutate(ID2 = 1:n()) #add coluna com numeracao em ordem para melhor visualizacao no qgis
  # mutate(date = substr(Acquisition.Time, 1, 10)) %>%
  #  mutate(date = gsub(pattern = ".", replacement = "-",x = date, fixed = T)) %>%

#visualizing data
telemetria %>%
  filter(ID == "Ben") %>%
ggplot(., aes(GPS.Longitude, GPS.Latitude, col = ID)) +
  geom_point(size = 0.1) + geom_path(linewidth = 0.1)

# Project to UTM
llcoord <- st_as_sf(telemetria[, c("GPS.Longitude", "GPS.Latitude")], coords = c("GPS.Longitude", "GPS.Latitude"), 
                    crs = CRS("+proj=longlat +datum=WGS84"))
utmcoord <- st_transform(llcoord, crs = CRS("+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"))

# Add Easting-Northing to data (in km)
telemetria[, c("x", "y")] <- st_coordinates(utmcoord)/1000

# Plot Northing vs Easting
telemetria %>%
  filter(ID == "Ben") %>%
ggplot(., aes(x, y, col = ID)) + 
  geom_point(size = 0.1) + geom_path(linewidth = 0.1) +
  coord_equal()

# Table of time intervals in data
plot(table(diff(telemetria$time)), 
     xlab = "time interval (min)", ylab = "count")

# Use function from utility_function.R to split data at gaps > 3 hours
telemetria_split <- split_at_gap(data = telemetria, max_gap = 3*60, shortest_track = 15)

telemetria %>%
  filter(ID == "Alexander") %>%
  ggplot(., aes(x, y, col = ID)) + 
  geom_point(size = 0.1) + geom_path(linewidth = 0.1) + theme(legend.position="none") 
  coord_equal()

data_split %>%
  filter(str_detect(ID, "Alexander")) %>%
  ggplot(., aes(x, y, col = ID)) + 
  geom_point(size = 0.1) + geom_path(linewidth = 0.1) + theme(legend.position="none") 
  coord_equal()

