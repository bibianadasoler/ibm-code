# permeability layer using telemetry data

# quais dados usar? todos ou com as nuvens excluidas? 

# Load GPS data
telemetria_barbara <- read.csv("gps_data_filtered.csv") %>% 
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>%
  group_by(ID) %>%
  mutate(diff = c(0, diff(time))) %>%
  filter(diff <= 0 | diff >= 18) %>% # remove records obtained in a time interval < 18 min, but 0 is kept to mantain the first record
  dplyr::select(ID, GPS.Longitude, GPS.Latitude, time) %>%
  filter(ID == "Barbara",
         between(time, as.Date('2018-08-24'), as.Date('2018-09-24'))) %>% 
  mutate(ID = case_when(
    between(time, as.Date('2018-08-24'), as.Date('2018-09-10')) ~ "Barbara-1",
    between(time, as.Date('2018-09-11'), as.Date('2018-09-24')) ~ "Barbara-2")) %>% ungroup() %>%
  st_as_sf(., coords = c("GPS.Longitude", "GPS.Latitude"), crs = CRS("+proj=longlat +datum=WGS84")) %>%
  st_transform(., crs = CRS("+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"))

# Load habitat data
habitat_barbara <- raster("/Users/bibianaterra/Desktop/barbara102033.tif")

# Quick look
plot(habitat_barbara)

raster_points <- rasterToPoints(habitat_barbara, spatial = T)
class_avail_barbara <- raster::extract(x = habitat_barbara, y = raster_points)
class_avail_barbara <- as.data.frame(table(class_avail_barbara)) 
class_avail_barbara <- class_avail_barbara %>%
  mutate(prop_avail = (Freq)/sum(Freq)) %>%
  rename(., class = class_avail_barbara) %>%
  dplyr::select(class, prop_avail) %>%
  pull(prop_avail, name = class)

class_use_barbara <- telemetria_barbara %>%
  mutate(class = as.factor(raster::extract(x = habitat_barbara, y = telemetria_barbara))) %>%
  mutate(class = factor(class, levels = c( 1, 5, 14, 15, 26, 45, 50, 65))) %>%
  with(., table(ID, class)) %>%
  as.data.frame(.) %>% group_by(ID) %>%
  mutate(prop_use = (Freq)/sum(Freq)) %>%
  dplyr::select(ID, class, prop_use) %>%
  pivot_wider(names_from = class, values_from = prop_use) %>%
  column_to_rownames(var = "ID") %>% as.data.frame(.) 

wi <- widesII(class_use_barbara, class_avail_barbara, avknown = T, alpha = 0.01)
wi$wi

permeabilidade_classe <- rescale(wi$wi, from = c(min(wi$wi), max(wi$wi)), to = (c(1,100)))

