# permeability layer using telemetry data

# Packages ----
library(dplyr)
library(tibble)
library(tidyr)
library(sf)
library(raster)
library(adehabitatHR)
library(adehabitatHS)
library(momentuHMM)
library(scales)

# quais dados usar? todos ou com as nuvens excluidas? 
# excluimos registros com distancias menores que a media do estado 2

# Load GPS data ----
telemetria_barbara <- read.csv("gps_data_filtered.csv") %>% 
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) %>%
  group_by(ID) %>%
  mutate(diff = c(0, diff(time))) %>%
  filter(diff <= 0 | diff >= 18) %>% # remove records obtained in a time interval < 18 min, but 0 is kept to mantain the first record
  dplyr::select(ID, GPS.Longitude, GPS.Latitude, time) %>%
  filter(ID == "Barbara",
         between(time, as.Date('2018-08-24'), as.Date('2018-09-24'))) %>% 
  mutate(ID = case_when(
    between(time, as.Date('2018-08-24'), as.Date('2018-09-11')) ~ "Barbara-1",
    between(time, as.Date('2018-09-11'), as.Date('2018-09-24')) ~ "Barbara-2")) %>% ungroup() %>%
  st_as_sf(., coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326) %>%
  st_transform(., crs = CRS("+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")) #%>%
  #st_write(., dsn = "/Users/bibianaterra/Desktop/barbara_pontos.shp", delete_layer = T)

# removendo registros menores que a media do state 2
telemetria_barbara_LongSteps <- telemetria_barbara %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>% # criando colunas com as coordenadas x e y
  as.data.frame(.) %>%
  prepData(., coordNames = c("x", "y"), type = "UTM") %>%
  filter(step > 29) %>%
  st_as_sf(., coords = c("x", "y"), crs = CRS("+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")) %>%
  dplyr::select(ID, time, geometry)

head(telemetria_barbara_LongSteps)

# Load habitat data ----
habitat_barbara <- raster("/Users/bibianaterra/Desktop/barbara2.tif")
names(habitat_barbara) <- c("class")
plot(habitat_barbara)

# Minimum Convex Polygons ----
# um para cada individuo
telemetria_barbara_MCP <-  telemetria_barbara_LongSteps %>%
  dplyr::select(ID) %>%
  as_Spatial(.)

MCP_ids <- mcp(telemetria_barbara_MCP, percent = 100) # area em hectares

plot(habitat_barbara)
points(telemetria_barbara_MCP, col = telemetria_barbara_MCP$ID,  pch = 20)
lines(MCP_ids, col = alpha(1:5, 0.5))

# Extracao classe por pixel ----
# extrair classes de cada pixel da area da telemetria
# deixando os dados no formato para entrar na analise wi
all_classes = c(4, 11, 12, 15, 21, 25)

class_avail_polygons <- MCP_ids %>%
  raster::extract(x = habitat_barbara, y = ., df = T) %>%
  mutate(class = factor(class, levels = all_classes)) %>%
  table(.) %>%
  as.data.frame(.) %>%
  pivot_wider(names_from = class, values_from = Freq) %>%
  column_to_rownames(var = "ID")

# extrair classes de cada registro de cada bicho
# deixando os dados no formato para entrar na analise wi
class_use_barbara <- telemetria_barbara_LongSteps %>%
  mutate(class = as.factor(raster::extract(x = habitat_barbara, y = telemetria_barbara_LongSteps))) %>%
  mutate(class = factor(class, levels = all_classes)) %>%
  with(., table(ID, class)) %>%
  as.data.frame(.) %>%
  pivot_wider(names_from = class, values_from = Freq) %>%
  column_to_rownames(var = "ID") %>% as.data.frame(.) 
 
# analise wi ----
# razao de selecao uso x disponivel
wi <- widesIII(class_use_barbara, class_avail_polygons, avknown = T, alpha = 0.01)
wi$wi

# padronizando na escala de 1 a 100
permeabilidade_classe <- rescale(wi$wi, from = c(min(wi$wi), max(wi$wi)), to = (c(1,100)))

# raster reclassificado ----
# reclassificando raster com valores de permeabilidade
permeability_values <- permeabilidade_classe %>%
  as.data.frame(.) %>%
  rownames_to_column(., var = "ID") %>%
  rename("permeability" = ".") %>%
  mutate(ID = as.numeric(ID),
         permeability = as.numeric(permeability)) %>%
  as.matrix(.)
  
habitat_permeability <- reclassify(habitat_barbara, permeability_values)
