library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(readxl)
library(scales)
library(adehabitatHS)


# DADOS DE ENTRADA ----
area_estudo <- read_sf("./shapes", layer = "cerrado_pantanal_wgs84") 
plot(area_estudo)

# registros de ocorrencia - datapaper xenarthrans
xenarthrans <- read.csv("/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/Xenarthrans/ecy2663-sup-0001-datas1/NEOTROPICAL_XENARTHRANS_QUALITATIVE.csv", sep = ",")


## filtrar registros de ocorrencia na area de interesse ----

reg_myrmeco_ocorrencia <- xenarthrans %>%
  mutate(TIME = COL_END_YR - COL_STRT_YR) %>%
  filter(SPECIES == "Myrmecophaga tridactyla", # filtra apenas a especie
         COL_STRT_YR >= "2013", # filtra registros a partir do ano com mais de 100 registros
         COL_END_YR >= "2013",
         METHOD %in% c("ACTIVE SEARCH", # filtra registros dos metodos escolhidos
                       "ACTIVE SEARCH AND VESTIGE",
                       "CAMERA TRAP",
                       "CAMERA TRAP AND VESTIGE",
                       "CAPTURE",
                       "LINE TRANSECT",
                       "LINE TRANSECT AND CAMERA TRAP",
                       "LINE TRANSECT AND LIVE TRAP",
                       "OPPORTUNISTIC",
                       "TELEMETRY",
                       "TRACKS PLOT",
                       "VESTIGE"),
         TIME == 0) %>% # filtra registros em que o monitoramento aconteceu no mesmo ano
  dplyr::select(ORDEMBD, LONG_X, LAT_Y, METHOD, COL_END_YR) %>% # seleciona apenas algumas colunas da planilha original para manter
  st_as_sf(., coords = c("LONG_X","LAT_Y"), crs = 4326) %>% #transforma em arquivo shape
  st_filter(., area_estudo) #%>% # filtra apenas os registros na area de interesse
  #st_write(., dsn = "/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/Xenarthrans/reg_myrmeco_ocorrencia.shp", delete_layer = T) # salva
#converti para 102033 manualmente no qgis
#reg_myrmeco_ocorrencia_UTM <- read_sf("./shapes", layer = "reg_myrmeco_ocorrencia_102033") 

ggplot() + #visualizacao dos dados
  geom_sf(data = area_estudo) +
  geom_sf(data = reg_myrmeco_ocorrencia)

# USO DA PAISAGEM ----
## raster da area de estudo para cada ano com registro de ocorrencia ----
## reclassificacao ----
# os rasters do mapbiomas foram reclassificados agrupando nivel 4 em nivel 3 (ex: soja para lavoura temporaria)
mapbiomas_reclass <- matrix(data = c(3, 3, #forest formation
                                   5, 5, #magrove
                                   4, 4, #savanna formation
                                   12, 12, #grassland
                                   13, 13, #other non forest formation
                                   29, 29, #rocky outcrop
                                   32, 32, #salt flat
                                   11, 11, #wetland
                                   47, 36, #citrus to perennial crops
                                   46, 36, #coffee to perennial crops
                                   48, 36, #other to perennial crops
                                   62, 19, #cotton to temporary crops
                                   41, 19, #other to temporary crops
                                   40, 19, #rice to temporary crops
                                   39, 19, #soybean to temporary crops
                                   20, 19, #sugarcane to temporary crops
                                   9, 9, #forest plantation
                                   21, 21, #land use mosaic
                                   15, 15, #pasture
                                   23, 23, #beach and dune
                                   30, 30, #mining
                                   25, 25, #other non vegetated area
                                   24, 24, #urban area
                                   31, 26, #aquaculture to water
                                   33, 26, #river, lake and ocean to water
                                   27, 27), #non observed
                          ncol = 2, byrow = T)
compress <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9")
# raster original do mapbiomas reclassificado para os grupos acima
# para salvar datatype = "INT1U"
#mapbiomas_2018_RECLASS <- reclassify(mapbiomas_2018, mapbiomas_reclass)
#raster::writeRaster(mapbiomas_2018_clipped_RECLASS, "/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/mapbiomas_raster/mapbiomas-2018_RECLASS.tif", format = "GTIff", options = compress, datatype = "INT1U")

## raster da area de estudo para cada ano com registro de ocorrencia ----
mapbiomas_2013_RECLASS <- raster("/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/mapbiomas_raster/mapbiomas-2013_RECLASS.tif")
mapbiomas_2014_RECLASS <- raster("/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/mapbiomas_raster/mapbiomas-2014_RECLASS.tif")
mapbiomas_2015_RECLASS <- raster("/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/mapbiomas_raster/mapbiomas-2015_RECLASS.tif")
mapbiomas_2016_RECLASS <- raster("/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/mapbiomas_raster/mapbiomas-2016_RECLASS.tif")
mapbiomas_2017_RECLASS <- raster("/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/mapbiomas_raster/mapbiomas-2017_RECLASS.tif")
#plot(mapbiomas_2013)
#plot(mapbiomas_2013_RECLASS)

## registros de ocorrencia por ano ----
reg_ocorr2013 <- reg_myrmeco_ocorrencia %>%
  dplyr::filter(COL_END_YR == 2013)
reg_ocorr2014 <- reg_myrmeco_ocorrencia %>%
  dplyr::filter(COL_END_YR == 2014)
reg_ocorr2015 <- reg_myrmeco_ocorrencia %>%
  dplyr::filter(COL_END_YR == 2015)
reg_ocorr2016 <- reg_myrmeco_ocorrencia %>%
  dplyr::filter(COL_END_YR == 2016)
reg_ocorr2017 <- reg_myrmeco_ocorrencia %>%
  dplyr::filter(COL_END_YR == 2017)

## classe de vegetacao de cada registro por ano ----
reg_class2013 <- raster::extract(x = mapbiomas_2013_RECLASS, y = reg_ocorr2013)
reg_class2013 <- data.frame(table(reg_class2013)) %>%
  rename(., id = reg_class2013) %>%
  mutate(p_use = (Freq)/sum(Freq))

reg_class2014 <- raster::extract(x = mapbiomas_2014_RECLASS, y = reg_ocorr2014)
reg_class2014 <- data.frame(table(reg_class2014)) %>%
  rename(., id = reg_class2014) %>%
  mutate(p_use = (Freq)/sum(Freq))

reg_class2015 <- raster::extract(x = mapbiomas_2015_RECLASS, y = reg_ocorr2015)
reg_class2015 <- data.frame(table(reg_class2015)) %>%
  rename(., id = reg_class2015) %>%
  mutate(p_use = (Freq)/sum(Freq))

reg_class2016 <- raster::extract(x = mapbiomas_2016_RECLASS, y = reg_ocorr2016)
reg_class2016 <- data.frame(table(reg_class2016)) %>%
  rename(., id = reg_class2016) %>%
  mutate(p_use = (Freq)/sum(Freq))

reg_class2017 <- raster::extract(x = mapbiomas_2017_RECLASS, y = reg_ocorr2017)
reg_class2017 <- data.frame(table(reg_class2017)) %>%
  rename(., id = reg_class2017) %>%
  mutate(p_use = (Freq)/sum(Freq))

## proporcao de classe disponivel na area durante todos os anos ----
pland <- read_xlsx("area_ha_Cerr_Pant.xlsx", sheet = 1) %>%
  mutate(id = as.character(id)) %>%
  dplyr::mutate_if(is.numeric, round, 8)
classes <- pull(pland, id)

## calculando razao de selecao (resistencia o complemento disso) ----
### 2013 ----
used2013 <- pland %>% 
  left_join(reg_class2013) %>% 
  dplyr::select(id, pland2013, p_use) %>%
  dplyr::mutate_if(is.numeric, round, 8) %>%
  filter(pland2013 > 0) %>%
  pull(p_use, name = id) %>%
  replace(is.na(.), 0)
avail2013 <- pland %>%
  filter(pland2013 > 0) %>%
  pull(pland2013, name = id)

wi2013 <- widesI(u = used2013, a = avail2013)
razao2013 <- wi2013$wi

### 2014 ----
used2014 <- pland %>% 
  left_join(reg_class2014) %>% 
  dplyr::select(id, pland2014, p_use) %>%
  dplyr::mutate_if(is.numeric, round, 8) %>%
  filter(pland2014 > 0) %>%
  pull(p_use, name = id) %>%
  replace(is.na(.), 0)
avail2014 <- pland %>%
  filter(pland2014 > 0) %>%
  pull(pland2014, name = id)

wi2014 <- widesI(u = used2014, a = avail2014)
razao2014 <- wi2014$wi

### 2015 ----
used2015 <- pland %>% 
  left_join(reg_class2015) %>% 
  dplyr::select(id, pland2015, p_use) %>%
  dplyr::mutate_if(is.numeric, round, 8) %>%
  filter(pland2015 > 0) %>%
  pull(p_use, name = id) %>%
  replace(is.na(.), 0)
avail2015 <- pland %>%
  filter(pland2015 > 0) %>%
  pull(pland2015, name = id)

wi2015 <- widesI(u = used2015, a = avail2015)
razao2015 <- wi2015$wi

### 2016 ----
used2016 <- pland %>% 
  left_join(reg_class2016) %>% 
  dplyr::select(id, pland2016, p_use) %>%
  dplyr::mutate_if(is.numeric, round, 8) %>%
  filter(pland2016 > 0) %>%
  pull(p_use, name = id) %>%
  replace(is.na(.), 0)
avail2016 <- pland %>%
  filter(pland2016 > 0) %>%
  pull(pland2016, name = id)

wi2016 <- widesI(u = used2016, a = avail2016)
razao2016 <- wi2016$wi

### 2017 ----
used2017 <- pland %>% 
  left_join(reg_class2017) %>% 
  dplyr::select(id, pland2017, p_use) %>%
  dplyr::mutate_if(is.numeric, round, 8) %>%
  filter(pland2017 > 0) %>%
  pull(p_use, name = id) %>%
  replace(is.na(.), 0)
avail2017 <- pland %>%
  filter(pland2017 > 0) %>%
  pull(pland2017, name = id)

wi2017 <- widesI(u = used2017, a = avail2017)
razao2017 <- wi2017$wi

plot(wi2013$wi, type = "l")
points(wi2014$wi, type = "l", col = "red", add = T)
points(wi2015$wi, type = "l", col = "blue", add = T)
points(wi2016$wi, type = "l", col = "green", add = T)
points(wi2017$wi, type = "l", col = "yellow", add = T)

results_use <- data.frame(cbind(used2013, used2014, used2015, used2016, used2017, avail2013, avail2014, 
                    avail2015, avail2016, avail2017, razao2013, razao2014, razao2015, razao2016, razao2017)) %>%
  mutate(., RazaoMedia = rowMeans(dplyr::select(., starts_with("razao")), na.rm = TRUE))
plot(results_use$RazaoMedia)

# PERMEABILIDADE DA PAISAGEM ----
permeabilidade_classe <- rescale(results_use$RazaoMedia, from = c(min(results_use$RazaoMedia), max(results_use$RazaoMedia)), to = (c(1,100)))

# reclassificacao do raster do ano da telemetria ----
# raster do ano com mais dados de telemetria 2018
mapbiomas_2018_clipped <- raster("/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/mapbiomas_raster/mapbiomas-2018_RECLASS_clipped.tif")
plot(mapbiomas_2018_clipped)
# matrix com valores para reclassificacao
matrix_permeability_reclass <- matrix(data = c(3, 64.838185,
                                   5, 1,
                                   4, 15.473302,
                                   12, 49.778496,
                                   29, 100,
                                   32, 1,
                                   11, 25.504940,
                                   36, 1,
                                   19, 3.027372,
                                   9, 44.525273,
                                   21, 13.635563,
                                   15, 4.529468,
                                   23, 1,
                                   30, 1,
                                   25, 1,
                                   24, 13.431142,
                                   26, 31.562660,
                                   27, 1,
                                   0, NA,
                                   13, NA),
                          ncol = 2, byrow = T)

# reclassificacao do raster de 2018
mapbiomas_2018_permeability <- reclassify(mapbiomas_2018_clipped, matrix_permeability_reclass)
plot(mapbiomas_2018_permeability)
raster::writeRaster(mapbiomas_2018_permeability, "landscape_netlogo_telemetria.tif", format = "GTIff", overwrite = T, datatype = "INT1U", options = compress)


