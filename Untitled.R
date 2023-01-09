library(moveHMM)
library(dplyr)
library(sf)
library(ggplot2)
library(raster)

#selecionando colunas de interesse
dados = data %>%
    as_tibble() %>%
    # filter(!ID %in% c("Nash", 'Phoenix 1 error')) %>%
    arrange(ID, Acquisition.Time) %>%
    dplyr::select(ID, Acquisition.Time, GPS.Longitude, GPS.Latitude) %>%
    mutate(ID2 = 1:n()) #add coluna com numeracao em ordem para melhor visualizacao no qgis

# salvar shape dos dados de cada espécie
dados.shp <- st_as_sf(dados, coords = c("GPS.Longitude","GPS.Latitude"), crs = 4326)
table(dados$ID) # algumas especies nao foram salvas por descricao de erro no artigo Phoenix 1 error e Yoki
#st_write(dados.shp %>% 
#    filter(ID == "Delphine"), dsn = "raw.data.anteater_Delphine.shp", delete_layer = T)


#calcular step size e turning angle
parametros <- prepData(dados, type = "LL",coordNames = c("GPS.Longitude","GPS.Latitude"))
head(parametros)
summary(parametros)
aggregate(step ~ ID, data = parametros, max)
aggregate(angle ~ ID, data = parametros, mean)
hist(parametros$step, breaks = 10,  xlim = c(0, 1))

# carregar dados datapaper xenarthrans
xenarthrans <- read.csv("/Users/bibianaoliveira/OneDrive/Doutorado/Predicao_ferrovias/Xenarthrans/ecy2663-sup-0001-datas1/NEOTROPICAL_XENARTHRANS_QUALITATIVE.csv", sep = ",")
#writexl::write_xlsx(xenarthrans, "/Users/bibianaoliveira/OneDrive/Doutorado/Predicao_ferrovias/Xenarthrans/Qualitative.xlsx")
reg_myrmeco <- xenarthrans %>%
  filter(SPECIES == "Myrmecophaga tridactyla",
         COUNTRY == "BRAZIL",
         METHOD %in% "Active search",
                       "Active search and vestige",
                       "Camera trap",
                       "Camera trap and vestige",
                       "Capture",
                       "Line transect",
                       "Line transect and camera trap",
                       "Line transect and live trap",
                       "Opportunistic",
                       "Telemetry",
                       "Tracks plot",
                       "Vestige",
                       NA) %>%
  dplyr::select(ORDEMBD, DATA_TYPE, STATE, MUNICIPALITY, LONG_X, LAT_Y)
reg_myrmeco.shp <- st_as_sf(reg_myrmeco, coords = c("LONG_X","LAT_Y"), crs = 4326)
#st_write(reg_myrmeco.shp, dsn = "/Users/bibianaoliveira/OneDrive/Doutorado/Predicao_ferrovias/Xenarthrans/reg_myrmeco.shp", delete_layer = T)

# identificando as classes de vegetacao em cada registro
reg_myrmeco.shpMS <- reg_myrmeco.shp %>%
  filter(STATE == "MATO GROSSO DO SUL") #filtra para o estado
area_BR <- st_read("/Users/bibianaoliveira/OneDrive/Doutorado/Predicao_ferrovias/Dados_BR/Area/area.shp")
reg_in_area <- st_filter(reg_myrmeco.shpMS, area_BR) #filtra apenas os registros na area de interesse
ggplot() + #visualizacao dos pontos
  geom_sf(data = area_BR) +
  geom_sf(data = reg_myrmeco.shpMS) +
  geom_sf(data = reg_in_area, colour = "red")

mapbiomas_area <- raster("/Users/bibianaoliveira/OneDrive/Doutorado/Predicao_ferrovias/Dados_BR/Area/drive-download-20221021T093503Z-001/mapbiomas-brazil-collection-70-00000000000000000000-2018.tif")
mapbiomas_area.df <- as.data.frame(mapbiomas_area, xy = TRUE) %>%
  na.omit()


ggplot(data = mapbiomas_area.df) + #visualizacao dos pontos
  geom_raster(aes(x = x, y = y)) +
  geom_sf(data = reg_in_area, colour = "red") 
