# numero de patches em cada raster
library(terra)
library(landscapemetrics)
library(dplyr)
library(purrr)
library(stringr)

# lendo a pasta com os rasters para ver todos os arquivos
rasters <- list.files(
  path = "./results/rasters",
  full.names = TRUE, #retorna o caminho completo do arquivo
  ignore.case = TRUE #ignora se o valor é maiúsculo ou minúsculo
)

raster_patches_numbers <- function(raster_file, class) {
  patches_number <- rast(raster_file) %>%
    get_patches(., class = class)
  patches_number <- length(terra::unique(patches_number[[1]][[1]]))
  
  get_run_id <- raster_file %>% 
    str_split_i(., "/", -1) #fatia o endereço completo dos arquivos baseado em / e fica só com o final 
  id_patches <- c(get_run_id, patches_number)
  return(id_patches)
}
raster_patches_numbers(rasters[7524], 1)

id_patches <- data.frame("run_id" = NA,
                 "habitat_patches" = NA)

for (i in 1:length(rasters)) ( 
  id_patches[i, ] <- raster_patches_numbers(rasters[i], class = 1)
  )
write.csv(id_patches, file = "./results/id_pacthes.csv", row.names = F)