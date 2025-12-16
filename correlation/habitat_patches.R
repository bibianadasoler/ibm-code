##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## script to extract number of patches from 21999 rasters generated in the habitat
# amount model and to correlate proportion of habitat and number of patches 
# 
## to run this script will be need to download the folder with all raster, which
## is heavier than github allowed
## to download the folder with the rasters go to: https://1drv.ms/f/c/8dcd8842240deb54/IgBU6w0kQojNIICN3iICAAAAAQ7_-yWg5IIdN_TVGhDSuAA?e=vFDpIU 
#

# loading packages
library(dplyr)
library(landscapemetrics)

# to access the folder with the rasters
rasters <- list.files(path = here::here("rasters"),
                      full.names = TRUE, 
                      ignore.case = TRUE)

# function to get the number of patches of habitat for each raster
raster_patches_numbers <- function(raster_file, class) {
  patches_number <- terra::rast(raster_file) %>%
    get_patches(., class = 1)
  patches_number <- length(terra::unique(patches_number[[1]][[1]]))
  get_run_id <- raster_file %>% 
    stringr::str_split_i(., "/", -1) %>%
    stringr::str_split_i(., ".asc", 1)
  id_patches <- c(get_run_id, patches_number)
  return(id_patches)
}
raster_patches_numbers(rasters, 1)

# to save the number of patches for each raster
id_patches <- data.frame("run_id" = NA,
                         "habitat_patches" = NA)
for (i in 1:length(rasters)) ( 
  id_patches[i, ] <- raster_patches_numbers(rasters[i], class = 1)
)
id_patches$habitat_patches <- as.integer(id_patches$habitat_patches)

### to correlate number of patches and proportion of habitat
# load cvs file with run_id and proportion_of_habitat for each raster
prop_habitat <- read.csv(here::here("run_id_prop_habitat.csv"))

# to combine both objects to correlate
to_correlate <-left_join(prop_habitat, id_patches, by = "run_id")

# correlation
cor.test(to_correlate$proportion_of_habitat, to_correlate$habitat_patches)
plot(to_correlate$proportion_of_habitat, to_correlate$habitat_patches)

