# movement parameters - step size and turning angle
# based on Statistical Methods Series: Movement Ecology - Theo Michelot momentuHMM 

# Packages ----
library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(sp)
library(adehabitatLT)
library(momentuHMM)

# Importing and organizing data ----
# importando os dados da telemetria e selecionando registros de interesse
#telemetria <- data %>%
  as_tibble() %>%
  filter(!ID %in% c("Yoki", 'Phoenix 1 error')) %>% # exclude animals with gps problem
  arrange(ID, Acquisition.Time) %>%
  mutate(time = as.POSIXct(Acquisition.Time, format = "%Y.%m.%d %H:%M:%S", tz = "America/New_York"))  %>%
  mutate(ID2 = 1:n()) %>% #add column to identify each record
  dplyr::select(ID, GPS.Longitude, GPS.Latitude, time, ID2)
# salvando os dados selecionados
#write.table(telemetria, "gps_data_filtered.txt", row.names = F)

telemetria <- read.csv("gps_data_filtered.csv") %>%
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
#visualizing data
telemetria %>%
  filter(ID == "Ben") %>%
ggplot(., aes(GPS.Longitude, GPS.Latitude, col = ID)) +
  geom_point(size = 0.1) + geom_path(linewidth = 0.1)

# Project to UTM
llcoord <- st_as_sf(telemetria[, c("GPS.Longitude", "GPS.Latitude")], coords = c("GPS.Longitude", "GPS.Latitude"), 
                    crs = CRS("+proj=longlat +datum=WGS84"))
utmcoord <- st_transform(llcoord, crs = CRS("+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"))

# Add Easting-Northing to data 
telemetria[, c("x", "y")] <- st_coordinates(utmcoord)

# Plot Northing vs Easting
telemetria %>%
  filter(ID == "Ben") %>%
ggplot(., aes(x, y, col = ID)) + 
  geom_point(size = 0.1) + geom_path(linewidth = 0.1) +
  coord_equal()

# Table of time intervals in data
plot(table(diff(telemetria$time)), 
     xlab = "time interval (min)", ylab = "count")


# Use function from utility_function.R to split data at gaps 1 hour
telemetria_split <- split_at_gap(data = telemetria, max_gap = 60, shortest_track = 60)

# visualizing
telemetria %>%
  filter(ID == "Alexander") %>%
  ggplot(., aes(x, y, col = ID)) + 
  geom_point(size = 0.1) + geom_path(linewidth = 0.1) + theme(legend.position="none") +
  coord_equal()

telemetria_split %>%
  filter(str_detect(ID, "Alexander")) %>%
  ggplot(., aes(x, y, col = ID)) + 
  geom_point(size = 0.1) + geom_path(linewidth = 0.1) + theme(legend.position="none") +
  coord_equal()

# Create adehabitat trajectory padded with NAs
telemetria_adehabitat <- setNA(ltraj = as.ltraj(xy = telemetria_split[, c("x", "y")], 
                                   date = telemetria_split$time, 
                                   id = telemetria_split$ID), 
                  date.ref = telemetria_split$time[1], 
                  dt = 20, units = "min")

# Transform back to dataframe
telemetria_na <- ld(telemetria_adehabitat)[, c("id", "x", "y", "date")]
colnames(telemetria_na) <- c("ID", "x", "y", "time")

# Prepare data for HMM (compute step lengths and turning angles)
telemetria_hmm1 <-prepData(telemetria_na, type = "UTM")

head(telemetria_hmm1, 10)
summary(telemetria_hmm1)

# step length 
hist(telemetria_hmm1$step, breaks = 100, xlim = c(0, 0.1))

alexander <- telemetria_hmm1 %>%
  filter(str_detect(ID, "Alexander"),
         step >= 0.2) # %>%
  ggplot(., aes(x, y, col = ID)) + 
  geom_point(size = 0.1) + geom_path(linewidth = 0.1) + theme(legend.position="none") +
  coord_equal()

alexander_HMM2 <- fitHMM(alexander, nbStates = 2, dist = dist, Par0 = Par0_2s_alex)
Par0_2s_alex <- list(step = c(0.05, 0.2, 0.05, 0.2), angle = c(0.1, 3))
  
head(telemetria_hmm1)
# turning angle
hist(telemetria_hmm1$angle, breaks = seq(-pi, pi, length = 15))
plot.edf(telemetria_hmm1$angle)

# Observation distributions (step lengths and turning angles)
dist <- list(step = "gamma", angle = "vm")

# Initial parameters
# A short guide to choosing initial parameter values for the estimation in moveHMM -Theo Michelot & Roland Langrock
# We can therefore select one starting value near the lower end of the range of observed step lengths (for state 1, say), and the other starting value such that the corresponding state can capture the steps which we would intuitively associate with more active behaviour (for state 2).
# When the gamma distribution is used to model step lengths, the standard deviation is usually of the same order of magnitude as the mean of the distribution
# We would not expect any other value than 0 or π for the mean turning angle.
# We usually expect the concentration to be larger in the state with larger step lengths
# (step mean 1, step mean 2, step SD 1, step SD 2, zero-mass 1, zero-mass 2) and (angle concentration 1, angle concentration 2)
Par0_2s <- list(step = c(0.05, 0.2, 0.05, 0.2, 0, 0), angle = c(0.1, 3))
Par0_3s <- list(step = c(0.02, 0.1, 0.3, 0.02, 0.1, 0.3, 0, 0, 0), 
                angle = c(0.01, 0.1, 3))

# Fit a 2-state HMM
hmm1_tamandua <- fitHMM(telemetria_hmm1, nbStates = 2, dist = dist, Par0 = Par0_2s)
hmm2_tamandua <- fitHMM(telemetria_hmm1, nbStates = 3, dist = dist, Par0 = Par0_3s)

# Print parameter estimates
hmm1_tamandua
hmm2_tamandua

# Plot estimated distributions and state-coloured tracks
plot(hmm1_tamandua, breaks = 25, ask = FALSE)
plot(hmm1_tamandua, animals = "Alexander-1", breaks = 25, ask = FALSE)

# Plot pseudo-residuals for 2-state and 3-state models
plotPR(hmm1_tamandua)

