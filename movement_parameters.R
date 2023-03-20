#### movement parameters - step size and turning angle

# TELEMETRIA ----
#selecionando colunas de interesse
telemetria = data %>%
  as_tibble() %>%
  filter(!ID %in% c("Yoki", 'Phoenix 1 error')) %>%
  arrange(ID, Acquisition.Time) %>%
  # mutate(date = substr(Acquisition.Time, 1, 10)) %>%
  #  mutate(date = gsub(pattern = ".", replacement = "-",x = date, fixed = T)) %>%
  dplyr::select(ID, GPS.Longitude, GPS.Latitude, Acquisition.Time) %>%
  mutate(ID2 = 1:n()) #add coluna com numeracao em ordem para melhor visualizacao no qgis

ano_telemetria <- data %>% #numero registros por ano, sem limpar nem nada, apenas para ideia
  mutate(Year = substr(Acquisition.Time, 1, 4)) %>%
  dplyr::select(Year) %>%
  table(.)

# salvar shape dos dados de cada espécie
telemetria.shp <- st_as_sf(telemetria, coords = c("GPS.Longitude","GPS.Latitude"), crs = 4326) %>%
  st_transform(., CRS("+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"))
table(telemetria.shp$ID) # algumas especies nao foram salvas por descricao de erro no artigo Phoenix 1 error e Yoki
#st_write(telemetria.shp, dsn = "/Users/bibianaoliveira/OneDrive/Doutorado/Predicao_ferrovias/Dados_BR/raw.data.anteater102033.shp", delete_layer = T)


# BAYESMOVE ----
library(bayesmove)
library(move)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

### BUMPUS - tamandua BEN ----
Bumpus <- telemetria.shp %>%
  mutate(Easting = unlist(map(telemetria.shp$geometry, 1)),
         Northing = unlist(map(telemetria.shp$geometry, 2))) %>%
  data_frame(.) %>%
  filter(ID == "Ben") %>%
  mutate(date = gsub(pattern = ".", replacement = "-",x = Acquisition.Time, fixed = T)) %>%
  dplyr::select(ID, Easting, Northing, date)

# calcula step e angle
plot(Bumpus)
Bumpus2<- prep_data(dat = Bumpus, coord.names = c("Easting","Northing"), id = "ID")

sort(table(Bumpus2$dt), decreasing = TRUE)[1:20]

#Round times w/in 2 min of 15 min interval
Bumpus3<- round_track_time(dat = Bumpus2, id = "ID", int = 1200, tol = 120,
                           units = "secs")
sort(table(Bumpus3$dt), decreasing = TRUE)[1:20]
#> 
#>  900 1828 1773 1802 1831 2710 1705 1708 1728 1739 1741 1748 1756 1759 1762 1777 
#>  826    3    2    2    2    2    1    1    1    1    1    1    1    1    1    1 
#> 1780 1781 1788 1793 
#>    1    1    1    1


# Create list from data frame
Bumpus.list<- df_to_list(dat = Bumpus3, ind = "ID")

# Filter observations to keep only those recorded at 15 min (900 s)
Bumpus.filt <- filter_time(dat.list = Bumpus.list, int = 1200) %>% 
  bind_rows
### teste meu
Bumpus3 <- Bumpus3 %>% filter(dt == 1200)
plot(density(na.omit(Bumpus3$step)), main = "")
plot(density(na.omit(Bumpus3$angle)), main = "")
# Define bin number and limits for turning angles
angle.bin.lims<- seq(from=-pi, to=pi, by=pi/4)  #8 bins

# Define bin number and limits for step lengths
dist.bin.lims<- quantile(Bumpus3$step, c(0, 0.25, 0.5, 0.75, 0.90, 1), na.rm = T)  #5 bins

Bumpus.filt2<- discrete_move_var(Bumpus3, lims = list(dist.bin.lims, angle.bin.lims),
                                 varIn = c("step","angle"), varOut = c("SL","TA"))

##Viz discretization of params
#only retain id and discretized step length (SL) and turning angle (TA) columns
discr.var <- Bumpus.filt2 %>%
  dplyr::select(SL, TA) %>% 
  bind_rows() %>%
  pivot_longer(cols = c(SL, TA), names_to = "var", values_to = "bin")

param.prop<- discr.var %>%
  group_by(var, bin) %>%
  summarise(n=n()) %>%
  mutate(prop=n/nrow(Bumpus.filt2)) %>%
  ungroup()  #if don't ungroup after grouping, ggforce won't work
#> `summarise()` has grouped output by 'var'. You can override using the `.groups`
#> argument.

param.prop<- param.prop[-14,]
param.prop[1:5, "value"]<- ((diff(dist.bin.lims)/2) + dist.bin.lims[1:5])
param.prop[6:13, "value"]<- (diff(angle.bin.lims)/2) + angle.bin.lims[1:8]

#plot of discretized distrib
ggplot(data = param.prop %>% filter(var == "SL"), aes(value, prop)) +
  geom_bar(stat = "identity", width = (diff(dist.bin.lims)-0.025),
           fill = "lightblue", color = "black") +
  labs(x = "Step Length (m)", y = "Proportion") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))
ggplot(data = param.prop %>% filter(var == "TA"), aes(value, prop)) +
  geom_bar(stat = "identity", width = (diff(angle.bin.lims)-0.025),
           fill = "firebrick4", color = "black") +
  labs(x = "Turning Angle (rad)", y = "Proportion") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))
# Only retain discretized step length (SL) and turning angle (TA) columns
Bumpus.sub<- subset(Bumpus.filt2, select = c(SL, TA))
set.seed(1)
# Define model params
alpha=0.1  #prior
ngibbs=100 #number of Gibbs sampler iterations
nburn=ngibbs/2  #number of burn-in iterations
nmaxclust=3  #number of maximum possible states (clusters) present

# Run model
dat.res<- cluster_obs(dat=Bumpus.sub, alpha=alpha, ngibbs=ngibbs, nmaxclust=nmaxclust,
                      nburn=nburn)

# Inspect traceplot of log-likelihood
plot(dat.res$loglikel, type = "l")

## Inspect and Plot results
post.seq<- (nburn + 1):ngibbs  #posterior samples

theta<- dat.res$theta[post.seq,]
colnames(theta)<- 1:ncol(theta)
theta1<- colMeans(theta)
theta1<- sort(theta1, decreasing = TRUE)
cumsum(theta1)  #2 states seem optimal; represents > 90% of assigned observations
#>         1         2         3         4         5         6         7         8 
#> 0.5802831 0.9107538 0.9613538 0.9810943 0.9901168 0.9960240 0.9981285 0.9990904 
#>         9        10 
#> 0.9995524 1.0000000


# Extract bin estimates for each possible state from the `phi` matrix of the model results
behav.res<- get_behav_hist(dat = dat.res, nburn = nburn, ngibbs = ngibbs, nmaxclust = nmaxclust,
                           var.names = c("Step Length","Turning Angle"))
behav.res$behav<- factor(behav.res$behav, levels = 1:nmaxclust)

# Plot state-dependent distributions 
ggplot(behav.res, aes(x = bin, y = prop, fill = as.factor(behav))) +
  geom_bar(stat = 'identity') +
  labs(x = "\nBin", y = "Proportion\n") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x.bottom = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.text.x = element_text(face = "bold")) +
  scale_fill_manual(values = c(viridis::viridis(2), rep("grey35", 8)), guide = FALSE) +
  scale_y_continuous(breaks = c(0.00, 0.50, 1.00)) +
  scale_x_continuous(breaks = 1:8) +
  facet_grid(behav ~ var, scales = "free_x")
#> Warning: It is deprecated to specify `guide = FALSE` to remove a guide. Please
#> use `guide = "none"` instead.

## Attribute behaviors to states and extract each of the different estimates
# Using MAP estimate, threshold of 75% assignments from posterior, and most common state
z.post<- as.matrix(dat.res$z.posterior)
z.post2<- t(apply(z.post, 1, function(x) x/sum(x)))
thresh<- 0.75
z.post3<- apply(z.post2, 1, function(x) ifelse(max(x) > thresh, which(x > thresh), NA))
z.post4<- apply(z.post2, 1, function(x) which.max(x))

## Add states to data frame
Bumpus.states<- Bumpus.filt2 %>% 
  mutate(z.map = dat.res$z.MAP,
         z.post.thresh = z.post3,
         z.post.max = z.post4 ,
         obs = ind,
         time1 = NA
  )

n.states<- 2
Bumpus.states$z.map<- ifelse(Bumpus.states$z.map > n.states, NA, Bumpus.states$z.map)
Bumpus.states$z.post.max<- ifelse(Bumpus.states$z.post.max > n.states, NA, Bumpus.states$z.post.max)

# Identify previously filtered observations and merge into final data frame
ind<- setdiff(1:nrow(Bumpus3), Bumpus.states$obs)
omit.df<- Bumpus3[ind,] %>% 
  mutate(obs = ind,
         time1 = NA,
         SL = NA,
         TA = NA,
         z.map = NA,
         z.post.thresh = NA,
         z.post.max = NA)


Bumpus.states2<- rbind(Bumpus.states, omit.df)
Bumpus.states2<- Bumpus.states2[order(Bumpus.states2$obs),]


# Assign names to states
Bumpus.states2<- Bumpus.states2 %>% 
  mutate(across(c('z.map','z.post.thresh','z.post.max'),
                ~case_when(. == 1 ~ "Encamped",
                           . == 2 ~ "Transit",
                           is.na(.) ~ "Unclassified")
  )) %>% 
  mutate(across(c('z.map','z.post.thresh','z.post.max'),
                factor, levels = c('Encamped','Exploratory','Transit','Unclassified')
  ))


Bumpus.states2 %>%   # for estimates based on MAP estimate
  group_by(z.map) %>% 
  tally() %>% 
  mutate(prop = n/sum(n))
#> # A tibble: 3 × 3
#>   z.map            n  prop
#>   <fct>        <int> <dbl>
#> 1 Encamped       437 0.476
#> 2 Transit        340 0.370
#> 3 Unclassified   142 0.155

Bumpus.states2 %>%   # for estimates based on threshold on posterior
  group_by(z.post.thresh) %>% 
  tally() %>% 
  mutate(prop = n/sum(n))
#> # A tibble: 3 × 3
#>   z.post.thresh     n  prop
#>   <fct>         <int> <dbl>
#> 1 Encamped        365 0.397
#> 2 Transit         165 0.180
#> 3 Unclassified    389 0.423

Bumpus.states2 %>%   # for estimates based on mode of posterior
  group_by(z.post.max) %>% 
  tally() %>% 
  mutate(prop = n/sum(n))
#> # A tibble: 3 × 3
#>   z.post.max       n  prop
#>   <fct>        <int> <dbl>
#> 1 Encamped       541 0.589
#> 2 Transit        285 0.310
#> 3 Unclassified    93 0.101

# Map behavioral states for Leroy (w/ MAP estimates for states)
ggplot() +
  geom_path(data = Bumpus.states2, aes(x, y), color="gray60", size=0.25) +
  geom_point(data = Bumpus.states2, aes(x, y, fill=z.map), size=1.5, pch=21, alpha=0.7) +
  geom_point(data = Bumpus.states2 %>%
               group_by(ID) %>%
               slice(which(row_number() == 1)) %>%
               ungroup(), aes(x, y), color = "green", pch = 21, size = 3, stroke = 1.25) +
  geom_point(data = Bumpus.states2 %>%
               group_by(ID) %>%
               slice(which(row_number() == n())) %>%
               ungroup(), aes(x, y), color = "red", pch = 24, size = 3, stroke = 1.25) +
  scale_fill_manual("Behavior",
                    values = c(viridis::viridis(2), "grey50")) +
  labs(x = "Easting", y = "Northing", title = "MAP estimate") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid = element_blank()) +
  guides(fill = guide_legend(label.theme = element_text(size = 12),
                             title.theme = element_text(size = 14)))


# Map behavioral states for Leroy (w/ threshold of posterior samples for states)
ggplot() +
  geom_path(data = Bumpus.states2, aes(x, y), color="gray60", size=0.25) +
  geom_point(data = Bumpus.states2, aes(x, y, fill=z.post.thresh), size=1.5, pch=21, alpha=0.7) +
  geom_point(data = Bumpus.states2 %>%
               group_by(ID) %>%
               slice(which(row_number() == 1)) %>%
               ungroup(), aes(x, y), color = "green", pch = 21, size = 3, stroke = 1.25) +
  geom_point(data = Bumpus.states2 %>%
               group_by(ID) %>%
               slice(which(row_number() == n())) %>%
               ungroup(), aes(x, y), color = "red", pch = 24, size = 3, stroke = 1.25) +
  scale_fill_manual("Behavior", values = c(viridis::viridis(2), "grey50")) +
  labs(x = "Easting", y = "Northing", title = "Threshold on posterior") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid = element_blank()) +
  guides(fill = guide_legend(label.theme = element_text(size = 12),
                             title.theme = element_text(size = 14)))

# Map behavioral states for Leroy (w/ most common states from posterior)
ggplot() +
  geom_path(data = Bumpus.states2, aes(x, y), color="gray60", size=0.25) +
  geom_point(data = Bumpus.states2, aes(x, y, fill=z.post.max), size=1.5, pch=21, alpha=0.7) +
  geom_point(data = Bumpus.states2 %>%
               group_by(ID) %>%
               slice(which(row_number() == 1)) %>%
               ungroup(), aes(x, y), color = "green", pch = 21, size = 3, stroke = 1.25) +
  geom_point(data = Bumpus.states2 %>%
               group_by(ID) %>%
               slice(which(row_number() == n())) %>%
               ungroup(), aes(x, y), color = "red", pch = 24, size = 3, stroke = 1.25) +
  scale_fill_manual("Behavior", values = c(viridis::viridis(2), "grey50")) +
  labs(x = "Easting", y = "Northing", title = "Mode of posterior") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid = element_blank()) +
  guides(fill = guide_legend(label.theme = element_text(size = 12),
                             title.theme = element_text(size = 14)))

### EXEMPLO BAYESMOVE ----
data(leroy)
# Convert from `Move` object to data frame
leroy2<- data.frame(leroy) %>% 
  mutate(id = "leroy") %>% 
  rename(date = study.local.timestamp)
# Calculate step lengths (step), turning angles (angle), and time step (dt) 
leroy2<- prep_data(dat = leroy2, coord.names = c("utm.easting","utm.northing"), id = "id")
# Which dt (time step) is most common?
sort(table(leroy2$dt), decreasing = TRUE)[1:20]  #15 min (900 s) is primary dt
#> 
#> 901 900 902 896 898 903 907 909 890 904 906 910 892 905 874 894 925 927 865 876 
#>  27  24  21  20  20  16  15  15  14  14  14  14  13  13  12  12  12  12  11  11
#Round times w/in 2 min of 15 min interval
leroy3<- round_track_time(dat = leroy2, id = "id", int = 900, tol = 120, time.zone = "UTC",
                          units = "secs")
sort(table(leroy3$dt), decreasing = TRUE)[1:20]
#> 
#>  900 1828 1773 1802 1831 2710 1705 1708 1728 1739 1741 1748 1756 1759 1762 1777 
#>  826    3    2    2    2    2    1    1    1    1    1    1    1    1    1    1 
#> 1780 1781 1788 1793 
#>    1    1    1    1


# Create list from data frame
leroy.list<- df_to_list(dat = leroy3, ind = "id")

# Filter observations to keep only those recorded at 15 min (900 s)
leroy.filt<- filter_time(dat.list = leroy.list, int = 900) %>% 
  bind_rows

# View distributions of data streams
plot(density(na.omit(leroy.filt$step)), main = "")


# Define bin number and limits for turning angles
angle.bin.lims<- seq(from=-pi, to=pi, by=pi/4)  #8 bins

# Define bin number and limits for step lengths
dist.bin.lims<- quantile(leroy.filt$step, c(0, 0.25, 0.5, 0.75, 0.90, 1))  #5 bins


leroy.filt2<- discrete_move_var(leroy.filt, lims = list(dist.bin.lims, angle.bin.lims),
                                varIn = c("step","angle"), varOut = c("SL","TA"))

# Only retain discretized step length (SL) and turning angle (TA) columns
leroy.sub<- subset(leroy.filt2, select = c(SL, TA))

set.seed(1)

# Define model params
alpha=0.1  #prior
ngibbs=10  #number of Gibbs sampler iterations
nburn=ngibbs/2  #number of burn-in iterations
nmaxclust=10  #number of maximum possible states (clusters) present

# Run model
dat.res<- cluster_obs(dat=leroy.sub, alpha=alpha, ngibbs=ngibbs, nmaxclust=nmaxclust,
                      nburn=nburn)

# Inspect traceplot of log-likelihood
plot(dat.res$loglikel, type = "l")
## Inspect and Plot results
post.seq<- (nburn + 1):ngibbs  #posterior samples

theta<- dat.res$theta[post.seq,]
colnames(theta)<- 1:ncol(theta)
theta1<- colMeans(theta)
theta1<- sort(theta1, decreasing = TRUE)
cumsum(theta1)  #2 states seem optimal; represents > 90% of assigned observations
#>         1         2         3         4         5         6         7         8 
#> 0.5802831 0.9107538 0.9613538 0.9810943 0.9901168 0.9960240 0.9981285 0.9990904 
#>         9        10 
#> 0.9995524 1.0000000
#> 
# Extract bin estimates for each possible state from the `phi` matrix of the model results
behav.res<- get_behav_hist(dat = dat.res, nburn = nburn, ngibbs = ngibbs, nmaxclust = nmaxclust,
                           var.names = c("Step Length","Turning Angle"))
behav.res$behav<- factor(behav.res$behav, levels = 1:nmaxclust)

# Plot state-dependent distributions 
ggplot(behav.res, aes(x = bin, y = prop, fill = as.factor(behav))) +
  geom_bar(stat = 'identity') +
  labs(x = "\nBin", y = "Proportion\n") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x.bottom = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.text.x = element_text(face = "bold")) +
  scale_fill_manual(values = c(viridis::viridis(2), rep("grey35", 8)), guide = FALSE) +
  scale_y_continuous(breaks = c(0.00, 0.50, 1.00)) +
  scale_x_continuous(breaks = 1:8) +
  facet_grid(behav ~ var, scales = "free_x")
#> Warning: It is deprecated to specify `guide = FALSE` to remove a guide. Please
#> use `guide = "none"` instead.
#> ## Attribute behaviors to states and extract each of the different estimates
# Using MAP estimate, threshold of 75% assignments from posterior, and most common state
z.post<- as.matrix(dat.res$z.posterior)
z.post2<- t(apply(z.post, 1, function(x) x/sum(x)))
thresh<- 0.75
z.post3<- apply(z.post2, 1, function(x) ifelse(max(x) > thresh, which(x > thresh), NA))
z.post4<- apply(z.post2, 1, function(x) which.max(x))


## Add states to data frame
leroy.states<- leroy.filt2 %>% 
  mutate(z.map = dat.res$z.MAP,
         z.post.thresh = z.post3,
         z.post.max = z.post4)

n.states<- 2
leroy.states$z.map<- ifelse(leroy.states$z.map > n.states, NA, leroy.states$z.map)
leroy.states$z.post.max<- ifelse(leroy.states$z.post.max > n.states, NA, leroy.states$z.post.max)


# Identify previously filtered observations and merge into final data frame
ind<- setdiff(1:nrow(leroy3), leroy.states$obs)
omit.df<- leroy3[ind,] %>% 
  mutate(obs = ind,
         time1 = NA,
         SL = NA,
         TA = NA,
         z.map = NA,
         z.post.thresh = NA,
         z.post.max = NA)


leroy.states2<- rbind(leroy.states, omit.df)
leroy.states2<- leroy.states2[order(leroy.states2$obs),]



# Assign names to states
leroy.states2<- leroy.states2 %>% 
  mutate(across(c('z.map','z.post.thresh','z.post.max'),
                ~case_when(. == 1 ~ "Encamped",
                           . == 2 ~ "Transit",
                           is.na(.) ~ "Unclassified")
  )) %>% 
  mutate(across(c('z.map','z.post.thresh','z.post.max'),
                factor, levels = c('Encamped','Exploratory','Transit','Unclassified')
  ))




#calcular step size e turning angle ----
# adehabitatHS
parametros <- prepData(telemetria, type = "LL",coordNames = c("GPS.Longitude","GPS.Latitude"))
head(parametros)
summary(parametros)
aggregate(step ~ ID, data = parametros, max)
aggregate(angle ~ ID, data = parametros, mean)
hist(parametros$step, breaks = 10,  xlim = c(0, 1))



