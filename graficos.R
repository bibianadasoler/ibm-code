library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
library(gridExtra)


#===========################# REGULAR LANDSCAPE ##########################
# load data ---------------------------------------------------------------

# setwd("~/Dropbox/IBMs/IBibsM/simulations")
# dir()
myd_regular <- read.csv("/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/results/teste.csv")
myd_regular <- myd_regular %>% as_tibble()
names(myd_regular)




# analyze -----------------------------------------------------------------

myd_regular1 <- myd_regular %>% 
  group_by(type_scenario, perceptual.range, matrix.permeability) %>%
  summarise(crossings = mean(total_crossings),
            crossings_sd = sd(total_crossings),
            prop.top.sections = mean(assess.top.sections),
            prop.top.sections_sd = sd(assess.top.sections))

names(myd_regular1)
ggplot(myd_regular1, aes(factor(perceptual.range), factor(matrix.permeability), fill=prop.top.sections)) +
  geom_raster() +
  facet_wrap(~type_scenario) +
  theme_minimal() +
  scale_fill_viridis("Hotspot\nlikelihood") +
  labs(title = "Hotspots", x="Perceptual range", y="Matrix permeability")

(crossingsmean <- ggplot(myd_regular1, aes(factor(perceptual.range), factor(matrix.permeability), fill=crossings)) +
  geom_raster() +
  facet_wrap(~type_scenario, ncol=1) +
  theme_minimal() +
  scale_fill_viridis("N Crossings") +
  labs(title = "Crossings", x="Perceptual range", y="Matrix permeability"))


(crossings_sd <- ggplot(myd_regular1, aes(factor(perceptual.range), factor(matrix.permeability), fill=crossings_sd)) +
  geom_raster() +
  facet_wrap(~type_scenario, ncol = 1) +
  theme_minimal() +
  scale_fill_viridis("SD Crossings") +
  labs(title = "SD Crossings", x="Perceptual range", y="Matrix permeability"))

grid.arrange(crossingsmean, crossings_sd, ncol = 2)

#===========################# RANDOM LANDSCAPE ###########################

# load data ---------------------------------------------------------------

myd_random <- read.csv("/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/results/teste_random.csv")
myd_random <- myd_random %>% as_tibble()
myd_random
names(myd_random)


# analyze 

myd_random1 <- myd_random %>% 
  select(habitat_area, matrix_area, proportion_habitat_random,
         perceptual.range, matrix.permeability, total_crossings, assess.top.sections) %>%
  mutate(prop.habitat = habitat_area/(habitat_area+matrix_area))

myd_random1
names(myd_random1)
range(myd_random1$prop.habitat)
hist(myd_random1$prop.habitat)

ggplot(myd_random1, aes(prop.habitat, assess.top.sections, col=assess.top.sections)) +
  annotate("rect", xmin=0, xmax=1, ymin=0.5, ymax=1, alpha=0.2, fill="red") +
  annotate("rect", xmin=0, xmax=1, ymin=0.75, ymax=1, alpha=0.2, fill="red") +
  geom_point(size=.1) +
  geom_smooth(size=.5, alpha=.2) +
  scale_x_continuous(breaks = c(.2, .8)) +
  facet_grid(perceptual.range ~ matrix.permeability) +
  scale_color_viridis("") +
  labs(title = "Random landscapes", x="Proportion of habitat", y="Hotspot likelihood") +
  ylim(0,1) +
  theme_bw() +
  theme(legend.position="none",
        text=element_text(size = 12))
  


# setwd("~/Dropbox/IBMs/IBibsM/Images")
# ggsave("Random_lands.tiff", width=18, height=14, units="cm", dpi=150, device = grDevices::tiff,)


#### 
glm(assess.top.sections ~ mean_hab_neighbors, data = myd_regular)
glm(assess.top.sections ~ mean_hab_neighbors, data = myd_regular1)
glm(assess.top.sections ~ mean_hab_neighbors, data = myd_regular)


