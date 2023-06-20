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

myd_random <- read.csv("/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/results/teste_random-lagos.csv", sep = ";")
myd_random <- myd_random %>% as_tibble()
myd_random
names(myd_random)


# analyze 

myd_random1 <- myd_random %>% 
  select(habitat_area, matrix_area, perceptual.range, matrix.permeability, 
         total_crossings, assess.top.sections) %>%
  mutate(prop.habitat = (habitat_area / (habitat_area + matrix_area))) %>%
  filter(!is.na(assess.top.sections),
         assess.top.sections != 1,
         assess.top.sections != 0)

myd_random1
names(myd_random1)
range(myd_random1$prop.habitat)
hist(myd_random1$prop.habitat)

ggplot(myd_random1, aes(prop.habitat, assess.top.sections, col=assess.top.sections)) +
  annotate("rect", xmin=0, xmax=1, ymin=0.5, ymax=1, alpha=0.2, fill="red") +
  annotate("rect", xmin=0, xmax=1, ymin=0.75, ymax=1, alpha=0.2, fill="red") +
  geom_point(size=.1) +
  geom_smooth(linewidth=.5, alpha=.2) +
  scale_x_continuous(breaks = c(.2, .8)) +
  facet_grid(perceptual.range ~ matrix.permeability) +
  scale_color_viridis("") +
  labs(title = "permeabilidade da matriz", x="Proportion of habitat", y="Hotspot likelihood") +
  ylim(0,1) +
  theme_bw() +
  theme(legend.position="none",
        text=element_text(size = 12))
  


# setwd("~/Dropbox/IBMs/IBibsM/Images")
# ggsave("Random_lands.tiff", width=18, height=14, units="cm", dpi=150, device = grDevices::tiff,)


#### 
library(betareg)
prop_perm_int <- betareg(assess.top.sections ~ prop.habitat*matrix.permeability, data = myd_random1)
summary(prop_perm_int)
range(myd_random1$assess.top.sections)

prop <- betareg(assess.top.sections ~ prop.habitat, data = myd_random1)
summary(prop)

perm <- betareg(assess.top.sections ~ matrix.permeability, data = myd_random1)
summary(perm)

#Extraia os coeficientes e seus nomes do modelo
coef_data <- as.data.frame(summary(prop_perm_int)$coefficients)

# Crie um novo dataframe para os coeficientes e seus nomes
coef_data <- data.frame(coef_name = row.names(coef_data),
                        coef_value = coef_data[, 1])

# Crie uma coluna para identificar as variáveis preditoras
coef_data$predictor <- gsub(":.*", "", coef_data$coef_name)

# Crie uma coluna para identificar se é um coeficiente principal ou de interação
coef_data$coef_type <- ifelse(grepl(":", coef_data$coef_name), "Interaction", "Main Effect")

# Plote os coeficientes
ggplot(coef_data, aes(x = predictor, y = coef_value, fill = coef_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variável Preditora", y = "Coeficiente", fill = "Tipo de Coeficiente") +
  theme_minimal() +
  theme(legend.position = "top")

# Crie um dataframe com novos dados para gerar os valores preditos
new_data <- data.frame(prop.habitat = seq(min(myd_random1$prop.habitat), max(myd_random1$prop.habitat), length.out = 100),
                       matrix.permeability = seq(min(myd_random1$matrix.permeability), max(myd_random1$matrix.permeability), length.out = 100))

# Obtenha os valores preditos usando a função predict()
new_data$predicted <- predict(prop_perm_int, newdata = new_data, type = "response")

# Plote os efeitos das variáveis preditoras com a função plot_model()
library(sjPlot)
plot_model(prop_perm_int, type = "pred", terms = c("prop.habitat", "matrix.permeability"), show.ci = FALSE)
