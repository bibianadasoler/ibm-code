# graficos results gerados com sobol
library(dplyr)
library(stringr)
library(ggplot2)
library(viridis)
library(gridExtra)

dados_sobol_netlogo <- read.csv("./results/SobolSteps500.csv", header = T) %>%
  rename(., run_id = run.id) %>%
  rename(., proportion.of.habitat = proportion_habitat_random) %>%
  select(-type_scenario)
dados_sobol_nlrx <- read.csv("./results/sobol_random500-raw_sobol.csv") %>%
  rename(., N_individuals = number.of.individuals) %>%
  rename(., habitat_area = count.patches.with..habitat...1.) %>%
  rename(., matrix_area = count.patches.with..habitat...2.) %>%
  rename(., mean_hab_neighbors = mean..hab_neighbors..of.patches.with..habitat...0.) %>%
  rename(., total_crossings = sum..visits..of.patches.with..habitat...0.) %>%
  rename(., landscape_area = X.count.patches.) %>%
  select(-X.run.number., -save.data., -scenario)

dados_patches <- read.csv("./results/id_pacthes.csv",  header = T)  
dados_patches <- dados_patches %>%
  mutate(run_id = str_split_i(dados_patches$run_id, ".asc", 1)) %>%
  left_join(dados_patches)

dados_sobol_patches <- merge(dados_sobol_netlogo, dados_sobol_nlrx) %>%
  mutate(prop.habitat = (habitat_area / (landscape_area) * 100)) %>%
  select(run_id, N_individuals, steps, X.step., siminputrow, # gerais
         proportion.of.habitat, prop.habitat, matrix.permeability, perceptual.range, vision.angle, landscape_area, habitat_area, matrix_area, #inputs
         assess.top.sections, total_crossings, mean_hab_neighbors) %>%#outputs  
  merge(., dados_patches)

plot(dados_sobol_patches$proportion.of.habitat, dados_sobol_patches$habitat_patches)
range(dados_sobol_patches$proportion.of.habitat)
range(dados_sobol_patches$prop.habitat)
hist(dados_sobol_patches$proportion.of.habitat, ylim = c(0,600))
hist(dados_sobol_patches$prop.habitat)


mean_prop <- dados_sobol_patches %>% group_by(proportion.of.habitat) %>% 
  summarise(mean_top = mean(assess.top.sections),
            mean_cross = mean(total_crossings))
(mean_prop_top_graph <- ggplot(mean_prop, aes(proportion.of.habitat, mean_top)) +
  annotate("rect", xmin = 10, xmax = 90, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 10, xmax = 90, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 1.5) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
  ylim(0,1) +
  labs(y = "mean assess top sections") +
  theme_bw() +
  theme(legend.position="none",
        text=element_text(size = 12)))
(mean_prop_cross_graph <- ggplot(mean_prop, aes(proportion.of.habitat, mean_cross)) +
    geom_point(size = 1.5) +
    geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
    ylim(500, 800) +
    labs(y = "mean total crossings") +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size = 12)))

mean_perm <- dados_sobol_patches %>% group_by(matrix.permeability) %>% 
  summarise(mean_top = mean(assess.top.sections),
            mean_cross = mean(total_crossings))
(mean_perm_top_graph <- ggplot(mean_perm, aes(matrix.permeability, mean_top)) +
    annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
    annotate("rect", xmin = 0.1, xmax = 0.9, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
    geom_point(size = 1.5) +
    geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
    ylim(0,1) +
    labs(y = "mean assess top sections") +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size = 12)))
(mean_perm_cross_graph <- ggplot(mean_perm, aes(matrix.permeability, mean_cross)) +
    geom_point(size = 1.5) +
    geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
    ylim(500, 800) +
    labs(y = "mean total crossings") +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size = 12)))

mean_percep <- dados_sobol_patches %>% group_by(perceptual.range) %>% 
  summarise(mean_top = mean(assess.top.sections),
            mean_cross = mean(total_crossings))
(mean_percep_top_graph <- ggplot(mean_percep, aes(perceptual.range, mean_top)) +
    annotate("rect", xmin = 5, xmax = 20, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
    annotate("rect", xmin = 5, xmax = 20, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
    geom_point(size = 1.5) +
    geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
    ylim(0,1) +
    labs(y = "mean assess top sections") +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size = 12)))
(mean_percep_cross_graph <- ggplot(mean_percep, aes(perceptual.range, mean_cross)) +
    geom_point(size = 1.5) +
    geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
    ylim(500, 800) +
    labs(y = "mean total crossings") +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size = 12)))

mean_vision <- dados_sobol_patches %>% group_by(vision.angle) %>% 
  summarise(mean_top = mean(assess.top.sections),
            mean_cross = mean(total_crossings))
(mean_vision_top_graph <- ggplot(mean_vision, aes(vision.angle, mean_top)) +
    annotate("rect", xmin = 90, xmax = 180, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
    annotate("rect", xmin = 90, xmax = 180, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
    geom_point(size = 1.5) +
    geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
    ylim(0,1) +
    labs(y = "mean assess top sections") +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size = 12)))
(mean_vision_cross_graph <- ggplot(mean_vision, aes(vision.angle, mean_cross)) +
    geom_point(size = 1.5) +
    geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
    ylim(500, 800) +
    labs(y = "mean total crossings") +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size = 12)))



grid.arrange(mean_prop_top_graph, mean_perm_top_graph, mean_percep_top_graph, mean_vision_top_graph,
             mean_prop_cross_graph, mean_perm_cross_graph, mean_percep_cross_graph, mean_vision_cross_graph,
             ncol = 4)

ggplot(dados_sobol_patches, aes(habitat_patches, assess.top.sections)) +
  annotate("rect", xmin = 0, xmax = 25, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 0, xmax = 25, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 1.5) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
  ylim(0,1) +
  labs(y = "assess top sections") +
  theme_bw() +
  theme(legend.position="none",
        text=element_text(size = 12))


ggplot(dados_sobol_patches, aes(mean_hab_neighbors, assess.top.sections)) +
  annotate("rect", xmin = 0, xmax = 6, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 0, xmax = 6, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 1.5) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.2) +
  ylim(0,1) +
  labs(y = "assess top sections") +
  theme_bw() +
  theme(legend.position="none",
        text=element_text(size = 12))



