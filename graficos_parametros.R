#
#
# graficos para cada parametro 
#
#
library(ggplot2)
library(gridExtra)
### Submodel habitat amount ----
#### Assess top sections ----
##### Proportion of habitat ----
(assess_prop_habitat_amount <- ggplot(habitat_amount_simulations, aes(x = proportion_of_habitat, y = assess_top_sections)) +
  annotate("rect", xmin = 10, xmax = 90, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 10, xmax = 90, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(0,1) +
  labs(y = "Proportion of crossing top sections", x = "Proportion of habitat") +  
   theme_bw())
 
##### Matrix permeability ----
(assess_perm_habitat_amount <-ggplot(habitat_amount_simulations, aes(x = matrix_permeability, y = assess_top_sections)) +
  annotate("rect", xmin = 0.1, xmax = 0.90, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 0.10, xmax = 0.90, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(0,1) +
  labs(y = "Proportion of crossing top sections", x = "Matrix permeability") +  
  theme_bw() )

##### Perceptual range ----
(assess_percep_habitat_amount <-ggplot(habitat_amount_simulations, aes(x = perceptual_range, y = assess_top_sections)) +
  annotate("rect", xmin = 5, xmax = 42, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 5, xmax = 42, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(0,1) +
  labs(y = "Proportion of crossing top sections", x = "Perceptual range") +
  theme_bw() )

##### Vision angle ----
(assess_vision_habitat_amount <-ggplot(habitat_amount_simulations, aes(x = vision_angle, y = assess_top_sections)) +
  annotate("rect", xmin = 90, xmax = 180, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 90, xmax = 180, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(0,1) +
  labs(y = "Proportion of crossing top sections", x = "Vision angle") + 
  theme_bw() )

#### Total crossings ----
##### Proportion of habitat ----
(crossings_prop_habitat_amount <- ggplot(habitat_amount_simulations, aes(x = proportion_of_habitat, y = total_crossings)) +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(300,2200) +
  labs(y = "Total crossings road", x = "Proportion of habitat") +
  theme_bw())

##### Matrix permeability ----
(crossings_perm_habitat_amount <- ggplot(habitat_amount_simulations, aes(x = matrix_permeability, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(300,2200) +
   labs(y = "Total crossings road", x = "Matrix permeability") +
   theme_bw())

##### Perceptual range ----
(crossings_percep_habitat_amount <- ggplot(habitat_amount_simulations, aes(x = perceptual_range, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(300,2200) +
   labs(y = "Total crossings road", x = "Perceptual range") +
   theme_bw())

##### Vision angle ----
(crossings_vision_habitat_amount <- ggplot(habitat_amount_simulations, aes(x = vision_angle, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(300,2200) +
   labs(y = "Total crossings road", x = "Vision angle") +
   theme_bw())

##### todos juntos ----
grid.arrange(assess_prop_habitat_amount, assess_perm_habitat_amount, assess_percep_habitat_amount, assess_vision_habitat_amount,
             crossings_prop_habitat_amount, crossings_perm_habitat_amount, crossings_percep_habitat_amount, crossings_vision_habitat_amount,
             ncol = 4)

### Submodel configuration ----
#### Assess top sections ----
##### Scenarios ----
(assess_scenarios_config <- ggplot(configuration_simulations, aes(x = scenario, y = assess_top_sections)) +
  annotate("rect", xmin = 1, xmax = 7, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 1, xmax = 7, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(0,1) +
  labs(y = "Proportion of crossing top sections", x = "Scenario") +
  theme_bw())

##### Matrix permeability ----
(assess_perm_config <- ggplot(configuration_simulations, aes(x = matrix_permeability, y = assess_top_sections)) +
  annotate("rect", xmin = 10, xmax = 90, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 10, xmax = 90, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(0,1) +
  labs(y = "Proportion of crossing top sections", x = "Matrix permeability") +
  theme_bw() )

##### Perceptual range ----
ggplot(configuration_simulations, aes(x = perceptual_range, y = assess_top_sections)) +
  annotate("rect", xmin = 5, xmax = 42, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 5, xmax = 42, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(0,1) +
  labs(y = "Proportion of crossing top sections", x = "Perceptual range") +
  theme_bw() )

##### Vision angle ----
ggplot(configuration_simulations, aes(x = vision_angle, y = assess_top_sections)) +
  annotate("rect", xmin = 90, xmax = 180, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 90, xmax = 180, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
  ylim(0,1) +
  labs(y = "Proportion of crossing top sections", x = "Vision angle") +
  theme_bw() )


#### Total crossings ----
##### Proportion of habitat ----
(crossings_prop_config <- ggplot(configuration_simulations, aes(x = proportion_of_habitat, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(300,2200) +
   labs(y = "Total crossings road", x = "Proportion of habitat") +
   theme_bw())

##### Matrix permeability ----
(crossings_perm_config <- ggplot(configuration_simulations, aes(x = matrix_permeability, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(300,2200) +
   labs(y = "Total crossings road", x = "Matrix permeability") +
   theme_bw())

##### Perceptual range ----
(crossings_percep_config <- ggplot(configuration_simulations, aes(x = perceptual_range, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(300,2200) +
   labs(y = "Total crossings road", x = "Perceptual range") +
   theme_bw())

##### Vision angle ----
(crossings_vision_config <- ggplot(configuration_simulations, aes(x = vision_angle, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(300,2200) +
   labs(y = "Total crossings road", x = "Vision angle") + 
   theme_bw())

##### todos juntos ----
grid.arrange(assess_prop_config, assess_perm_config, assess_percep_config, assess_vision_configt,
             crossings_prop_config, crossings_perm_config, crossings_percep_config, crossings_vision_config,
             ncol = 4)