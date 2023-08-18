#
#
# graficos para cada parametro 
#
#
library(ggplot2)
library(gridExtra)
library(dplyr)

### Submodel habitat amount ----
habitat_amount_simulations <- read.csv(here("results", "habitat_amount_simulation2.csv"))
mean_prop_habitat <- habitat_amount_simulations %>%
  group_by(proportion_of_habitat) %>%
  summarise(mean_assess = mean(assess_top_sections),
            mean_cross = mean(total_crossings))

mean_matrix_habitat <- habitat_amount_simulations %>%
  group_by(matrix_permeability) %>%
  summarise(mean_assess = mean(assess_top_sections),
            mean_cross = mean(total_crossings))

mean_percep_habitat <- habitat_amount_simulations %>%
  group_by(perceptual_range) %>%
  summarise(mean_assess = mean(assess_top_sections),
            mean_cross = mean(total_crossings))

mean_vision_habitat <- habitat_amount_simulations %>%
  group_by(vision_angle) %>%
  summarise(mean_assess = mean(assess_top_sections),
            mean_cross = mean(total_crossings))
#### Assess top sections ----
##### Proportion of habitat ----
(assess_prop_habitat_amount <- ggplot(mean_prop_habitat, aes(x = proportion_of_habitat, y = mean_assess)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   annotate("rect", xmin = 10, xmax = 90, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
   annotate("rect", xmin = 10, xmax = 90, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
   ylim(0,1) +
   labs(y = "Proportion of crossing top sections", x = "Proportion of habitat") +  
   theme_bw())

##### Matrix permeability ----
(assess_perm_habitat_amount <-ggplot(mean_matrix_habitat, aes(x = matrix_permeability, y = mean_assess)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   annotate("rect", xmin = 10, xmax = 90, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
   annotate("rect", xmin = 10, xmax = 90, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
   ylim(0,1) +
   labs(y = "Proportion of crossing top sections", x = "Matrix permeability") +  
   theme_bw() )

##### Perceptual range ----
(assess_percep_habitat_amount <-ggplot(mean_percep_habitat, aes(x = perceptual_range, y = mean_assess)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   annotate("rect", xmin = 5, xmax = 42, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
   annotate("rect", xmin = 5, xmax = 42, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
   ylim(0,1) +
   labs(y = "Proportion of crossing top sections", x = "Perceptual range") +
   theme_bw() )

##### Vision angle ----
(assess_vision_habitat_amount <-ggplot(mean_vision_habitat, aes(x = vision_angle, y = mean_assess)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   annotate("rect", xmin = 90, xmax = 180, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
   annotate("rect", xmin = 90, xmax = 180, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
   ylim(0,1) +
   labs(y = "Proportion of crossing top sections", x = "Vision angle") + 
   theme_bw() )

#### Total crossings ----
##### Proportion of habitat ----
(crossings_prop_habitat_amount <- ggplot(mean_prop_habitat, aes(x = proportion_of_habitat, y = mean_cross)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(250,1000) +
   labs(y = "Total crossings road", x = "Proportion of habitat") +
   theme_bw())

##### Matrix permeability ----
(crossings_perm_habitat_amount <- ggplot(mean_matrix_habitat, aes(x = matrix_permeability, y = mean_cross)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(250,1000) +
   labs(y = "Total crossings road", x = "Matrix permeability") +
   theme_bw())

##### Perceptual range ----
(crossings_percep_habitat_amount <- ggplot(mean_percep_habitat, aes(x = perceptual_range, y = mean_cross)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(250,1000) +
   labs(y = "Total crossings road", x = "Perceptual range") +
   theme_bw())

##### Vision angle ----
(crossings_vision_habitat_amount <- ggplot(mean_vision_habitat, aes(x = vision_angle, y = mean_cross)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(250,1000) +
   labs(y = "Total crossings road", x = "Vision angle") +
   theme_bw())

##### todos juntos ----
grid.arrange(assess_prop_habitat_amount, assess_perm_habitat_amount, assess_percep_habitat_amount, assess_vision_habitat_amount,
             crossings_prop_habitat_amount, crossings_perm_habitat_amount, crossings_percep_habitat_amount, crossings_vision_habitat_amount,
             ncol = 4)



### Submodel configuration ----
mean_scenario_config <- configuration_simulations %>%
  group_by(type_scenario) %>%
  summarise(mean_assess = mean(assess_top_sections),
            mean_cross = mean(total_crossings))

mean_matrix_config <- configuration_simulations %>%
  group_by(matrix_permeability) %>%
  summarise(mean_assess = mean(assess_top_sections),
            mean_cross = mean(total_crossings))

mean_percep_config <- configuration_simulations %>%
  group_by(perceptual_range) %>%
  summarise(mean_assess = mean(assess_top_sections),
            mean_cross = mean(total_crossings))

mean_vision_config <- configuration_simulations %>%
  group_by(vision_angle) %>%
  summarise(mean_assess = mean(assess_top_sections),
            mean_cross = mean(total_crossings))
#### Assess top sections ----
##### Scenarios ----
(assess_scenarios_config <- ggplot(mean_scenario_config, aes(x = type_scenario, y = mean_assess)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   annotate("rect", xmin = 1, xmax = 7, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
   annotate("rect", xmin = 1, xmax = 7, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
   ylim(0,1) +
   labs(y = "Proportion of crossing top sections", x = "Scenario") +
   theme_bw())

##### Matrix permeability ----
(assess_perm_config <- ggplot(mean_matrix_config, aes(x = matrix_permeability, y = mean_assess)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   annotate("rect", xmin = 10, xmax = 90, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
   annotate("rect", xmin = 10, xmax = 90, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
   ylim(0,1) +
   labs(y = "Proportion of crossing top sections", x = "Matrix permeability") +
   theme_bw() )

##### Perceptual range ----
(assess_percep_config <- ggplot(mean_percep_config, aes(x = perceptual_range, y = mean_assess)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   annotate("rect", xmin = 5, xmax = 42, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
   annotate("rect", xmin = 5, xmax = 42, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
   ylim(0,1) +
   labs(y = "Proportion of crossing top sections", x = "Perceptual range") +
   theme_bw() )

##### Vision angle ----
(assess_vision_config <- ggplot(mean_vision_config, aes(x = vision_angle, y = mean_assess)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   annotate("rect", xmin = 90, xmax = 180, ymin = 0.5, ymax = 1, alpha = 0.2, fill = "red") +
   annotate("rect", xmin = 90, xmax = 180, ymin = 0.75, ymax = 1, alpha = 0.2, fill = "red") +
   ylim(0,1) +
   labs(y = "Proportion of crossing top sections", x = "Vision angle") +
   theme_bw() )


#### Total crossings ----
##### Scenarios ----
(crossings_scenarios_config <- ggplot(mean_scenario_config, aes(x = type_scenario, y = mean_cross)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(0,1750) +
   labs(y = "Total crossings road", x = "Scenarios") +
   theme_bw())

##### Matrix permeability ----
(crossings_perm_config <- ggplot(mean_matrix_config, aes(x = matrix_permeability, y = mean_cross)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(400,1500) +
   labs(y = "Total crossings road", x = "Matrix permeability") +
   theme_bw())

##### Perceptual range ----
(crossings_percep_config <- ggplot(mean_percep_config, aes(x = perceptual_range, y = mean_cross)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(400,1500) +
   labs(y = "Total crossings road", x = "Perceptual range") +
   theme_bw())

##### Vision angle ----
(crossings_vision_config <- ggplot(mean_vision, aes(x = vision_angle, y = mean_cross)) +
   geom_point(size = 1) +
   geom_smooth(method = lm, linewidth = 0.8, alpha = 0.5) +
   ylim(400,1500) +
   labs(y = "Total crossings road", x = "Vision angle") + 
   theme_bw())

##### todos juntos ----
grid.arrange(assess_scenarios_config, assess_perm_config, assess_percep_config, assess_vision_config,
             crossings_scenarios_config, crossings_perm_config, crossings_percep_config, crossings_vision_config,
             ncol = 4)


##### HABITAT AND CONFIGURATION----
grid.arrange(assess_scenarios_config, assess_perm_config, assess_percep_config, assess_vision_config,
             assess_prop_habitat_amount, assess_perm_habitat_amount, assess_percep_habitat_amount, assess_vision_habitat_amount,
             ncol = 4)
