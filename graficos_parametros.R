#
#
# graficos para cada parametro 
#
#
library(meantables)
library(dplyr)
library(ggplot2)
library(ggdist)
library(gridExtra)

### Submodel habitat amount ----

#### Assess top sections ----
mean_assess_prop_habitat <- habitat_amount_simulations %>%
  group_by(proportion_of_habitat) %>%
  mean_table(assess_top_sections) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(proportion_of_habitat = group_cat)

mean_assess_perm_habitat <- habitat_amount_simulations %>%
  group_by(matrix_permeability) %>%
  mean_table(assess_top_sections) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(matrix_permeability = group_cat)

mean_assess_percep_habitat <- habitat_amount_simulations %>%
  group_by(perceptual_range) %>%
  mean_table(assess_top_sections) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(perceptual_range = group_cat)

mean_assess_vision_habitat <- habitat_amount_simulations %>%
  group_by(vision_angle) %>%
  mean_table(assess_top_sections) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(vision_angle = group_cat)

##### Proportion of habitat ----
(assess_prop_habitat_amount <- ggplot(mean_assess_prop_habitat, aes(x = proportion_of_habitat, y = mean)) +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
  geom_point(data = habitat_amount_simulations, aes(x = proportion_of_habitat, y = assess_top_sections), size = 0.05) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
  geom_line(color = "grey5", linewidth = 0.5) +
  ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
  labs(y = "Proportion of crossing top sections", x = "Proportion of habitat") +  
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"))
)

##### Matrix permeability ----
(assess_perm_habitat_amount <- ggplot(mean_assess_perm_habitat, aes(x = matrix_permeability, y = mean)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(data = habitat_amount_simulations, aes(x = matrix_permeability, y = assess_top_sections), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "grey5", linewidth = 0.5) +
   ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Proportion of crossing top sections", x = "Matrix permeability") +  
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"))
)

##### Perceptual range ----
(assess_percep_habitat_amount <- ggplot(mean_assess_percep_habitat, aes(x = perceptual_range, y = mean)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(data = habitat_amount_simulations, aes(x = perceptual_range, y = assess_top_sections), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "grey5", linewidth = 0.5) +
   ylim(0,1) + scale_x_continuous(breaks = c(5, 15, 25, 35, 42)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Proportion of crossing top sections", x = "Perceptual range") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"))
)

##### Vision angle ----
(assess_vision_habitat_amount <-ggplot(mean_assess_vision_habitat, aes(x = vision_angle, y = mean)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(data = habitat_amount_simulations, aes(x = vision_angle, y = assess_top_sections), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "grey5", linewidth = 0.5) +
   ylim(0,1) + scale_x_continuous(breaks = c(90, 120, 150, 180)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Proportion of crossing top sections", x = "Vision angle") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"))
)
#### Total crossings ----
mean_total_prop_habitat <- habitat_amount_simulations %>%
  group_by(proportion_of_habitat) %>%
  mean_table(total_crossings) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(proportion_of_habitat = group_cat)

mean_total_perm_habitat <- habitat_amount_simulations %>%
  group_by(matrix_permeability) %>%
  mean_table(total_crossings) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(matrix_permeability = group_cat)

mean_total_percep_habitat <- habitat_amount_simulations %>%
  group_by(perceptual_range) %>%
  mean_table(total_crossings) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(perceptual_range = group_cat)

mean_total_vision_habitat <- habitat_amount_simulations %>%
  group_by(vision_angle) %>%
  mean_table(total_crossings) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(vision_angle = group_cat)


##### Proportion of habitat ----
(crossings_prop_habitat_amount <- ggplot(mean_total_prop_habitat, aes(x = proportion_of_habitat, y = mean)) +
   geom_point(data = habitat_amount_simulations, aes(x = proportion_of_habitat, y = total_crossings), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "blue", linewidth = 0.5) +
   labs(y = "Total crossings road", x = "Proportion of habitat") +
   scale_y_continuous(breaks =  c(0, 250, 500, 750, 1000, 1250, 1500, 1750), limits = c(0,1750)) +
   scale_x_continuous(breaks = c(10,25,50,75,90)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"))
)

##### Matrix permeability ----
(crossings_perm_habitat_amount <- ggplot(mean_total_perm_habitat, aes(x = matrix_permeability, y = mean)) +
   geom_point(data = habitat_amount_simulations, aes(x = matrix_permeability, y = total_crossings), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "blue", linewidth = 0.5) +
   labs(y = "Total crossings road", x = "Matrix permeability") +
   scale_y_continuous(breaks =  c(0, 250, 500, 750, 1000, 1250, 1500, 1750), limits = c(0,1750)) +
   scale_x_continuous(breaks = c(10,30,50,70,90)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"))
)

##### Perceptual range ----
(crossings_percep_habitat_amount <- ggplot(mean_total_percep_habitat, aes(x = perceptual_range, y = mean)) +
   geom_point(data = habitat_amount_simulations, aes(x = perceptual_range, y = total_crossings), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "blue", linewidth = 0.5) +
   labs(y = "Total crossings road", x = "Perceptual range") +
   scale_y_continuous(breaks =  c(0, 250, 500, 750, 1000, 1250, 1500, 1750), limits = c(0,1750)) +
   scale_x_continuous(breaks = c(5, 15, 25, 35, 42)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"))
)
##### Vision angle ----
(crossings_vision_habitat_amount <- ggplot(mean_total_vision_habitat, aes(x = vision_angle, y = mean)) +
   geom_point(data = habitat_amount_simulations, aes(x = vision_angle, y = total_crossings), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "blue", linewidth = 0.5) +
   labs(y = "Total crossings road", x = "Vision angle") +
   scale_y_continuous(breaks =  c(0, 250, 500, 750, 1000, 1250, 1500, 1750), limits = c(0,1750)) +
   scale_x_continuous(breaks = c(90, 120, 150, 180)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"))
)

##### todos juntos ----
grid.arrange(assess_prop_habitat_amount, assess_perm_habitat_amount, assess_percep_habitat_amount, assess_vision_habitat_amount,
             crossings_prop_habitat_amount, crossings_perm_habitat_amount, crossings_percep_habitat_amount, crossings_vision_habitat_amount,
             ncol = 4)


### Submodel configuration ----
#### Assess top sections ----
mean_assess_perm_config <- configuration_simulations %>%
  group_by(matrix_permeability) %>%
  mean_table(assess_top_sections) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(matrix_permeability = group_cat)

mean_assess_percep_config <- configuration_simulations %>%
  group_by(perceptual_range) %>%
  mean_table(assess_top_sections) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(perceptual_range = group_cat)

mean_assess_vision_config <- configuration_simulations %>%
  group_by(vision_angle) %>%
  mean_table(assess_top_sections) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(vision_angle = group_cat)
##### Scenarios ----
(assess_scenarios_config <- ggplot(configuration_simulations, aes(x = factor(scenario), y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect",  xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect",  xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   stat_halfeye(adjust = 0.5, width = 0.75, justification = -0.25, .width = 0, point_colour = NA, fill = "grey35") +
   geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5) +
   labs(y = "Proportion of crossing top sections", x = "Scenario") +
   scale_x_discrete(labels=c("1" = "A", "2" = "B", "3" = "C", "4" = "D",
                             "5" = "E", "6" = "F",  "7" = "G")) +
   ylim(0,1)  + 
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"))
)
##### Matrix permeability ----
(assess_perm_config <- ggplot(mean_assess_perm_config, aes(x = matrix_permeability, y = mean)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(data = configuration_simulations, aes(x = matrix_permeability, y = assess_top_sections), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "grey5", linewidth = 0.5) +
   labs(y = "Proportion of crossing top sections", x = "Matrix permeability") +
   ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"))
)
##### Perceptual range ----
(assess_percep_config <- ggplot(mean_assess_percep_config, aes(x = perceptual_range, y = mean)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(data = configuration_simulations, aes(x = perceptual_range, y = assess_top_sections), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "grey5", linewidth = 0.5) +
   labs(y = "Proportion of crossing top sections", x = "Perceptual range") +
   ylim(0,1) + scale_x_continuous(breaks = c(5, 15, 25, 35, 42)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"))
)

##### Vision angle ----
(assess_vision_config <- ggplot(mean_assess_vision_config, aes(x = vision_angle, y = mean)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(data = configuration_simulations, aes(x = vision_angle, y = assess_top_sections), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "grey5", linewidth = 0.5) +
   labs(y = "Proportion of crossing top sections", x = "Vision angle") +
   ylim(0,1) + scale_x_continuous(breaks = c(90, 120, 150, 180)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"))
)

#### Total crossings ----
mean_total_perm_config <- configuration_simulations %>%
  group_by(matrix_permeability) %>%
  mean_table(total_crossings) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(matrix_permeability = group_cat)

mean_total_percep_config <- configuration_simulations %>%
  group_by(perceptual_range) %>%
  mean_table(total_crossings) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(perceptual_range = group_cat)

mean_total_vision_config <- configuration_simulations %>%
  group_by(vision_angle) %>%
  mean_table(total_crossings) %>%
  select(mean, lcl, ucl, group_cat) %>%
  rename(vision_angle = group_cat)

##### Scenarios ----
(crossings_scenarios_config <- ggplot(configuration_simulations, aes(x = factor(scenario), y = total_crossings)) +
   stat_halfeye(adjust = 0.5, width = 0.75, justification = -0.25, .width = 0, point_colour = NA, fill = "grey35") +
   geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5) +
   labs(y = "Total crossings road", x = "Scenario") +
   scale_x_discrete(labels=c("1" = "A", "2" = "B", "3" = "C", "4" = "D",
                             "5" = "E", "6" = "F",  "7" = "G")) +
   scale_y_continuous(breaks =  c(0, 600, 1200, 1800, 2400, 3000, 3600), limits = c(0,3600)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"))
 ) 

##### Matrix permeability ----
(crossings_perm_config <- ggplot(mean_total_perm_config, aes(x = matrix_permeability, y = mean)) +
   geom_point(data = configuration_simulations, aes(x = matrix_permeability, y = total_crossings), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "blue", linewidth = 0.5) +
   labs(y = "Total crossings road", x = "Matrix permeability") +
   scale_y_continuous(breaks =  c(0, 600, 1200, 1800, 2400, 3000, 3600), limits = c(0,3600)) +
   scale_x_continuous(breaks = c(10, 30, 50, 70, 90)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"))
)

##### Perceptual range ----
(crossings_percep_config <- ggplot(mean_total_percep_config, aes(x = perceptual_range, y = mean)) +
   geom_point(data = configuration_simulations, aes(x = perceptual_range, y = total_crossings), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "blue", linewidth = 0.5) +
   labs(y = "Total crossings road", x = "Perceptual range") +
   scale_y_continuous(breaks =  c(0, 600, 1200, 1800, 2400, 3000, 3600), limits = c(0,3600)) +
   scale_x_continuous(breaks = c(5, 15, 25, 35, 42)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"))
)

##### Vision angle ----
(crossings_vision_config <- ggplot(mean_total_vision_config, aes(x = vision_angle, y = mean)) +
   geom_point(data = configuration_simulations, aes(x = vision_angle, y = total_crossings), size = 0.05) +
   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.8, fill = "grey100", linewidth = 0) +
   geom_line(color = "blue", linewidth = 0.5) +
   labs(y = "Total crossings road", x = "Vision angle") +
   scale_y_continuous(breaks =  c(0, 600, 1200, 1800, 2400, 3000, 3600), limits = c(0,3600)) +
   scale_x_continuous(breaks = c(90, 120, 150, 180)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"))
)

##### todos juntos ----
grid.arrange(assess_scenarios_config, assess_perm_config, assess_percep_config, assess_vision_config,
             crossings_scenarios_config, crossings_perm_config, crossings_percep_config, crossings_vision_config,
             ncol = 4)

##### HABITAT AND CONFIGURATION----
grid.arrange(assess_scenarios_config, assess_perm_config, assess_percep_config, assess_vision_config,
             assess_prop_habitat_amount, assess_perm_habitat_amount, assess_percep_habitat_amount, assess_vision_habitat_amount,
             ncol = 4)
