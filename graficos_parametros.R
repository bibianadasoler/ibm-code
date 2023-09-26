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
library(here)

# Submodel habitat amount ----
habitat_amount <- readRDS(here("results", "habitat_amount_dezmil.RDS"))
habitat_amount_simulations <- habitat_amount@simdesign@simoutput
#### Assess top sections ----

##### Proportion of habitat ----
# linear
(assess_prop_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = proportion_of_habitat, y = assess_top_sections)) +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
  geom_point(size = 0.05) +
  geom_smooth(method = "lm", col = "red", linewidth = 0.5) +
  ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90), expand = c(0,0.5)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
  labs(y = "Crossings aggregation", x = "Proportion of habitat", tag = "A") +  
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Matrix permeability ----
# exponential 
a <- 0.9241
b <- -0.1120
x_values <- 10:max(habitat_amount_simulations$matrix_permeability)
y_values <- a + b * log(x_values)
(assess_perm_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = matrix_permeability, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), col = "red", linewidth = 0.5) +
   ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Crossings aggregation", x = "Matrix permeability", tag = "B") +  
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Perceptual range ----
# exponential
a <- 0.39359
b <- 0.03562
x_values <- 5:max(habitat_amount_simulations$perceptual_range)
y_values <- a + b * log(x_values)
(assess_percep_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = perceptual_range, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), col = "red", linewidth = 0.5) +
   ylim(0,1) + scale_x_continuous(breaks = c(5, 15, 25, 35, 42), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Crossings aggregation", x = "Perceptual range", tag = "C") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Vision angle ----
# linear 
(assess_vision_habitat_amount <-ggplot(data = habitat_amount_simulations, aes(x = vision_angle, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(size = 0.05) +
   geom_smooth(method = "lm", col = "red", linewidth = 0.5) +
   ylim(0,1) + scale_x_continuous(breaks = c(90, 120, 150, 180), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Crossings aggregation", x = "Vision angle", tag = "D") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)
#### Total crossings ----
##### Proportion of habitat ----
# linear
(crossings_prop_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = proportion_of_habitat, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = "lm", col = "red", linewidth = 0.5) +
   labs(y = "Total crossings", x = "Proportion of habitat", tag = "A") +
   scale_y_continuous(breaks =  c(0, 500, 1000, 1500), limits = c(0, 1500)) +
   scale_x_continuous(breaks = c(10,25,50,75,90), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Matrix permeability ----
# exponencial
a <- 356.54
b <- 84.09
x_values <- 10:max(habitat_amount_simulations$matrix_permeability)
y_values <- a + b * log(x_values)
(crossings_perm_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = matrix_permeability, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), col = "red", linewidth = 0.5) +
   labs(y = "Total crossings", x = "Matrix permeability", tag = "B") +
   scale_y_continuous(breaks =  c(0, 500, 1000, 1500), limits = c(0, 1500)) +
   scale_x_continuous(breaks = c(10,30,50,70,90), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Perceptual range ----
# linear
(crossings_percep_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = perceptual_range, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = "lm", col = "red", linewidth = 0.5) +
   labs(y = "Total crossings", x = "Perceptual range", tag = "C") +
   scale_y_continuous(breaks =  c(0, 500, 1000, 1500), limits = c(0, 1500)) +
   scale_x_continuous(breaks = c(5, 15, 25, 35, 42), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)
##### Vision angle ----
# exponential
a <- 347.61
b <- 66.79
x_values <- 90:max(habitat_amount_simulations$vision_angle)
y_values <- a + b * log(x_values)
(crossings_vision_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = vision_angle, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), col = "red", linewidth = 0.5) +
   labs(y = "Total crossings", x = "Vision angle", tag = "D") +
    scale_y_continuous(breaks =  c(0, 500, 1000, 1500), limits = c(0, 1500)) +
   scale_x_continuous(breaks = c(90, 120, 150, 180), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### todos juntos ----
# grid.arrange(assess_prop_habitat_amount, assess_perm_habitat_amount, assess_percep_habitat_amount, assess_vision_habitat_amount,
#              crossings_prop_habitat_amount, crossings_perm_habitat_amount, crossings_percep_habitat_amount, crossings_vision_habitat_amount,
#              ncol = 4)


# Submodel configuration ----
configuration_simulations <- readRDS(here("results", "configuration_simulations_dezmil.RDS"))
configuration_simulations <- configuration_simulations@simdesign@simoutput
#### Assess top sections ----
##### Scenarios ----
(assess_scenarios_config <- ggplot(configuration_simulations, aes(x = factor(scenario), y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect",  xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect",  xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   stat_halfeye(adjust = 0.5, width = 0.75, justification = -0.25, .width = 0, point_colour = NA, fill = "grey35") +
   geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5) +
   labs(y = "Crossings aggregation", x = "Scenarios", tag = "E") +
   scale_x_discrete(labels=c("1" = "A", "2" = "B", "3" = "C", "4" = "D",
                             "5" = "E", "6" = "F",  "7" = "G"), expand = c(0,0.2)) +
   ylim(0,1)  + 
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)
##### Matrix permeability ----
# exponential 
a <- 0.8930
b <- -0.1052
x_values <- 10:max(configuration_simulations$matrix_permeability)
y_values <- a + b * log(x_values)
(assess_perm_config <- ggplot(configuration_simulations, aes(x = matrix_permeability, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), col = "red", linewidth = 0.5) +
   labs(y = "Crossings aggregation", x = "Matrix permeability", tag = "F") +
   ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Perceptual range ----
# linear
(assess_percep_config <- ggplot(data = configuration_simulations, aes(x = perceptual_range, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(size = 0.05) +
   geom_smooth(method = "lm", col = "red", linewidth = 0.5) +
   labs(y = "Crossings aggregation", x = "Perceptual range", tag = "G") +
   ylim(0,1) + scale_x_continuous(breaks = c(5, 15, 25, 35, 42), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Vision angle ----
# linear
(assess_vision_config <- ggplot(data = configuration_simulations, aes(x = vision_angle, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "red") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "red") +
   geom_point(size = 0.05) +
   geom_smooth(method = "lm", col = "red", linewidth = 0.5) +
   labs(y = "Crossings aggregation", x = "Vision angle", tag = "H") +
   ylim(0,1) + scale_x_continuous(breaks = c(90, 120, 150, 180), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

#### Total crossings ----
##### Scenarios ----
(crossings_scenarios_config <- ggplot(configuration_simulations, aes(x = factor(scenario), y = total_crossings)) +
   stat_halfeye(adjust = 0.5, width = 0.75, justification = -0.25, .width = 0, point_colour = NA, fill = "grey35") +
   geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5) +
   labs(y = "Total crossings", x = "Scenario", tag = "E") +
   scale_x_discrete(labels=c("1" = "A", "2" = "B", "3" = "C", "4" = "D",
                             "5" = "E", "6" = "F",  "7" = "G"), expand = c(0,0.2)) +
   scale_y_continuous(breaks =  c(0, 600, 1200, 1800, 2400, 3000, 3600), limits = c(0,3600)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
 ) 

##### Matrix permeability ----
# exponential
a <- 1171.28 
b <- -87.09 
x_values <- 10:max(configuration_simulations$matrix_permeability)
y_values <- a + b * log(x_values)
(crossings_perm_config <- ggplot(data = configuration_simulations, aes(x = matrix_permeability, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), col = "red", linewidth = 0.5) +
   labs(y = "Total crossings", x = "Matrix permeability", tag = "F") +
   scale_y_continuous(breaks =  c(0, 600, 1200, 1800, 2400, 3000, 3600), limits = c(0,3600)) +
   scale_x_continuous(breaks = c(10, 30, 50, 70, 90), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Perceptual range ----
# linear
(crossings_percep_config <- ggplot(data = configuration_simulations, aes(x = perceptual_range, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_smooth(method = "lm", col = "red", linewidth = 0.5) +
   labs(y = "Total crossings", x = "Perceptual range", tag = "G") +
   scale_y_continuous(breaks =  c(0, 600, 1200, 1800, 2400, 3000, 3600), limits = c(0,3600)) +
   scale_x_continuous(breaks = c(5, 15, 25, 35, 42), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### Vision angle ----
# exponential
a <- -644.3
b <- 304.3
x_values <- 90:max(configuration_simulations$vision_angle)
y_values <- a + b * log(x_values)
(crossings_vision_config <- ggplot(data = configuration_simulations, aes(x = vision_angle, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), col = "red", linewidth = 0.5) +
   labs(y = "Total crossings", x = "Vision angle", tag = "H") +
   scale_y_continuous(breaks =  c(0, 600, 1200, 1800, 2400, 3000, 3600), limits = c(0, 3600)) +
   scale_x_continuous(breaks = c(90, 120, 150, 180), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.02,1))
)

##### todos juntos ----
# grid.arrange(assess_scenarios_config, assess_perm_config, assess_percep_config, assess_vision_config,
#              crossings_scenarios_config, crossings_perm_config, crossings_percep_config, crossings_vision_config,
#              ncol = 4)

### CROSSINGS AGGREGATION ----
grid.arrange(assess_prop_habitat_amount, assess_perm_habitat_amount, assess_percep_habitat_amount, assess_vision_habitat_amount,
             assess_scenarios_config, assess_perm_config, assess_percep_config, assess_vision_config,
             ncol = 4)
#1234 x 851

### TOTAL CROSSINGS ----
grid.arrange(crossings_prop_habitat_amount, crossings_perm_habitat_amount, crossings_percep_habitat_amount, crossings_vision_habitat_amount,
             crossings_scenarios_config, crossings_perm_config, crossings_percep_config, crossings_vision_config,
             ncol = 4)
