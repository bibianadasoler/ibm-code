#
#
# script to create the graphics panel for inputs individually
# for each parameter we plot only the best-ranked model 
#
library(here)
library(dplyr)
library(aomisc)
library(ggplot2)
library(ggdist)
library(patchwork)

# Submodel habitat amount ----
habitat_amount <- readRDS(here::here("results", "habitat_amount_simulations.rds"))
habitat_amount_simulations <- habitat_amount@simdesign@simoutput
#### Assess top sections ----

##### Proportion of habitat ----
# bragg4
bragg4 <- drm(habitat_amount_simulations$assess_top_sections ~ habitat_amount_simulations$proportion_of_habitat, fct = DRC.bragg.4())
x_values <- 10:max(habitat_amount_simulations$proportion_of_habitat)
y_values <- coefficients(bragg4)[2] + (coefficients(bragg4)[3] - coefficients(bragg4)[2]) * exp(- coefficients(bragg4)[1] * (x_values - coefficients(bragg4)[4])^2)
(assess_prop_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = proportion_of_habitat, y = assess_top_sections)) +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
  geom_point(size = 0.05) +
  geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
            col = "red", linewidth = 0.75) +
  ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90), expand = c(0,0.5)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
  labs(y = "Crossings aggregation", x = "Proportion of habitat", tag = "A") +  
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Matrix permeability ----
# asymptotic 
asymptotic <- drm(habitat_amount_simulations$assess_top_sections ~ habitat_amount_simulations$matrix_permeability, fct = DRC.asymReg())
x_values <- 10:max(habitat_amount_simulations$matrix_permeability)
y_values <- coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values)
(assess_perm_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = matrix_permeability, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Crossings aggregation", x = "Matrix permeability", tag = "B") +  
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Perceptual range ----
# asymptotic 
asymptotic <- drm(habitat_amount_simulations$assess_top_sections ~ habitat_amount_simulations$perceptual_range, fct = DRC.asymReg())
x_values <- 5:max(habitat_amount_simulations$perceptual_range)
y_values <- coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values)
(assess_percep_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = perceptual_range, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   ylim(0,1) + scale_x_continuous(breaks = c(5, 15, 25, 35, 42), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Crossings aggregation", x = "Perceptual range", tag = "C") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Vision angle ----
# exponential 
exponential_growth <- drm(habitat_amount_simulations$assess_top_sections ~ habitat_amount_simulations$vision_angle, fct = DRC.expoGrowth())
x_values <- 90:max(habitat_amount_simulations$vision_angle)
y_values <- coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values)
(assess_vision_habitat_amount <-ggplot(data = habitat_amount_simulations, aes(x = vision_angle, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   ylim(0,1) + scale_x_continuous(breaks = c(90, 120, 150, 180), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   labs(y = "Crossings aggregation", x = "Vision angle", tag = "D") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

#### Total crossings ----
##### Proportion of habitat ----
# bragg4
bragg4 <- drm(habitat_amount_simulations$total_crossings ~ habitat_amount_simulations$proportion_of_habitat, fct = DRC.bragg.4())
x_values <- 10:max(habitat_amount_simulations$proportion_of_habitat)
y_values <- coefficients(bragg4)[2] + (coefficients(bragg4)[3] - coefficients(bragg4)[2]) * exp(- coefficients(bragg4)[1] * (x_values - coefficients(bragg4)[4])^2)
(crossings_prop_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = proportion_of_habitat, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   labs(y = "Total crossings", x = "Proportion of habitat", tag = "I") +
   scale_y_continuous(breaks =  c(0, 500, 1000, 1500), limits = c(0, 1500)) +
   scale_x_continuous(breaks = c(10,25,50,75,90), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Matrix permeability ----
# asymptotic 
asymptotic <- drm(habitat_amount_simulations$total_crossings ~ habitat_amount_simulations$matrix_permeability, fct = DRC.asymReg())
x_values <- 10:max(habitat_amount_simulations$matrix_permeability)
y_values <- coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values)
(crossings_perm_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = matrix_permeability, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   labs(y = "Total crossings", x = "Matrix permeability", tag = "J") +
   scale_y_continuous(breaks =  c(0, 500, 1000, 1500), limits = c(0, 1500)) +
   scale_x_continuous(breaks = c(10,30,50,70,90), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Perceptual range ----
# bragg4
bragg4 <- drm(habitat_amount_simulations$total_crossings ~ habitat_amount_simulations$perceptual_range, fct = DRC.bragg.4())
x_values <- 5:max(habitat_amount_simulations$perceptual_range)
y_values <- coefficients(bragg4)[2] + (coefficients(bragg4)[3] - coefficients(bragg4)[2]) * exp(- coefficients(bragg4)[1] * (x_values - coefficients(bragg4)[4])^2)
(crossings_percep_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = perceptual_range, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   labs(y = "Total crossings", x = "Perceptual range", tag = "K") +
   scale_y_continuous(breaks =  c(0, 500, 1000, 1500), limits = c(0, 1500)) +
   scale_x_continuous(breaks = c(5, 15, 25, 35, 42), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Vision angle ----
# asymptotic 
asymptotic <- drm(habitat_amount_simulations$total_crossings ~ habitat_amount_simulations$vision_angle, fct = DRC.asymReg())
x_values <- 90:max(habitat_amount_simulations$vision_angle)
y_values <- coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values)
(crossings_vision_habitat_amount <- ggplot(data = habitat_amount_simulations, aes(x = vision_angle, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   labs(y = "Total crossings", x = "Vision angle", tag = "L") +
   scale_y_continuous(breaks =  c(0, 500, 1000, 1500), limits = c(0, 1500)) +
   scale_x_continuous(breaks = c(90, 120, 150, 180), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

# Submodel configuration ----
configuration <- readRDS(here::here("results", "configuration_simulations.RDS"))
configuration_simulations <- configuration@simdesign@simoutput
#### Assess top sections ----
##### Scenarios ----
(assess_scenarios_config <- ggplot(configuration_simulations, aes(x = factor(scenario), y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect",  xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect",  xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
   stat_halfeye(adjust = 0.5, width = 0.75, justification = -0.25, .width = 0, point_colour = NA, fill = "black") +
   geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5) +
   labs(y = "Crossings aggregation", x = "Scenarios", tag = "E") +
   scale_x_discrete(labels=c("1" = "A", "2" = "B", "3" = "C", "4" = "D",
                             "5" = "E", "6" = "F",  "7" = "G"), expand = c(0,0.2)) +
   ylim(0,1)  + 
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)
##### Matrix permeability ----
# asymptotic 
asymptotic <- drm(configuration_simulations$assess_top_sections ~ configuration_simulations$matrix_permeability, fct = DRC.asymReg())
x_values <- 10:max(configuration_simulations$matrix_permeability)
y_values <- coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values)
(assess_perm_config <- ggplot(configuration_simulations, aes(x = matrix_permeability, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   labs(y = "Crossings aggregation", x = "Matrix permeability", tag = "F") +
   ylim(0,1) + scale_x_continuous(breaks = c(10,30,50,70,90), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Perceptual range ----
# exponential 
exponential_growth <- drm(configuration_simulations$assess_top_sections ~ configuration_simulations$perceptual_range, fct = DRC.expoGrowth())
x_values <- 5:max(configuration_simulations$perceptual_range)
y_values <- coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values)
(assess_percep_config <- ggplot(data = configuration_simulations, aes(x = perceptual_range, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   labs(y = "Crossings aggregation", x = "Perceptual range", tag = "G") +
   ylim(0,1) + scale_x_continuous(breaks = c(5, 15, 25, 35, 42), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Vision angle ----
# linear
(assess_vision_config <- ggplot(data = configuration_simulations, aes(x = vision_angle, y = assess_top_sections)) +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
   annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
   geom_point(size = 0.05) +
   geom_smooth(method = "lm", col = "red", linewidth = 0.75) +
   labs(y = "Crossings aggregation", x = "Vision angle", tag = "H") +
   ylim(0,1) + scale_x_continuous(breaks = c(90, 120, 150, 180), expand = c(0,0.5)) +
   geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

#### Total crossings ----
##### Scenarios ----
(crossings_scenarios_config <- ggplot(configuration_simulations, aes(x = factor(scenario), y = total_crossings)) +
   stat_halfeye(adjust = 0.5, width = 0.75, justification = -0.25, .width = 0, point_colour = NA, fill = "black") +
   geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5) +
   labs(y = "Total crossings", x = "Scenario", tag = "M") +
   scale_x_discrete(labels=c("1" = "A", "2" = "B", "3" = "C", "4" = "D",
                             "5" = "E", "6" = "F",  "7" = "G"), expand = c(0,0.2)) +
   scale_y_continuous(breaks =  c(0, 1200, 2400, 3600), limits = c(0,3600)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Matrix permeability ----
asymptotic <- drm(configuration_simulations$total_crossings ~ configuration_simulations$matrix_permeability, fct = DRC.asymReg())
x_values <- 10:max(configuration_simulations$matrix_permeability)
y_values <- coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values)
(crossings_perm_config <- ggplot(data = configuration_simulations, aes(x = matrix_permeability, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   labs(y = "Total crossings", x = "Matrix permeability", tag = "N") +
   scale_y_continuous(breaks =  c(0, 1200, 2400, 3600), limits = c(0,3600)) +
   scale_x_continuous(breaks = c(10, 30, 50, 70, 90), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

##### Perceptual range ----
# exponential 
exponential_growth <- drm(configuration_simulations$total_crossings ~ configuration_simulations$perceptual_range, fct = DRC.expoGrowth())
x_values <- 5:max(configuration_simulations$perceptual_range)
y_values <- coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values)
(crossings_percep_config <- ggplot(data = configuration_simulations, aes(x = perceptual_range, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y),
             col = "red", linewidth = 0.75) +
   labs(y = "Total crossings", x = "Perceptual range", tag = "O") +
   scale_y_continuous(breaks =  c(0, 1200, 2400, 3600), limits = c(0,3600)) +
   scale_x_continuous(breaks = c(5, 15, 25, 35, 42), expand = c(0,0.5)) +
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(fill = NA, colour = "grey20"), 
          panel.grid.major.y = element_line(colour = "grey92"),
          plot.tag = element_text(size = 10),
          plot.tag.position = c(0.025,1.02),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
)

##### Vision angle ----
asymptotic <- drm(configuration_simulations$total_crossings ~ configuration_simulations$vision_angle, fct = DRC.asymReg())
x_values <- 90:max(configuration_simulations$vision_angle)
y_values <- coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values)
(crossings_vision_config <- ggplot(data = configuration_simulations, aes(x = vision_angle, y = total_crossings)) +
   geom_point(size = 0.05) +
   geom_line(data = data.frame(x = x_values, y = y_values), aes(x, y), 
             col = "red", linewidth = 0.75) +
   labs(y = "Total crossings", x = "Vision angle", tag = "P") +
   scale_y_continuous(breaks =  c(0, 1200, 2400, 3600), limits = c(0, 3600)) +
   scale_x_continuous(breaks = c(90, 120, 150, 180), expand = c(0,0.5)) +
   theme(panel.background = element_rect(fill = "white"), 
         panel.border = element_rect(fill = NA, colour = "grey20"), 
         panel.grid.major.y = element_line(colour = "grey92"),
         plot.tag = element_text(size = 10),
         plot.tag.position = c(0.025,1.02),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))
)

#### PANEL ----
### CROSSINGS AGGREGATION ----
(habitat_aggreg <- assess_prop_habitat_amount + assess_perm_habitat_amount + assess_percep_habitat_amount + assess_vision_habitat_amount +
  plot_layout(ncol = 4) + plot_annotation(title = 'Habitat amount model', theme = theme(plot.title = element_text(size = 12, vjust = 1))))
(config_aggreg <- assess_scenarios_config + assess_perm_config + assess_percep_config + assess_vision_config +
  plot_layout(ncol = 4) + plot_annotation(title = 'Habitat arrangement model', theme = theme(plot.title = element_text(size = 12, vjust = 1))))

### TOTAL CROSSINGS ----
(habitat_total <- crossings_prop_habitat_amount + crossings_perm_habitat_amount + crossings_percep_habitat_amount + crossings_vision_habitat_amount +
  plot_layout(ncol = 4) + plot_annotation(title = 'Habitat amount model', theme = theme(plot.title = element_text(size = 12, vjust = 1))))
(config_total <- crossings_scenarios_config + crossings_perm_config + crossings_percep_config + crossings_vision_config +
  plot_layout(ncol = 4) + plot_annotation(title = 'Habitat arrangement model', theme = theme(plot.title = element_text(size = 12, vjust = 1))))

### all togheter 
(all_together <-  wrap_elements(habitat_aggreg) / wrap_elements(config_aggreg) /wrap_elements(habitat_total) / wrap_elements(config_total))

ggsave(all_together, filename = here::here("imagens", "inputs_outputs_relationship.png"), 
       dpi = 300, width = 3200, height = 4200, unit = "px")
