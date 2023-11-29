library(here)
library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)

##### HABITAT AMOUNT SUBMODEL 
habitat_amount <- readRDS(here("results", "habitat_amount_simulations.RDS"))
habitat_amount_simulations <- habitat_amount@simdesign@simoutput

habitat_categoric <- habitat_amount_simulations %>%
  mutate(class_prop = case_when(proportion_of_habitat >= 10 & proportion_of_habitat < 30 ~ "10-30",
                                proportion_of_habitat >= 30 & proportion_of_habitat < 50 ~ "30-50",
                                proportion_of_habitat >= 50 & proportion_of_habitat < 70 ~ "50-70",
                                proportion_of_habitat >= 70 & proportion_of_habitat <= 90 ~ "70-90"),
         class_matrix = case_when(matrix_permeability >= 10 & matrix_permeability < 30 ~ "a10-30",
                                  matrix_permeability >= 30 & matrix_permeability < 50 ~ "b30-50",
                                  matrix_permeability >= 50 & matrix_permeability < 70 ~ "c50-70",
                                  matrix_permeability >= 70 & matrix_permeability <= 90 ~ "d70-90"),
         class_vision = case_when(vision_angle >= 90 & vision_angle < 120 ~ "a90-120",
                                  vision_angle >= 120 & vision_angle < 150 ~ "b120-150",
                                  vision_angle >= 150 & vision_angle <= 180 ~ "c150-180") ) %>%
  dplyr::select(assess_top_sections, total_crossings, class_prop, class_matrix, perceptual_range, class_vision) 


habitat_aggreg <- ggplot(habitat_categoric, aes(perceptual_range, assess_top_sections, col = class_prop)) +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
  geom_smooth() +
  scale_color_manual(values = c("#d7301f", "#b30000", "#7f0000", "black")) +
  scale_y_continuous(n.breaks = 5, limits = c(0,1), sec.axis = sec_axis(~., name = "Matrix permeability", breaks = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Vision angle", breaks = NULL)) +
  facet_grid(class_matrix ~ class_vision, 
             labeller = labeller(class_vision = c("a90-120" = "90-120", "b120-150" = "120-150", "c150-180" = "150-180"),
                                 class_matrix = c("a10-30" = "10-30", "b30-50" = "30-50", "c50-70" = "50-70", "d70-90" = "70-90")))  +
  theme_bw() +
  labs(title = "Habitat amount model", tag = "A", x = "Perceptual range", y ="Crossings aggregation", col = "Proportion of habitat") +
  theme(legend.position = "bottom",
        plot.tag.position = c(0.02, 0.99),
        text = element_text(size = 12),
        legend.text = element_markdown(size = 11),
        axis.title.y.right = element_text(vjust = 1),
        axis.title.x.top = element_text(vjust = .4))

habitat_total <- ggplot(habitat_categoric, aes(perceptual_range, total_crossings, col = class_prop)) +
  geom_smooth() +
  scale_color_manual(values = c("#d7301f", "#b30000", "#7f0000", "black")) +
  scale_y_continuous(breaks = c(0, 750, 1500), limits = c(0,1500), sec.axis = sec_axis(~., name = "Matrix permeability", breaks = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Vision angle", breaks = NULL)) +
  facet_grid(class_matrix ~ class_vision, 
             labeller = labeller(class_vision = c("a90-120" = "90-120", "b120-150" = "120-150", "c150-180" = "150-180"),
                                 class_matrix = c("a10-30" = "10-30", "b30-50" = "30-50", "c50-70" = "50-70", "d70-90" = "70-90")))  +
  theme_bw() +
  labs(title = "Habitat amount model", tag = "A", x = "Perceptual range", y ="Total crossings", col = "Proportion of habitat") +
  theme(legend.position = "bottom",
        plot.tag.position = c(0.02, 0.99),
        text = element_text(size = 12),
        legend.text = element_markdown(size = 11),
        axis.title.y.right = element_text(vjust = 1),
        axis.title.x.top = element_text(vjust = .4))


##### CONFIGURATION SUBMODEL ----
configuration <- readRDS(here("results", "configuration_simulations.RDS"))
configuration_simulations <- configuration@simdesign@simoutput

config_categoric <- configuration_simulations %>%
  mutate(class_matrix = case_when(matrix_permeability >= 10 & matrix_permeability < 30 ~ "a10-30",
                                  matrix_permeability >= 30 & matrix_permeability < 50 ~ "b30-50",
                                  matrix_permeability >= 50 & matrix_permeability < 70 ~ "c50-70",
                                  matrix_permeability >= 70 & matrix_permeability <= 90 ~ "d70-90"),
         class_vision = case_when(vision_angle >= 90 & vision_angle < 120 ~ "a90-120",
                                  vision_angle >= 120 & vision_angle < 150 ~ "b120-150",
                                  vision_angle >= 150 & vision_angle <= 180 ~ "c150-180"),
         scenario = as.character(scenario)) %>%
  dplyr::select(assess_top_sections, total_crossings, scenario, class_matrix, perceptual_range, class_vision) 


config_aggreg <- ggplot(config_categoric, aes(perceptual_range, assess_top_sections, colour = scenario)) +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.25, ymax = 1, alpha = 0.15, fill = "grey15") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.5, ymax = 1, alpha = 0.15, fill = "grey15") +
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 0.75, ymax = 1, alpha = 0.15, fill = "grey15") +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
  geom_smooth() +
  scale_color_manual(values = c("black", "#4575b4", "#74add1", "#fdae61", "#f46d43", "red", "#a50026"),
                     labels = c("A", "B", "C", "D", "E", "F", "G")) +  
  scale_y_continuous(n.breaks = 5, limits = c(0,1), sec.axis = sec_axis(~., name = "Matrix permeability", breaks = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Vision angle", breaks = NULL)) +
  facet_grid(class_matrix ~ class_vision, 
             labeller = labeller(class_vision = c("a90-120" = "90-120", "b120-150" = "120-150", "c150-180" = "150-180"),
                                 class_matrix = c("a10-30" = "10-30", "b30-50" = "30-50", "c50-70" = "50-70", "d70-90" = "70-90")))  +
  theme_bw() +
  labs(title = "Habitat arrangement model", tag = "B", x = "Perceptual range", y ="Crossings aggregation", col = "Scenarios") +
  theme(legend.position = "bottom",
        plot.tag.position = c(0.02, 0.99),
        text = element_text(size = 12),
        legend.text = element_markdown(size = 11),
        axis.title.y.right = element_text(vjust = 1),
        axis.title.x.top = element_text(vjust = .4)) +
  guides(colour = guide_legend(ncol = 7))

config_total <- ggplot(config_categoric, aes(perceptual_range, total_crossings, col = scenario)) +
  geom_smooth() +
  scale_color_manual(values = c("black", "#4575b4", "#74add1", "#fdae61", "#f46d43", "red", "#a50026"),
                     labels = c("A", "B", "C", "D", "E", "F", "G")) +  
  scale_y_continuous(breaks = c(0, 1800, 3600), limits = c(0,3600), sec.axis = sec_axis(~., name = "Matrix permeability", breaks = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Vision angle", breaks = NULL)) +
  facet_grid(class_matrix ~ class_vision, 
             labeller = labeller(class_vision = c("a90-120" = "90-120", "b120-150" = "120-150", "c150-180" = "150-180"),
                                 class_matrix = c("a10-30" = "10-30", "b30-50" = "30-50", "c50-70" = "50-70", "d70-90" = "70-90")))  +
  theme_bw() +
  labs(title = "Habitat arrangement model", tag = "B", x = "Perceptual range", y ="Total crossings", col = "Scenarios") +
  theme(legend.position = "bottom",
        plot.tag.position = c(0.02, 0.99),
        text = element_text(size = 12),
        legend.text = element_markdown(size = 11),
        axis.title.y.right = element_text(vjust = 1),
        axis.title.x.top = element_text(vjust = .4)) +
  guides(colour = guide_legend(ncol = 7))

### panel crossings aggregation
aggreg <- wrap_elements(habitat_aggreg) / wrap_elements(config_aggreg)
ggsave(aggreg, filename = here::here("imagens", "inputs_aggregation.png"), 
       dpi = 300, width = 3200, height = 4200, unit = "px")


### panel total crossings 
total <- wrap_elements(habitat_total) / wrap_elements(config_total)
ggsave(total, filename = here::here("imagens", "inputs_total.png"), 
       dpi = 300, width = 3200, height = 4200, unit = "px")
