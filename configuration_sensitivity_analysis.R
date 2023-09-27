library(sensitivity)
library(dplyr)
library(here)
library(ggplot2)
library(patchwork)

# obter inputs do objeto netlogo para adicionar na analise de sensibilidade
# carregar o objeto netlogo - pasta results, arquivo configuration_simulations.rds
# inputs
configuration_simulations <- readRDS(file = here("results", "configuration_simulations.rds"))
random_sample_1 <- configuration_simulations@simdesign@simobject[[1]][["X1"]]
random_sample_2 <- configuration_simulations@simdesign@simobject[[1]][["X2"]]
experiment_design <- configuration_simulations@simdesign@siminput

# outputs
assess_values <- configuration_simulations@simdesign@simoutput$assess_top_sections
crossings_values <- configuration_simulations@simdesign@simoutput$total_crossings

# analise sensibilidade 
# variavel assess_top_sections
sensitivity_assess <- sobol2007(model = NULL, X1 = random_sample_1, X2 = random_sample_2, nboot = 300) 
sensitivity_assess$X <- experiment_design
# bias corrected 
tell(sensitivity_assess, (assess_values-mean(assess_values))/sd(assess_values))
print(sensitivity_assess)
plot(sensitivity_assess)

assess_first_graph <- sensitivity_assess[["S"]] %>%
  mutate(parameter = row.names(.),
         index = "first-order")
assess_total_graph <- sensitivity_assess[["T"]] %>%
  mutate(parameter = row.names(.),
         index = "total")
assess_graph <- rbind(assess_first_graph, assess_total_graph)

(sensi_assess <- ggplot(assess_graph, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*Si*)", "Total effect (*STi*)"), name = " ") +
    labs(title = "Crossings aggregation", y = "Parameters", x = "Sobol Index", tag = "A") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("perceptual_range", "vision_angle", "scenario", "matrix_permeability"),
                     labels=c("vision_angle" = "Vision angle", "scenario" = "Scenarios",
                              "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
    theme(panel.background = element_rect(fill = "white"), 
          legend.key = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          panel.grid.major.y = element_line(colour = "grey99"),
          legend.position = "bottom",
          legend.text = element_markdown(size = 11),
          plot.title = element_text(size = 12),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.01,0.99))
)


# total crossings
sensitivity_crossings <- sobol2007(model = NULL, X1 = random_sample_1, X2 = random_sample_2, nboot = 300) 
sensitivity_crossings$X <- experiment_design
# bias corrected 
tell(sensitivity_crossings, (crossings_values-mean(crossings_values))/sd(crossings_values))
print(sensitivity_crossings)
plot(sensitivity_crossings)

crossings_first_graph <- sensitivity_crossings[["S"]] %>%
  mutate(parameter = row.names(.),
         index = "first-order")
crossings_total_graph <- sensitivity_crossings[["T"]] %>%
  mutate(parameter = row.names(.),
         index = "total")
crossings_graph <- rbind(crossings_first_graph, crossings_total_graph) %>%
  mutate(`min. c.i.` = ifelse(`min. c.i.` <= 0, 0, `min. c.i.`))

(sensi_cross <- ggplot(crossings_graph, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*Si*)", "Total effect (*STi*)"), name = " ") +
    labs(title = "Total crossings", y = "Parameters", x = "Sobol Index", tag = "B") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("matrix_permeability", "vision_angle","perceptual_range", "scenario"),
                     labels=c("vision_angle" = "Vision angle", "scenario" = "Scenarios",
                              "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
    theme(panel.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          panel.grid.major.y = element_line(colour = "grey99"),
          legend.position = "bottom",
          legend.text = element_markdown(size = 11),
          plot.title = element_text(size = 12),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.01,0.99))
)

sensi_assess + sensi_cross + plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = 'bottom', legend.key.size = unit(1, 'cm'))

                                                                               