##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## Script to run sensitivity analysis using the files created in the script to 
## run the simulations "simulations_habitat_amount."
## Model: Habitat configuration
#
require(dplyr)

# load file with simulations information
configuration_simulations <- readRDS(file = here::here("results", "habitat_configuration_simulations.rds"))

# create objects for input samples
random_sample_1_configuration <- configuration_simulations@simdesign@simobject[[1]][["X1"]]
random_sample_2_configuration <- configuration_simulations@simdesign@simobject[[1]][["X2"]]
experiment_design_configuration <- configuration_simulations@simdesign@siminput

# create objects for outputs
aggregation_values_configuration <- configuration_simulations@simdesign@simoutput$assess_top_sections
crossings_values_configuration <- configuration_simulations@simdesign@simoutput$total_crossings

### Sensitivity analysis ----

#### Crossings aggregation output ----
sensitivity_assess_configuration <- sensitivity::sobol2007(model = NULL, X1 = random_sample_1_configuration, X2 = random_sample_2_configuration, nboot = 300) 
sensitivity_assess_configuration$X <- experiment_design_configuration
sensitivity::tell(sensitivity_assess_configuration, (aggregation_values_configuration-mean(aggregation_values_configuration))/sd(aggregation_values_configuration))
print(sensitivity_assess_configuration)

# organizing data from first and total effect from sobol to plot
aggreg_first_graph_configuration <- sensitivity_assess_configuration[["S"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "first-order")
aggreg_total_graph_configuration <- sensitivity_assess_configuration[["T"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "total")
aggreg_graph_configuration <- rbind(aggreg_first_graph_configuration, aggreg_total_graph_configuration)

# plot sensitivity results
require(ggplot2)
(sensi_assess_configuration <- ggplot(aggreg_graph_configuration, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*S<sub>i</sub>*)", "Total effect (*S<sub>Ti</sub>*)"), name = " ") +
    labs(title = "C) Crossings aggregation", y = "Parameters", x = "Sobol Index") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("vision_angle", "scenario", "perceptual_range", "matrix_permeability"),
                     labels=c("vision_angle" = "Vision angle", "scenario" = "Scenarios",
                              "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
    theme(panel.background = element_rect(fill = "white"), 
          legend.key = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          panel.grid.major.y = element_line(colour = "grey99"),
          legend.position = "bottom",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 13),
          legend.text = ggtext::element_markdown(size = 13),
          plot.title = element_text(size = 13))
)

#### Total crossings output ----
sensitivity_crossings_configuration <- sensitivity::sobol2007(model = NULL, X1 = random_sample_1_configuration, X2 = random_sample_2_configuration, nboot = 300) 
sensitivity_crossings_configuration$X <- experiment_design_configuration
sensitivity::tell(sensitivity_crossings_configuration, (crossings_values_configuration-mean(crossings_values_configuration))/sd(crossings_values_configuration))
print(sensitivity_crossings_configuration)

# organizing data from first and total effect from sobol to plot
crossings_first_graph_configuration <- sensitivity_crossings_configuration[["S"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "first-order")
crossings_total_graph_configuration <- sensitivity_crossings_configuration[["T"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "total")
crossings_graph_configuration <- rbind(crossings_first_graph_configuration, crossings_total_graph_configuration) %>%
  dplyr::mutate(`min. c.i.` = ifelse(`min. c.i.` <= 0, 0, `min. c.i.`))

# plot sensitivity results
require(ggplot2)
(sensi_cross_configuration <- ggplot(crossings_graph_configuration, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*S<sub>i</sub>*)", "Total effect (*S<sub>Ti</sub>*)"), name = " ") +
    labs(title = "D) Total crossings", y = "Parameters", x = "Sobol Index") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("vision_angle", "scenario", "perceptual_range", "matrix_permeability"),
                     labels=c("vision_angle" = "Vision angle", "scenario" = "Scenarios",
                              "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
    theme(panel.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          panel.grid.major.y = element_line(colour = "grey99"),
          legend.position = "bottom",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 13),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.text = ggtext::element_markdown(size = 13),
          plot.title = element_text(size = 13))
)

# combining the two plots of habitat configuration:
(habitat_configuration <- sensi_assess_configuration + sensi_cross_configuration +
    patchwork::plot_layout(ncol = 2, guides = "collect") + 
    patchwork::plot_annotation(title = 'Habitat configuration model', 
                               theme = theme(legend.position = "bottom",
                                             legend.justification = 0.35,
                                             plot.title = element_text(size = 14, vjust = 5, hjust = 0.5))))

# to combine and save all plots of sensitivity analyses (habitat amount and habitat configuration models)
# habitat amount is created in the script "habitat_amount_sensitivity_analysis.R"
# combine
(all_together <- patchwork::wrap_elements(habitat_amount) / patchwork::wrap_elements(habitat_configuration) + 
                 patchwork::plot_layout(guides = "collect")) 
# save
ggsave(all_together, filename = here::here("figures", "sensitivity_analysis.png"), 
       dpi = 600, width = 7000, height = 6000, unit = "px")

