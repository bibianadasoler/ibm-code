##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## Script to run sensitivity analysis using the files created in the script to 
## run the simulations "simulations_habitat_amount."
## Model: Habitat arrangement
#

# load file with simulations information
arrangement_simulations <- readRDS(file = here::here("results", "habitat_arrangement_simulations.rds"))

# create objects for input samples
random_sample_1_arrangement <- arrangement_simulations@simdesign@simobject[[1]][["X1"]]
random_sample_2_arrangement <- arrangement_simulations@simdesign@simobject[[1]][["X2"]]
experiment_design_arrangement <- arrangement_simulations@simdesign@siminput

# create objects for outputs
aggregation_values_arrangement <- arrangement_simulations@simdesign@simoutput$assess_top_sections
crossings_values_arrangement <- arrangement_simulations@simdesign@simoutput$total_crossings

### Sensitivity analysis ----

#### Crossings aggregation output ----
sensitivity_assess_arrangement <- sensitivity::sobol2007(model = NULL, X1 = random_sample_1_arrangement, X2 = random_sample_2_arrangement, nboot = 300) 
sensitivity_assess_arrangement$X <- experiment_design_arrangement
sensitivity::tell(sensitivity_assess_arrangement, (aggregation_values_arrangement-mean(aggregation_values_arrangement))/sd(aggregation_values_arrangement))
print(sensitivity_assess_arrangement)

# organizing data from first and total effect from sobol to plot
aggreg_first_graph_arrangement <- sensitivity_assess_arrangement[["S"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "first-order")
aggreg_total_graph_arrangement <- sensitivity_assess_arrangement[["T"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "total")
aggreg_graph_arrangement <- rbind(aggreg_first_graph_arrangement, aggreg_total_graph_arrangement)

# plot sensitivity results
require(ggplot2)
(sensi_assess_arrangement <- ggplot(aggreg_graph_arrangement, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*S<sub>i</sub>*)", "Total effect (*S<sub>Ti</sub>*)"), name = " ") +
    labs(title = "Crossings aggregation", y = "Parameters", x = "Sobol Index", tag = "C") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("perceptual_range", "vision_angle", "scenario", "matrix_permeability"),
                     labels=c("vision_angle" = "Vision angle", "scenario" = "Scenarios",
                              "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
    theme(panel.background = element_rect(fill = "white"), 
          legend.key = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          panel.grid.major.y = element_line(colour = "grey99"),
          legend.position = "bottom",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = ggtext::element_markdown(size = 11),
          plot.title = element_text(size = 12),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.01,0.99))
)

#### Total crossings output ----
sensitivity_crossings_arrangement <- sensitivity::sobol2007(model = NULL, X1 = random_sample_1_arrangement, X2 = random_sample_2_arrangement, nboot = 300) 
sensitivity_crossings_arrangement$X <- experiment_design_arrangement
sensitivity::tell(sensitivity_crossings_arrangement, (crossings_values_arrangement-mean(crossings_values_arrangement))/sd(crossings_values_arrangement))
print(sensitivity_crossings_arrangement)

# organizing data from first and total effect from sobol to plot
crossings_first_graph_arrangement <- sensitivity_crossings_arrangement[["S"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "first-order")
crossings_total_graph_arrangement <- sensitivity_crossings_arrangement[["T"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "total")
crossings_graph_arrangement <- rbind(crossings_first_graph_arrangement, crossings_total_graph_arrangement) %>%
  dplyr::mutate(`min. c.i.` = ifelse(`min. c.i.` <= 0, 0, `min. c.i.`))

# plot sensitivity results
require(ggplot2)
(sensi_cross_arrangement <- ggplot(crossings_graph_arrangement, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*S<sub>i</sub>*)", "Total effect (*S<sub>Ti</sub>*)"), name = " ") +
    labs(title = "Total crossings", y = "Parameters", x = "Sobol Index", tag = "D") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("matrix_permeability", "vision_angle","perceptual_range", "scenario"),
                     labels=c("vision_angle" = "Vision angle", "scenario" = "Scenarios",
                              "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
    theme(panel.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          panel.grid.major.y = element_line(colour = "grey99"),
          legend.position = "bottom",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = ggtext::element_markdown(size = 11),
          plot.title = element_text(size = 12),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.01,0.99))
)

# combining the two plots of habitat arrangement:
(habitat_arrangement <- sensi_assess_arrangement + sensi_cross_arrangement +
    patchwork::plot_layout(ncol = 2, guides = "collect") + 
    patchwork::plot_annotation(title = 'Habitat arrangement model', 
                               theme = theme(legend.position = 'bottom', 
                                             plot.title = element_text(size = 12, vjust = 1))))

# to combine and save all plots of sensitivity analyses (habitat amount and habitat arrangement models)
# habitat amount is created in the script "habitat_amount_sensitivity_analysis.R"
# combine
(all_together <- patchwork::wrap_elements(habitat_amount) / patchwork::wrap_elements(habitat_arrangement) + 
                 patchwork::plot_layout(guides = "collect"))
# save
ggsave(all_together, filename = here("figures", "sensitivity_analysis.png"), 
       dpi = 600, width = 7000, height = 6000, unit = "px")

=======
##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## Script to run sensitivity analysis using the files created in the script to 
## run the simulations "simulations_habitat_amount."
## Model: Habitat arrangement
#

# load file with simulations information
arrangement_simulations <- readRDS(file = here::here("results", "habitat_arrangement_simulations.rds"))

# create objects for input samples
random_sample_1_arrangement <- arrangement_simulations@simdesign@simobject[[1]][["X1"]]
random_sample_2_arrangement <- arrangement_simulations@simdesign@simobject[[1]][["X2"]]
experiment_design_arrangement <- arrangement_simulations@simdesign@siminput

# create objects for outputs
aggregation_values_arrangement <- arrangement_simulations@simdesign@simoutput$assess_top_sections
crossings_values_arrangement <- arrangement_simulations@simdesign@simoutput$total_crossings

### Sensitivity analysis ----

#### Crossings aggregation output ----
sensitivity_assess_arrangement <- sensitivity::sobol2007(model = NULL, X1 = random_sample_1_arrangement, X2 = random_sample_2_arrangement, nboot = 300) 
sensitivity_assess_arrangement$X <- experiment_design_arrangement
sensitivity::tell(sensitivity_assess_arrangement, (aggregation_values_arrangement-mean(aggregation_values_arrangement))/sd(aggregation_values_arrangement))
print(sensitivity_assess_arrangement)

# organizing data from first and total effect from sobol to plot
aggreg_first_graph_arrangement <- sensitivity_assess_arrangement[["S"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "first-order")
aggreg_total_graph_arrangement <- sensitivity_assess_arrangement[["T"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "total")
aggreg_graph_arrangement <- rbind(aggreg_first_graph_arrangement, aggreg_total_graph_arrangement)

# plot sensitivity results
require(ggplot2)
(sensi_assess_arrangement <- ggplot(aggreg_graph_arrangement, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*S<sub>i</sub>*)", "Total effect (*S<sub>Ti</sub>*)"), name = " ") +
    labs(title = "Crossings aggregation", y = "Parameters", x = "Sobol Index", tag = "C") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("perceptual_range", "vision_angle", "scenario", "matrix_permeability"),
                     labels=c("vision_angle" = "Vision angle", "scenario" = "Scenarios",
                              "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
    theme(panel.background = element_rect(fill = "white"), 
          legend.key = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          panel.grid.major.y = element_line(colour = "grey99"),
          legend.position = "bottom",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = ggtext::element_markdown(size = 11),
          plot.title = element_text(size = 12),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.01,0.99))
)

#### Total crossings output ----
sensitivity_crossings_arrangement <- sensitivity::sobol2007(model = NULL, X1 = random_sample_1_arrangement, X2 = random_sample_2_arrangement, nboot = 300) 
sensitivity_crossings_arrangement$X <- experiment_design_arrangement
sensitivity::tell(sensitivity_crossings_arrangement, (crossings_values_arrangement-mean(crossings_values_arrangement))/sd(crossings_values_arrangement))
print(sensitivity_crossings_arrangement)

# organizing data from first and total effect from sobol to plot
crossings_first_graph_arrangement <- sensitivity_crossings_arrangement[["S"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "first-order")
crossings_total_graph_arrangement <- sensitivity_crossings_arrangement[["T"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "total")
crossings_graph_arrangement <- rbind(crossings_first_graph_arrangement, crossings_total_graph_arrangement) %>%
  dplyr::mutate(`min. c.i.` = ifelse(`min. c.i.` <= 0, 0, `min. c.i.`))

# plot sensitivity results
require(ggplot2)
(sensi_cross_arrangement <- ggplot(crossings_graph_arrangement, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*S<sub>i</sub>*)", "Total effect (*S<sub>Ti</sub>*)"), name = " ") +
    labs(title = "Total crossings", y = "Parameters", x = "Sobol Index", tag = "D") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("matrix_permeability", "vision_angle","perceptual_range", "scenario"),
                     labels=c("vision_angle" = "Vision angle", "scenario" = "Scenarios",
                              "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
    theme(panel.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          panel.grid.major.y = element_line(colour = "grey99"),
          legend.position = "bottom",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = ggtext::element_markdown(size = 11),
          plot.title = element_text(size = 12),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.01,0.99))
)

# combining the two plots of habitat arrangement:
(habitat_arrangement <- sensi_assess_arrangement + sensi_cross_arrangement +
    patchwork::plot_layout(ncol = 2, guides = "collect") + 
    patchwork::plot_annotation(title = 'Habitat arrangement model', 
                               theme = theme(legend.position = 'bottom', 
                                             plot.title = element_text(size = 12, vjust = 1))))

# to combine and save all plots of sensitivity analyses (habitat amount and habitat arrangement models)
# habitat amount is created in the script "habitat_amount_sensitivity_analysis.R"
# combine
(all_together <- patchwork::wrap_elements(habitat_amount) / patchwork::wrap_elements(habitat_arrangement) + 
                 patchwork::plot_layout(guides = "collect"))
# save
ggsave(all_together, filename = here("figures", "sensitivity_analysis.png"), 
       dpi = 600, width = 7000, height = 6000, unit = "px")

>>>>>>> a6440e5850e1344c6b7da7ca8ec79541993a403e
