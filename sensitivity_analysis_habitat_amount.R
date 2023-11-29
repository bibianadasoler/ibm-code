##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## Script to run sensitivity analysis using the files created in the script to 
## run the simulations "simulations_habitat_amount."
## Model: Habitat amount
#

# load file with simulations information
habitat_amount_simulations <- readRDS(here::here("results", "habitat_amount_simulations.rds"))

# create objects for input samples
random_sample_1_amount <- habitat_amount_simulations@simdesign@simobject[[1]][["X1"]]
random_sample_2_amount <- habitat_amount_simulations@simdesign@simobject[[1]][["X2"]]
experiment_design_amount <- habitat_amount_simulations@simdesign@siminput

# create objects for outputs
aggregation_values_amount <- habitat_amount_simulations@simdesign@simoutput$assess_top_sections
crossings_values_amount <- habitat_amount_simulations@simdesign@simoutput$total_crossings

### Sensitivity analysis ----

#### Crossings aggregation output ----
sensitivity_aggreg_amount <- sensitivity::sobol2007(model = NULL, X1 = random_sample_1_amount, X2 = random_sample_2_amount, nboot = 300) 
sensitivity_aggreg_amount$X <- experiment_design_amount
sensitivity::tell(sensitivity_aggreg_amount, (aggregation_values_amount-mean(aggregation_values_amount))/sd(aggregation_values_amount))
print(sensitivity_aggreg_amount)

# organizing data from first and total effect from sobol to plot
aggreg_first_graph_amount <- sensitivity_aggreg_amount[["S"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "first-order")
aggreg_total_graph_amount <- sensitivity_aggreg_amount[["T"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "total")
aggreg_graph_amount <- rbind(aggreg_first_graph_amount, aggreg_total_graph_amount)

# plot sensitivity results
require(ggplot2)
(sensi_aggreg_amount <- ggplot(aggreg_graph_amount, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*S<sub>i</sub>*)", "Total effect (*S<sub>Ti</sub>*)"), name = " ") +
    labs(title = "Crossings aggregation", y = "Parameters", x = "Sobol Index", tag = "A") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("perceptual_range", "vision_angle", "proportion_of_habitat", "matrix_permeability"),
                     labels=c("vision_angle" = "Vision angle", "proportion_of_habitat" = "Proportion of habitat",
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
sensitivity_crossings_amount <- sensitivity::sobol2007(model = NULL, X1 = random_sample_1_amount, X2 = random_sample_2_amount, nboot = 300) 
sensitivity_crossings_amount$X <- experiment_design_amount
sensitivity::tell(sensitivity_crossings_amount, (crossings_values_amount-mean(crossings_values_amount))/sd(crossings_values_amount))
print(sensitivity_crossings_amount)

# organizing data from first and total effect from sobol to plot
crossings_first_graph_amount <- sensitivity_crossings_amount[["S"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "first-order")
crossings_total_graph_amount <- sensitivity_crossings_amount[["T"]] %>%
  dplyr::mutate(parameter = row.names(.),
                index = "total")
crossings_graph_amount <- rbind(crossings_first_graph_amount, crossings_total_graph_amount) %>%
  dplyr::mutate(`min. c.i.` = ifelse(`min. c.i.` <= 0, 0, `min. c.i.`),
                original = ifelse(original <= 0, 0, original))

# plot sensitivity results
require(ggplot2)
(sensi_cross_amount <- ggplot(crossings_graph_amount, aes(x = original, y = parameter,  group = index)) +
    geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                        labels = c("Main effect (*S<sub>i</sub>*)", "Total effect (*S<sub>Ti</sub>*)"), name = " ") +
    labs(title = "Total crossings", y = "Parameters", x = "Sobol Index", tag = "B") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(limits = c("vision_angle", "matrix_permeability",  "proportion_of_habitat", "perceptual_range"),
                     labels=c("vision_angle" = "Vision angle", "proportion_of_habitat" = "Proportion of habitat",
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

# combining the two plots of habitat amount:
(habitat_amount <- sensi_aggreg_amount + sensi_cross_amount + 
    patchwork::plot_layout(ncol = 2, guides = "collect") +
    patchwork::plot_annotation(title = 'Habitat amount model', 
                               theme = theme(legend.position = 'none', 
                                             plot.title = element_text(size = 12, vjust = 1))))

## go to script "habitat_arrangment_sensitivity_analysis to combine and save figures
