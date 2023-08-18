library(sensitivity)
library(dplyr)
library(here)
library(ggplot2)
library(gridExtra)

# obter inputs do objeto netlogo para adicionar na analise de sensibilidade
# carregar o objeto netlogo - pasta results, arquivo habitat_amount_simulations.rds
# inputs
habitat_amount_simulations <- readRDS(here("results", "habitat_amount_simulations2.rds"))
random_sample_1 <- habitat_amount_simulations@simdesign@simobject[[1]][["X1"]]
random_sample_2 <- habitat_amount_simulations@simdesign@simobject[[1]][["X2"]]
experiment_design <- habitat_amount_simulations@simdesign@siminput

# outputs
assess_values <- habitat_amount_simulations@simdesign@simoutput$assess_top_sections
crossings_values <- habitat_amount_simulations@simdesign@simoutput$total_crossings

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
                      labels = c("First-order index", "Total index"), name = " ") +
  labs(title = "Asses top sections", y = "Parameter", x = "Sobol Index") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_discrete(limits = c( "proportion_of_habitat", "perceptual_range", "vision_angle", "matrix_permeability"),
                   labels=c("vision_angle" = "Vision angle", "proportion_of_habitat" = "Proportion of habitat",
                            "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
  theme(legend.position = "bottom") )

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
  mutate(`min. c.i.` = ifelse(`min. c.i.` <= 0, 0, `min. c.i.`),
         original = ifelse(original <= 0, 0, original))

(sensi_cross <- ggplot(crossings_graph, aes(x = original, y = parameter,  group = index)) +
  geom_pointrange(aes(xmin = `min. c.i.`, xmax = `max. c.i.`, color = index), 
                  position = position_dodge(width = 0.3), size = 0.3) +
  scale_colour_manual(values = c("first-order" = "black", "total" = "grey50"),
                      labels = c("First-order index", "Total index"), name = " ") +
  labs(title = "Total crossings", y = "Parameter", x = "Sobol Index") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_discrete(limits = c("vision_angle", "matrix_permeability", "proportion_of_habitat", "perceptual_range"),
                  labels=c("vision_angle" = "Vision angle", "proportion_of_habitat" = "Proportion of habitat",
                          "perceptual_range" = "Perceptual range", "matrix_permeability" = "Matrix permeability")) +
   theme(legend.position = "bottom") )

grid.arrange(sensi_assess, sensi_cross, ncol = 1)


hist(habitat_amount_simulations1@simdesign@siminput$proportion_of_habitat)
table(habitat_amount_simulations1@simdesign@siminput$proportion_of_habitat)
hist(habitat_amount_simulations@simdesign@siminput$proportion_of_habitat)
table(habitat_amount_simulations@simdesign@siminput$proportion_of_habitat)

hist(habitat_amount_simulations@simdesign@siminput$matrix_permeability)
table(habitat_amount_simulations@simdesign@siminput$matrix_permeability)
hist(habitat_amount_simulations1@simdesign@siminput$matrix_permeability)
table(habitat_amount_simulations1@simdesign@siminput$matrix_permeability)


hist(habitat_amount_simulations@simdesign@siminput$perceptual_range)
table(habitat_amount_simulations@simdesign@siminput$perceptual_range)
hist(habitat_amount_simulations1@simdesign@siminput$perceptual_range)
table(habitat_amount_simulations1@simdesign@siminput$perceptual_range)

hist(configuration_simulations@simdesign@siminput$matrix_permeability)
table(configuration_simulations@simdesign@siminput$matrix_permeability)
