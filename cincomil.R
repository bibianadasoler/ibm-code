# unindo 5 mil 

habitat_amount_simulations1 <- readRDS(here("results", "re", "habitat_amount_5mil1.rds"))
random_sample_1_1 <- habitat_amount_simulations1@simdesign@simobject[[1]][["X1"]]
random_sample_2_1 <- habitat_amount_simulations1@simdesign@simobject[[1]][["X2"]]
output_1 <- habitat_amount_simulations1@simdesign@simoutput
experiment_design_1 <- output_1[,2:5]  #habitat_amount_simulations1@simdesign@siminput

habitat_amount_simulations <- readRDS(here("results", "re", "habitat_amount_5mil2.rds"))
random_sample_1 <- habitat_amount_simulations@simdesign@simobject[[1]][["X1"]]
random_sample_2 <- habitat_amount_simulations@simdesign@simobject[[1]][["X2"]]
output <- habitat_amount_simulations@simdesign@simoutput
experiment_design <- output[,2:5] #habitat_amount_simulations@simdesign@siminput

random_sample_1_FINAL <- rbind(random_sample_1_1, random_sample_1)
random_sample_2_FINAL <- rbind(random_sample_2_1, random_sample_2)
experiment_design_FINAL <- rbind(experiment_design_1, experiment_design)
output_FINAL <- rbind(output_1, output)
assess_values_FINAL <-output_FINAL$assess_top_sections
crossings_values_FINAL <- output_FINAL$total_crossings


sensitivity_assess_FINAL <- sobol2007(model = NULL, X1 = random_sample_1_FINAL, X2 = random_sample_2_FINAL, nboot = 300) 
sensitivity_assess_FINAL$X <- experiment_design_FINAL
# bias corrected 
tell(sensitivity_assess_FINAL, (assess_values_FINAL-mean(assess_values_FINAL))/sd(assess_values_FINAL))
print(sensitivity_assess_FINAL)
plot(sensitivity_assess_FINAL)




# outputs
