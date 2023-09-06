###################### 
#code for fitting multiple models (i.e., intercept only, linear, exponential and assintotic model) for distance decay curves (i.e., the  decay of  ecological community similarity with geographic distance)
#Bergamin, R. S., Bastazini, V. A. G., Vélez-Martin, E., Debastiani, V., Zanini, K. J., Loyola, R., & Müller, S. C. (2017). Linking beta diversity patterns to protected areas: lessons from the Brazilian Atlantic Rainforest. 
#Biodiversity and Conservation 26 (7): 1557–1568.
#DOI 10.1007/s10531-017-1315-y 
#####################

require(bbmle)
require(here)

# data
# habitat amount submodel
habitat_amount_simulations <- readRDS(here("results", "habitat_amount_6000amostras1.RDS"))
habitat_amount_variables <- habitat_amount_simulations@simdesign@simoutput
Y_asses <- habitat_amount_variables$assess_top_sections
Y_total <- habitat_amount_variables$total_crossings
X_prop <- habitat_amount_variables$proportion_of_habitat
X_perm <- habitat_amount_variables$matrix_permeability
X_percep <- habitat_amount_variables$perceptual_range
X_vision <- habitat_amount_variables$vision_angle

# configuration submodel
configuration_simulations <- readRDS(here("results", "configuration_simulations_6000.RDS"))
configuration_variables <- habitat_amount_simulations@simdesign@simoutput
Y_asses <- configuration_variables$assess_top_sections
Y_total <- configuration_variables$total_crossings
X_perm <- configuration_variables$matrix_permeability
X_percep <- configuration_variables$perceptual_range
X_vision <- configuration_variables$vision_angle



# functions
models_graph <- function (y, x) { 
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 

plot(y ~ x, xlab = "x", ylab = "y", pch = 20, cex = 0.5)
abline(null, col = "green")
abline(linear, col = "red")
lines(coefficients(exponential)[1] + (coefficients(exponential)[2])*log(1:max(x)), col = "yellow")
lines(coefficients(assintotic)[1] + (coefficients(assintotic)[2])/(1:max(x)), col = "lightblue")

#AIC 
AICctab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)

} 


### habitat amount fitting models ----
aggreg_prop <- models_graph(Y_asses, X_prop)
aggreg_perm <- models_graph(Y_asses, X_perm)

# configuration
## crossings aggregation 
#### matrix - exponencial
#### perceptual - linear
#### vision - linear
## total crossings
#### matrix - linear
### perceptual - linear
### vision - exponencial

# habitat amount
## crossings aggregation
#### proportion - linear
#### matrix - exponencial
#### perceptual - exponencial
#### vision - linear
## total crossings
#### proportion - exponencial
#### matrix - exponencial
#### perceptual - linear
#### vision - assintotica