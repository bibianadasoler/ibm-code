###################### 
#code for fitting multiple models (i.e., intercept only, linear, exponential and assintotic model) for distance decay curves (i.e., the  decay of  ecological community similarity with geographic distance)
#Bergamin, R. S., Bastazini, V. A. G., Vélez-Martin, E., Debastiani, V., Zanini, K. J., Loyola, R., & Müller, S. C. (2017). Linking beta diversity patterns to protected areas: lessons from the Brazilian Atlantic Rainforest. 
#Biodiversity and Conservation 26 (7): 1557–1568.
#DOI 10.1007/s10531-017-1315-y 
#####################

require(bbmle)
require(aomisc)
require(here)


# configuration submodel
configuration_simulations <- readRDS(here("results", "configuration_simulations_dezmil.RDS"))
configuration_variables <- configuration_simulations@simdesign@simoutput

# crossings aggregation ----
## matrix permeability ----
y <- configuration_variables$assess_top_sections
x <- configuration_variables$matrix_permeability
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
# BIC 
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
exponential
R2nls(exponential)
# a       b 
# 0.8930 -0.1052 
# $PseudoR2 0.2689933

## perceptual range ----
y <- configuration_variables$assess_top_sections
x <- configuration_variables$perceptual_range
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
#AIC 
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
linear
summary(linear)
# a       b 
# 0.438287 0.002456
# Adjusted R-squared:  0.05213 

## vision angle ----
y <- configuration_variables$assess_top_sections
x <- configuration_variables$vision_angle
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
#AIC 
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
linear
summary(linear)
# a       b 
# 0.317306 0.001324
# Adjusted R-squared:  0.08947


# total crossings ----
## matrix permeability ----
y <- configuration_variables$total_crossings
x <- configuration_variables$matrix_permeability
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
#AIC 
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
exponential
R2nls(exponential)
# a       b 
# 1171.28 -87.09 
# PseudoR2 0.01270231

## perceptual range ----
y <- configuration_variables$total_crossings
x <- configuration_variables$perceptual_range
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
#AIC 
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
linear
summary(linear)
# a       b 
# 347.79 21.05
# Adjusted R-squared:  0.2639 

## vision angle ----
y <- configuration_variables$total_crossings
x <- configuration_variables$vision_angle
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
#AIC 
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
assintotic
R2nls(assintotic)
# a       b 
# 1141 -38784
# PseudoR2 0.01892259

# graph ----
plot(y ~ x, xlab = "perm", ylab = "aggreg", pch = 20, cex = 0.5)
abline(null, col = "green")
abline(linear, col = "red")
lines(coefficients(exponential)[1] + (coefficients(exponential)[2])*log(1:max(x)), col = "yellow")
lines(coefficients(assintotic)[1] + (coefficients(assintotic)[2])/(1:max(x)), col = "lightblue")
