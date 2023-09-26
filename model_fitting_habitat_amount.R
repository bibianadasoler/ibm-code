###################### 
#code for fitting multiple models (i.e., intercept only, linear, exponential and assintotic model) for distance decay curves (i.e., the  decay of  ecological community similarity with geographic distance)
#Bergamin, R. S., Bastazini, V. A. G., Vélez-Martin, E., Debastiani, V., Zanini, K. J., Loyola, R., & Müller, S. C. (2017). Linking beta diversity patterns to protected areas: lessons from the Brazilian Atlantic Rainforest. 
#Biodiversity and Conservation 26 (7): 1557–1568.
#DOI 10.1007/s10531-017-1315-y 
#####################

require(bbmle)
require(here)
require(aomisc)

# habitat amount submodel ----
habitat_amount <- readRDS(here("results", "habitat_amount_dezmil.RDS"))
habitat_amount_variables <- habitat_amount@simdesign@simoutput

# crossings aggregation ----
### proportion of habitat ----
y <- habitat_amount_variables$assess_top_sections
x <- habitat_amount_variables$proportion_of_habitat
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
# BIC 
BICtab(null, linear, exponential, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
#LINEAR 
R2nls(exponential)
# a       b 
# 0.6549 -0.0489 

### matrix permeability ----
y <- habitat_amount_variables$assess_top_sections
x <- habitat_amount_variables$matrix_permeability
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# BIC 
BICtab(null, linear, exponential,  nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
exponential
R2nls(exponential)
# a       b 
# 0.9241 -0.1120 

### perceptual range ----
y <- habitat_amount_variables$assess_top_sections
x <- habitat_amount_variables$perceptual_range
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# BIC 
BICtab(null, linear, exponential,  nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
exponential
summary(linear)$adj.r.squared
# a       b 
# 0.39359 0.03562

### vision angle ----
y <- habitat_amount_variables$assess_top_sections
x <- habitat_amount_variables$vision_angle
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# BIC 
BICtab(null, linear, exponential, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
linear
summary(linear)$adj.r.squared
# a       b 
# 0.286302 0.001593


# total crossings ----
### proportion of habitat ----
y <- habitat_amount_variables$total_crossings
x <- habitat_amount_variables$proportion_of_habitat
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# BIC 
BICtab(null, linear, exponential,  nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
linear
R2nls(assintotic)
# a       b 
# 565.793 2.163 

### matrix permeability ----
y <- habitat_amount_variables$total_crossings
x <- habitat_amount_variables$matrix_permeability
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# BIC 
BICtab(null, linear, exponential,  nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
exponential
R2nls(exponential)
# a       b 
# 356.54 84.09 

## perceptual range ----
y <- habitat_amount_variables$total_crossings
x <- habitat_amount_variables$perceptual_range
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# BIC 
BICtab(null, linear, exponential, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
linear
summary(linear)$adj.r.squared
# a       b 
# 409.06 11.27

## vision angle ----
y <- habitat_amount_variables$total_crossings
x <- habitat_amount_variables$vision_angle
# null
null = lm(y ~ 1)
# linear
linear = lm(y ~ x)
# exponential
exponential = nls(y ~ a + b*log(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2]))
# BIC 
BICtab(null, linear, exponential, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
exponential
R2nls(exponential)
# a       b 
# 347.61 66.79


# graph ----
plot(y ~ x, xlab = "perm", ylab = "aggreg", pch = 20, cex = 0.5)
abline(null, col = "green")
abline(linear, col = "red")
lines(coefficients(exponential)[1] + (coefficients(exponential)[2])*log(1:max(x)), col = "yellow")
lines(coefficients(assintotic)[1] + (coefficients(assintotic)[2])/(1:max(x)), col = "lightblue")
