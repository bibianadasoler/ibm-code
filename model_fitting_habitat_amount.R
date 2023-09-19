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
habitat_amount_simulations <- readRDS(here("results", "habitat_amount_6000amostras1.RDS"))
habitat_amount_variables <- read.csv("/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/results/habitat_5mais5.csv") #output_FINAL #habitat_amount_simulations@simdesign@simoutput

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
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
exponential 
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
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
# BIC 
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
exponential
R2nls(exponential)
# a       b 
# 0.77566 -0.08088 

### perceptual range ----
y <- habitat_amount_variables$assess_top_sections
x <- habitat_amount_variables$perceptual_range
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
linear
summary(linear)$adj.r.squared
# a       b 
# 0.389393 0.003446

### vision angle ----
y <- habitat_amount_variables$assess_top_sections
x <- habitat_amount_variables$vision_angle
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
linear
summary(linear)$adj.r.squared
# a       b 
# 0.281138 0.001573


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
# assintotic
assintotic = nls(y ~ a + b/(x), start = list(a = coefficients(linear)[1], b = coefficients(linear)[2])) 
# BIC 
BICtab(null, linear, exponential, assintotic, nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
assintotic
R2nls(assintotic)
# a       b 
# 901.1 -940.0 

### matrix permeability ----
y <- habitat_amount_variables$total_crossings
x <- habitat_amount_variables$matrix_permeability
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
linear
R2nls(exponential)
# a       b 
# 966.012 -1.814 

## perceptual range ----
y <- habitat_amount_variables$total_crossings
x <- habitat_amount_variables$perceptual_range
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
linear
summary(linear)$adj.r.squared
# a       b 
# 616.39 11.02

## vision angle ----
y <- habitat_amount_variables$total_crossings
x <- habitat_amount_variables$vision_angle
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
# -604.0 302.8


# graph ----
plot(y ~ x, xlab = "perm", ylab = "aggreg", pch = 20, cex = 0.5)
abline(null, col = "green")
abline(linear, col = "red")
lines(coefficients(exponential)[1] + (coefficients(exponential)[2])*log(1:max(x)), col = "yellow")
lines(coefficients(assintotic)[1] + (coefficients(assintotic)[2])/(1:max(x)), col = "lightblue")
