##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## Script to evaluated the shape of the response curve between inputs and outputs 
## by conducting a model selection procedure considering the following candidate 
## models: null (intercept-only), linear, exponential, asymptotic, and Bragg (ie., unimodal response)
## Model: Habitat arrangement
#

# loading packages
library(aomisc)
library(bbmle)

# load file with simulations information
habitat_arrangement <- readRDS(here::here("results", "habitat_arrangement_simulations.RDS"))
habitat_arrangement_variables <- habitat_arrangement@simdesign@simoutput

# Crossings aggregation output ----
### Matrix permeability ----
Y <- habitat_arrangement_variables$assess_top_sections
X <- habitat_arrangement_variables$matrix_permeability
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)

### Perceptual range ----
Y <- habitat_arrangement_variables$assess_top_sections
X <- habitat_arrangement_variables$perceptual_range
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(exponential_growth)

### vision angle ----
Y <- habitat_arrangement_variables$assess_top_sections
X <- habitat_arrangement_variables$vision_angle
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
summary(linear)
R2nls(exponential_growth)

# Total crossings output ----
### Matrix permeability ----
Y <- habitat_arrangement_variables$total_crossings
X <- habitat_arrangement_variables$matrix_permeability
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)

# rs
R2nls(asymptotic)
R2nls(exponential_growth)

### Perceptual range ----
Y <- habitat_arrangement_variables$total_crossings
X <- habitat_arrangement_variables$perceptual_range
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(exponential_growth)

### Vision angle ----
Y <- habitat_arrangement_variables$total_crossings
X <- habitat_arrangement_variables$vision_angle
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)

# r2
R2nls(asymptotic)

