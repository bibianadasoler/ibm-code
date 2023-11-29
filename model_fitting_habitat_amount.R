<<<<<<< HEAD
##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## Script to evaluated the shape of the response curve between inputs and outputs 
## by conducting a model selection procedure considering the following candidate 
## models: null (intercept-only), linear, exponential, asymptotic, and Bragg (ie., unimodal response)
## Model: Habitat amount
#

# loading packages
library(aomisc)
library(bbmle)

# load file with simulations information
habitat_amount <- readRDS(here::here("results", "habitat_amount_simulations.RDS"))
habitat_amount_variables <- habitat_amount@simdesign@simoutput

# Crossings aggregation output ----
### Proportion of habitat ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$proportion_of_habitat
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
                nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(bragg4)

### Matrix permeability ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$matrix_permeability
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)

### Perceptual range ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$perceptual_range
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)

### Vision angle ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$vision_angle
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(exponential_growth)

# Total crossings output ----
### Proportion of habitat ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$proportion_of_habitat
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(bragg4)

### Matrix permeability ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$matrix_permeability
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)

### Perceptual range ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$perceptual_range
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(bragg4)

### Vision angle ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$vision_angle
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)
=======
##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## Script to evaluated the shape of the response curve between inputs and outputs 
## by conducting a model selection procedure considering the following candidate 
## models: null (intercept-only), linear, exponential, asymptotic, and Bragg (ie., unimodal response)
## Model: Habitat amount
#

# loading packages
library(aomisc)
library(bbmle)

# load file with simulations information
habitat_amount <- readRDS(here::here("results", "habitat_amount_simulations.RDS"))
habitat_amount_variables <- habitat_amount@simdesign@simoutput

# Crossings aggregation output ----
### Proportion of habitat ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$proportion_of_habitat
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
                nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(bragg4)

### Matrix permeability ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$matrix_permeability
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)

### Perceptual range ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$perceptual_range
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)

### Vision angle ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$vision_angle
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(exponential_growth)

# Total crossings output ----
### Proportion of habitat ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$proportion_of_habitat
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(bragg4)

### Matrix permeability ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$matrix_permeability
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)

### Perceptual range ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$perceptual_range
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(bragg4)

### Vision angle ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$vision_angle
variables <- data.frame(Y, X)

# candidate models
null <- lm(Y ~ 1)
linear <- drm(Y ~ X, fct = DRC.linear(), data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

# model selection
BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
# r2
R2nls(asymptotic)
>>>>>>> a6440e5850e1344c6b7da7ca8ec79541993a403e
