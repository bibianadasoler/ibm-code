library(aomisc)
library(bbmle)
library(here)

# habitat amount data ----
configuration <- readRDS(here("results", "configuration_simulations.RDS"))
configuration_variables <- configuration@simdesign@simoutput

# crossings aggregation ----
### matrix permeability ----
Y <- configuration_variables$assess_top_sections
X <- configuration_variables$matrix_permeability
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
R2nls(asymptotic)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values), col = "yellow", lwd = 3)


### perceptual range ----
Y <- configuration_variables$assess_top_sections
X <- configuration_variables$perceptual_range
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
R2nls(exponential_growth)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values), col = "pink", lwd = 3)

### vision angle ----
Y <- configuration_variables$assess_top_sections
X <- configuration_variables$vision_angle
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
summary(linear)
R2nls(exponential_growth)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
abline(linear, col = "red", lwd = 3)
lines(coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values), col = "pink", lwd = 3)


# total crossings ----
### matrix permeability ----
Y <- configuration_variables$total_crossings
X <- configuration_variables$matrix_permeability
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
R2nls(asymptotic)
R2nls(exponential_growth)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values), col = "yellow", lwd = 3)
lines(coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values), col = "pink", lwd = 3)

### perceptual range ----
Y <- configuration_variables$total_crossings
X <- configuration_variables$perceptual_range
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
R2nls(exponential_growth)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values), col = "pink", lwd = 3)


### vision angle ----
Y <- configuration_variables$total_crossings
X <- configuration_variables$vision_angle
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- lm(Y ~ X, data = variables)
exponential_growth <- drm(Y ~ X, fct = DRC.expoGrowth(), data = variables)
asymptotic <- drm(Y ~ X, fct = DRC.asymReg(), data = variables)
bragg4 <- drm(Y ~ X, fct = DRC.bragg.4(), data = variables)

BICtab(null, linear, exponential_growth, asymptotic, bragg4,
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
R2nls(asymptotic)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values), col = "yellow", lwd = 3)


# all plots ----
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
abline(null, col = "green", lwd = 3)
abline(linear, col = "red", lwd = 3)
lines(coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values), col = "pink", lwd = 3)
lines(coefficients(bragg4)[2] + (coefficients(bragg4)[3] - coefficients(bragg4)[2]) * exp(- coefficients(bragg4)[1] * (x_values - coefficients(bragg4)[4])^2) , col = "purple", lwd = 3)
lines(coefficients(asymptotic)[3] - (coefficients(asymptotic)[3] - coefficients(asymptotic)[1]) * exp (- coefficients(asymptotic)[2] * x_values), col = "yellow", lwd = 3)

# asymptotic a - (a - b) * (exp(-c * X); a = plateau; b = init; c = m
