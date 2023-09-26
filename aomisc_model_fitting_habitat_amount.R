library(aomisc)
library(bbmle)
library(here)

# habitat amount data ----
habitat_amount <- readRDS(here("results", "habitat_amount_simulations.RDS"))
habitat_amount_variables <- habitat_amount@simdesign@simoutput

# crossings aggregation ----
### proportion of habitat ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$proportion_of_habitat
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- nls(Y ~ NLS.linear(X, a, b), data = variables)
polynomial <- nls(Y ~ NLS.poly2(X, a, b, c), data = variables)
exponential_decay <- nls(Y ~ NLS.expoDecay(X, a, k), data = variables)
exponential_growth <- nls(Y ~ NLS.expoGrowth(X, a, k), data = variables)
logarithmic <- nls(Y ~ NLS.logCurve(X, a, b), data = variables)
bragg3 <- nls(Y ~ NLS.bragg.3(X, b, d, e), data = variables)
bragg4 <- nls(Y ~ NLS.bragg.4(X, b, c, d, e), data = variables)

BICtab(null, linear, polynomial, exponential_decay, exponential_growth, 
                logarithmic, bragg3, bragg4, 
                nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)

x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines( coefficients(bragg4)[2] + (coefficients(bragg4)[3] - coefficients(bragg4)[2]) * exp(- coefficients(bragg4)[1] * (x_values - coefficients(bragg4)[4])^2) , col = "purple", lwd = 3)


### matrix permeability ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$matrix_permeability
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- nls(Y ~ NLS.linear(X, a, b), data = variables)
polynomial <- nls(Y ~ NLS.poly2(X, a, b, c), data = variables)
exponential_decay <- nls(Y ~ NLS.expoDecay(X, a, k), data = variables)
exponential_growth <- nls(Y ~ NLS.expoGrowth(X, a, k), data = variables)
logarithmic <- nls(Y ~ NLS.logCurve(X, a, b), data = variables)
#bragg3 <- nls(Y ~ NLS.bragg.3(X, b, d, e), data = variables)
#bragg4 <- nls(Y ~ NLS.bragg.4(X, b, c, d, e), data = variables)

BICtab(null, linear, polynomial, exponential_decay, exponential_growth, 
       logarithmic, #bragg3, bragg4, 
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(polynomial)[1] + (coefficients(polynomial)[2]*x_values) + (coefficients(polynomial)[3]*x_values^2), col = "blue", lwd = 3)


### perceptual range ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$perceptual_range
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- nls(Y ~ NLS.linear(X, a, b), data = variables)
polynomial <- nls(Y ~ NLS.poly2(X, a, b, c), data = variables)
exponential_decay <- nls(Y ~ NLS.expoDecay(X, a, k), data = variables)
exponential_growth <- nls(Y ~ NLS.expoGrowth(X, a, k), data = variables)
logarithmic <- nls(Y ~ NLS.logCurve(X, a, b), data = variables)
bragg3 <- nls(Y ~ NLS.bragg.3(X, b, d, e), data = variables)
#bragg4 <- nls(Y ~ NLS.bragg.4(X, b, c, d, e), data = variables)

BICtab(null, linear, polynomial, exponential_decay, exponential_growth, 
       logarithmic, bragg3, #bragg4, 
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(logarithmic)[1] + (coefficients(logarithmic)[2]*log(x_values)), col = "yellow", lwd = 3)


### vision angle ----
Y <- habitat_amount_variables$assess_top_sections
X <- habitat_amount_variables$vision_angle
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- nls(Y ~ NLS.linear(X, a, b), data = variables)
polynomial <- nls(Y ~ NLS.poly2(X, a, b, c), data = variables)
exponential_decay <- nls(Y ~ NLS.expoDecay(X, a, k), data = variables)
exponential_growth <- nls(Y ~ NLS.expoGrowth(X, a, k), data = variables)
logarithmic <- nls(Y ~ NLS.logCurve(X, a, b), data = variables)
bragg3 <- nls(Y ~ NLS.bragg.3(X, b, d, e), data = variables)
#bragg4 <- nls(Y ~ NLS.bragg.4(X, b, c, d, e), data = variables)

BICtab(null, linear, polynomial, exponential_decay, exponential_growth, 
       logarithmic, bragg3, #bragg4, 
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values), col = "pink", lwd = 3)
lines(coefficients(exponential_decay)[1] * exp(coefficients(exponential_decay)[2] * x_values), col = "lightblue", lwd = 3)


# total crossings ----
### proportion of habitat ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$proportion_of_habitat
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- nls(Y ~ NLS.linear(X, a, b), data = variables)
polynomial <- nls(Y ~ NLS.poly2(X, a, b, c), data = variables)
exponential_decay <- nls(Y ~ NLS.expoDecay(X, a, k), data = variables)
exponential_growth <- nls(Y ~ NLS.expoGrowth(X, a, k), data = variables)
logarithmic <- nls(Y ~ NLS.logCurve(X, a, b), data = variables)
bragg3 <- nls(Y ~ NLS.bragg.3(X, b, d, e), data = variables)
bragg4 <- nls(Y ~ NLS.bragg.4(X, b, c, d, e), data = variables)

BICtab(null, linear, polynomial, exponential_decay, exponential_growth, 
       logarithmic, bragg3, bragg4, 
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines( coefficients(bragg4)[2] + (coefficients(bragg4)[3] - coefficients(bragg4)[2]) * exp(- coefficients(bragg4)[1] * (x_values - coefficients(bragg4)[4])^2) , col = "purple", lwd = 3)


### matrix permeability ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$matrix_permeability
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- nls(Y ~ NLS.linear(X, a, b), data = variables)
polynomial <- nls(Y ~ NLS.poly2(X, a, b, c), data = variables)
exponential_decay <- nls(Y ~ NLS.expoDecay(X, a, k), data = variables)
exponential_growth <- nls(Y ~ NLS.expoGrowth(X, a, k), data = variables)
logarithmic <- nls(Y ~ NLS.logCurve(X, a, b), data = variables)
bragg3 <- nls(Y ~ NLS.bragg.3(X, b, d, e), data = variables)
#bragg4 <- nls(Y ~ NLS.bragg.4(X, b, c, d, e), data = variables)

BICtab(null, linear, polynomial, exponential_decay, exponential_growth, 
       logarithmic, bragg3, #bragg4, 
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(polynomial)[1] + (coefficients(polynomial)[2]*x_values) + (coefficients(polynomial)[3]*x_values^2), col = "blue", lwd = 3)
lines( coefficients(bragg3)[2] * exp(- coefficients(bragg3)[1] * (x_values - coefficients(bragg3)[3])^2), col = "darkgreen", lwd = 3)
lines(coefficients(logarithmic)[1] + (coefficients(logarithmic)[2]*log(x_values)), col = "yellow", lwd = 3)

### perceptual range ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$perceptual_range
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- nls(Y ~ NLS.linear(X, a, b), data = variables)
polynomial <- nls(Y ~ NLS.poly2(X, a, b, c), data = variables)
exponential_decay <- nls(Y ~ NLS.expoDecay(X, a, k), data = variables)
exponential_growth <- nls(Y ~ NLS.expoGrowth(X, a, k), data = variables)
logarithmic <- nls(Y ~ NLS.logCurve(X, a, b), data = variables)
#bragg3 <- nls(Y ~ NLS.bragg.3(X, b, d, e), data = variables)
bragg4 <- nls(Y ~ NLS.bragg.4(X, b, c, d, e), data = variables)

BICtab(null, linear, polynomial, exponential_decay, exponential_growth, 
       logarithmic, bragg4, #bragg3, 
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines( coefficients(bragg4)[2] + (coefficients(bragg4)[3] - coefficients(bragg4)[2]) * exp(- coefficients(bragg4)[1] * (x_values - coefficients(bragg4)[4])^2) , col = "purple", lwd = 3)


### vision angle ----
Y <- habitat_amount_variables$total_crossings
X <- habitat_amount_variables$vision_angle
variables <- data.frame(Y, X)

null <- lm(Y ~ 1)
linear <- nls(Y ~ NLS.linear(X, a, b), data = variables)
polynomial <- nls(Y ~ NLS.poly2(X, a, b, c), data = variables)
exponential_decay <- nls(Y ~ NLS.expoDecay(X, a, k), data = variables)
exponential_growth <- nls(Y ~ NLS.expoGrowth(X, a, k), data = variables)
logarithmic <- nls(Y ~ NLS.logCurve(X, a, b), data = variables)
bragg3 <- nls(Y ~ NLS.bragg.3(X, b, d, e), data = variables)
bragg4 <- nls(Y ~ NLS.bragg.4(X, b, c, d, e), data = variables)

BICtab(null, linear, polynomial, exponential_decay, exponential_growth, 
       logarithmic, bragg3, bragg4, 
       nobs = 100, weights = TRUE, delta = TRUE, base = TRUE)
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
lines(coefficients(polynomial)[1] + (coefficients(polynomial)[2]*x_values) + (coefficients(polynomial)[3]*x_values^2), col = "blue", lwd = 3)
lines( coefficients(bragg3)[2] * exp(- coefficients(bragg3)[1] * (x_values - coefficients(bragg3)[3])^2), col = "darkgreen", lwd = 3)


# all graphs ----
x_values <- 1:max(X) 
plot(Y ~ X, pch = 20, cex = 0.5)
abline(null, col = "green", lwd = 3)
abline(linear, col = "red", lwd = 3)
lines(coefficients(logarithmic)[1] + (coefficients(logarithmic)[2]*log(x_values)), col = "yellow", lwd = 3)
lines(coefficients(exponential_growth)[1] * exp(coefficients(exponential_growth)[2] * x_values), col = "pink", lwd = 3)
lines(coefficients(exponential_decay)[1] * exp(coefficients(exponential_decay)[2] * x_values), col = "lightblue", lwd = 3)
lines(coefficients(polynomial)[1] + (coefficients(polynomial)[2]*x_values) + (coefficients(polynomial)[3]*x_values^2), col = "blue", lwd = 3)
lines( coefficients(bragg3)[2] * exp(- coefficients(bragg3)[1] * (x_values - coefficients(bragg3)[3])^2), col = "darkgreen", lwd = 3)
lines( coefficients(bragg4)[2] + (coefficients(bragg4)[3] - coefficients(bragg4)[2]) * exp(- coefficients(bragg4)[1] * (x_values - coefficients(bragg4)[4])^2) , col = "purple", lwd = 3)

