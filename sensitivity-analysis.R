library(nlrx)
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home")


### Full-factorial simulation ----
#### STEP 1: create a nl object -----
# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:\\Program Files\\NetLogo 6.3.0")
modelpath <- file.path("C:\\Users\\bibia\\OneDrive\\Doutorado\\Predicao_ferrovias\\ibm-code\\crossings.nlogo")
outpath <- file.path("C:\\Users\\bibia\\OneDrive\\Doutorado\\Predicao_ferrovias\\ibm-code\\results")
nl <- nl(nlversion = "6.3.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

# Macbook
netlogopath.mac <- file.path("/Applications/NetLogo 6.0.4")
modelpath.mac <- file.path("/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/crossings.nlogo")
outpath.mac <- file.path("/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/results")
nl.regular <- nl(nlversion = "6.0.4",
         nlpath = netlogopath.mac,
         modelpath = modelpath.mac,
         jvmmem = 1024)

print(nl.regular)
report_model_parameters(nl.regular)

nl.random <- nl(nlversion = "6.0.4",
                 nlpath = netlogopath.mac,
                 modelpath = modelpath.mac,
                 jvmmem = 1024)

print(nl.random)
report_model_parameters(nl.random)
#### STEP 2: attach an experiment ----
# in this example we want to calculate sensitivity for 12 outputs
# we varied all numeric model parameters to estimate their sensitivity on the output
# we define parameter ranges and distribution functions for all parameters
# runtime of 100 ticks and measure our metrics in the end

nl.regular@experiment <- experiment(expname = "regular-raw",
                            outpath = outpath.mac,
                            repetition = 2,   
                            tickmetrics = "false",
                            idsetup = "setup",  
                            idgo = "go",        
                            stopcond = "ticks = steps",
                            metrics = c("(count patches)", 
                                        "count patches with [habitat = 1]",
                                        "count patches with [habitat = 2]",
                                        "mean [hab_neighbors] of patches with [habitat = 0]",
                                        "sum [visits] of patches with [habitat = 0]",
                                        "assess-top-sections"),
                            variables = list("scenario" = list(min = 1, max = 7,  step = 1),
                                             "matrix-permeability" = list(min = 0.1, max = 0.9, step = 0.3),
                                             "perceptual-range" = list(min = 5, max = 20, step = 5),
                                             "vision-angle" = list(min = 90, max = 180, step = 30)),
                            constants = list("number-of-individuals" = 10,
                                             "steps" = 100,
                                             "proportion-of-habitat" = 50))
eval_variables_constants(nl.regular)

#
nl.random@experiment <- experiment(expname = "random-raw",
                                    outpath = outpath.mac,
                                    repetition = 2,   
                                    tickmetrics = "false",
                                    idsetup = "setup",  
                                    idgo = "go",        
                                    stopcond = "ticks = steps",
                                    metrics = c("(count patches)", 
                                                "count patches with [habitat = 1]",
                                                "count patches with [habitat = 2]",
                                                "mean [hab_neighbors] of patches with [habitat = 0]"),
                                   metrics.patches = c("sum [visits] of patches with [habitat = 0]",
                                                      "assess-top-sections"),
                                    variables = list("proportion-of-habitat" = list(min = 10, max = 90,  step = 20),
                                                     "matrix-permeability" = list(min = 0.1, max = 0.9, step = 0.3),
                                                     "perceptual-range" = list(min = 5, max = 20, step = 5),
                                                     "vision-angle" = list(min = 90, max = 180, step = 30)),
                                    constants = list("number-of-individuals" = 10,
                                                     "steps" = 100,
                                                     "scenario" = 8))
eval_variables_constants(nl.random)
report_model_parameters(nl.random)


#### STEP 3: attach a simulation design ----
nl.regular@simdesign <- simdesign_ff(nl = nl.regular,
                             nseeds = 5)
nl.regular@simdesign

nl.random@simdesign <- simdesign_ff(nl = nl.random,
                                     nseeds = 5)
nl.random@simdesign@siminput %>% print(n = 300)

teste <-run_nl_one(nl.random, getsim(nl.random, "simseeds")[1], 120)
#### STEP 4: run simulations ----
library(future)
plan(multisession)
progressr::handlers("progress")
raw.results.regular <- progressr::with_progress(run_nl_all(nl.regular))

raw.results.random <- progressr::with_progress(run_nl_all(nl.random))

#### STEP 5: investigate output ----
# attach results to nl and run analysis
setsim(nl.regular, "simoutput") <- raw.results.regular
write_simoutput(nl.regular)

setsim(nl.random, "simoutput") <- raw.results.random
write_simoutput(nl.random)

#further analysis
analyze.regular <- analyze_nl(nl.regular)

analyze.random <- analyze_nl(nl.random)

### End of full factorial ----


### Morris screening ----
nl.morris <- nl(nlversion = "6.0.4",
                 nlpath = netlogopath.mac,
                 modelpath = modelpath.mac,
                 jvmmem = 1024)

nl.morris@experiment <- experiment(expname = "crossings-raw",
                                  outpath = outpath.mac,
                                  repetition = 1,
                                  tickmetrics = "false",
                                  idsetup = "setup",
                                  idgo = "go",
                                  stopcond = "ticks = steps",
                                  metrics = c("(count patches)", 
                                    "count patches with [habitat = 1]",
                                    "count patches with [habitat = 2]",
                                    "mean [hab_neighbors] of patches with [habitat = 0]",
                                    "sum [visits] of patches with [habitat = 0]",
                                    "assess-top-sections"),
                                  variables = list("proportion-of-habitat" = list(min = 10, max = 100, step = 10, qfun = "qunif"),
                                                   "matrix-permeability" = list(min = 0.1, max = 1, step = 0.1, qfun = "qunif"),
                                                   "perceptual-range" = list(min = 5, max = 20, step = 5, qfun = "qunif"),
                                                   "vision-angle" = list(min = 90, max = 180, step = 30, qfun = "qunif")),
                                  constants = list("number-of-individuals" = 10,
                                                   "steps" = 10,
                                                   "scenario" = 8,
                                                   "matrix-permeability" = list(min = 0.1, max = 1, step = 0.1, qfun = "qunif"),
                                                   "perceptual-range" = list(min = 5, max = 20, step = 5, qfun = "qunif"),
                                                   "vision-angle" =))



### End of morris ----












#### outros exemplos tutorial nlrx ----
library(tidyverse)
input <- getsim(nl.sensi, "siminput") %>%    # Take input parameter matrix
  dplyr::select(names(getexp(nl.sensi, "variables"))) %>%  # Select variable parameters only
  dplyr::rename_all(~str_replace_all(., c("-" = "_", "\\s+" = "_"))) # Remove - and space characters.

output <- getsim(nl.sensi, "simoutput") %>%   # Take simulation output
  dplyr::group_by(`random-seed`, siminputrow) %>% # Group by random seed and siminputrow
  dplyr::summarise_at(getexp(nl.sensi, "metrics"), list(mean=mean, sd=sd)) %>% # Aggregate output
  dplyr::ungroup() %>%  # Ungroup
  dplyr::select(-`random-seed`, -siminputrow, -`(count patches)_mean`, 
                -`count patches with [habitat = 1]_mean`, 
                -`count patches with [habitat = 2]_mean`,
                #-`number-of-individuals`, -steps, -scenario
                -`(count patches)_sd`, 
                -`count patches with [habitat = 1]_sd`, 
                -`count patches with [habitat = 2]_sd`,
                -`mean [hab_neighbors] of patches with [habitat = 0]_sd`, 
                -`sum [visits] of patches with [habitat = 0]_sd`,
                -`assess-top-sections_sd`) %>%
  dplyr::rename_all(~str_replace_all(., c("-" = "", "\\s+" = "_", "\\[" = "", "\\]" = "", "=" = "", "\\(" = "", "\\)" = "", "__" = "_"))) # Remove - and space characters.

# Perform pcc and src for each output separately (map)
pcc.result <- purrr::map(names(output), function(x) sensitivity::pcc(X=input, y=output[,x], nboot = 100, rank = FALSE)) 
src.result <- purrr::map(names(output), function(x) sensitivity::src(X=input, y=output[,x], nboot = 100, rank = FALSE)) 