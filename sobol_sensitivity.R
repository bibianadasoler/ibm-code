library(nlrx)
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home")

### Sensitivity analyses - Sobol ----
#### STEP 1: create a nl object -----
netlogopath.mac <- file.path("/Applications/NetLogo 6.0.4")
modelpath.mac <- file.path("/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/crossings.nlogo")
outpath.mac <- file.path("/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/results/")
nl.sobol.random <- nl(nlversion = "6.0.4",
                      nlpath = netlogopath.mac,
                      modelpath = modelpath.mac,
                      jvmmem = 1024)

print(nl.sobol.random)
report_model_parameters(nl.sobol.random)


#### STEP 2 experiment----
nl.sobol.random@experiment <- experiment(expname = "sobol.random-raw",
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
                                         variables = list("proportion-of-habitat" = list(min = 10, max = 90, step = 20, qfun = "qunif"),
                                                          "matrix-permeability" = list(min = 0.1, max = 0.9, step = 0.1, qfun = "runif"),
                                                          "perceptual-range" = list(min = 5, max = 20, step = 5, qfun = "runif"),
                                                          "vision-angle" = list(min = 90, max = 180, qfun = "qunif")),
                                         constants = list("number-of-individuals" = 10,
                                                          "steps" = 10,
                                                          "scenario" = 8,
                                                          "save-data?" = "true",
                                                          "output-file" = "sobol_random"))
eval_variables_constants(nl.sobol.random)

#### STEP 3 simulation design----
nl.sobol.random@simdesign <- simdesign_sobol(nl = nl.sobol.random,
                                             samples = 10,
                                             sobolorder = 4,
                                             sobolnboot = 1,
                                             sobolconf = 0.95,
                                             nseeds = 1,
                                             precision = 4)
nl.sobol.random@simdesign@siminput 
teste <- run_nl_one(nl.sobol.random, getsim(nl.sobol.random,"simseeds")[1], 120)
#### STEP 4 run simulation----
library(future)
plan(multisession)
progressr::handlers("progress")
raw.results.sobol.random <- progressr::with_progress(run_nl_all(nl.sobol.random))

raw.results.sobol.random 

#### STEP 5 ----
setsim(nl.sobol.random, "simoutput") <- raw.results.sobol.random
write_simoutput(nl.sobol.random)

#further analysis
analyze.sobol.random <- analyze_nl(nl.sobol.random)
