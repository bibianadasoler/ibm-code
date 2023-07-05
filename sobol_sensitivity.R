library(nlrx)
#Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home")
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk-11")

### Sensitivity analyses - Sobol ----
#### STEP 1: create a nl object -----
netlogopath.mac <- file.path("C:\\Program Files\\NetLogo 6.0.4") 
modelpath.mac <- file.path("C:\\Users\\bibia\\OneDrive\\Doutorado\\Predicao_ferrovias\\ibm-code\\crossings.nlogo")
nl.sobol.random <- nl(nlversion = "6.0.4",
                      nlpath = netlogopath.mac,
                      modelpath = modelpath.mac,
                      jvmmem = 1024)

print(nl.sobol.random)
report_model_parameters(nl.sobol.random)


#### STEP 2 experiment----
nl.sobol.random@experiment <- experiment(expname = "sobol_random500-raw",
                                         outpath = "C:\\Users\\bibia\\OneDrive\\Doutorado\\Predicao_ferrovias\\ibm-code\\results",
                                         repetition = 10,
                                         tickmetrics = "false",
                                         idsetup = "setup",
                                         idgo = "go",
                                         runtime = 500,
                                         metrics = c("(count patches)", 
                                                     "count patches with [habitat = 1]",
                                                     "count patches with [habitat = 2]",
                                                     "mean [hab_neighbors] of patches with [habitat = 0]",
                                                     "sum [visits] of patches with [habitat = 0]",
                                                     "assess-top-sections"),
                                         variables = list("proportion-of-habitat" = list(min = 10, max = 90, qfun = "qunif"),
                                                          "matrix-permeability" = list(min = 0.1, max = 0.9, qfun = "qunif"),
                                                          "perceptual-range" = list(min = 5, max = 20, qfun = "qunif"),
                                                          "vision-angle" = list(min = 90, max = 180, qfun = "qunif")),
                                         constants = list("number-of-individuals" = 100,
                                                          "steps" = 500,
                                                          "scenario" = 8,
                                                          "save-data?" = "true"))
#eval_variables_constants(nl.sobol.random)
print(nl.sobol.random)
#### STEP 3 simulation design----
nl.sobol.random@simdesign <- simdesign_sobol(nl = nl.sobol.random,
                                                 samples = 80,
                                                 sobolorder = 2,
                                                 sobolnboot = 100,
                                                 sobolconf = 0.95,
                                                 nseeds = 1,
                                                 precision = 3)
nl.sobol.random@simdesign 
#nl.sobol.random@simdesign@siminput %>%
#   dplyr::group_by(`proportion-of-habitat`) %>% print(n = 600)
#nl.sobol.random@simdesign@siminput %>%
#   with(., table(`vision-angle`))
hist(nl.sobol.random@simdesign@siminput$`proportion-of-habitat`, breaks = 20)
hist(nl.sobol.random@simdesign@siminput$`matrix-permeability`, breaks = 20)
hist(nl.sobol.random@simdesign@siminput$`perceptual-range`, breaks = 5)
hist(nl.sobol.random@simdesign@siminput$`vision-angle`, xlim = c(90, 180), breaks = c(90, 120, 150, 180))

#### STEP 4 run simulation----
#teste <- run_nl_one(nl.sobol.random, getsim(nl.sobol.random,"simseeds")[1], 2)
print(nl.sobol.random)
library(future)
plan(multisession)
progressr::handlers("progress")
raw.results.sobol.random <- progressr::with_progress(run_nl_all(nl.sobol.random))
#raw.results.sobol.random 

#### STEP 5 ----
setsim(nl.sobol.random, "simoutput") <- raw.results.sobol.random
write_simoutput(nl.sobol.random)
saveRDS(nl.sobol.random, file.path(nl.sobol.random@experiment@outpath, "sobol_random500.rds"))

#further analysis 
# nao foi possivel pois apenas 1 random-seed
#analyze.sobol.random <- analyze_nl(nl.sobol.random)

# analises posteriores
print(sobol_random500@simdesign@simobject[[1]][["X"]]) 
plot(sobol_random500@simdesign@simobject[[1]][["X"]]) # comparacoes 


sobol_random500@simdesign@siminput
sobol_random500@simdesign@simoutput
tell(sobol_random500@simdesign@siminput,
     sobol_random500@simdesign@simoutput)
