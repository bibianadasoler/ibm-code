#
#
#
### script para rodar as simulacoes no netlogo
### CENARIOS FIXOS
#
#
#


# Sensitivity analysis - Sobol based on ants_nlrx_script
library(nlrx)
library(dplyr)
library(future)
library(here)

## Step1: Create a nl obejct:
nl_folder <- file.path("/Applications/NetLogo 6.0.4")
nl_crossings <- nl(nlversion = "6.0.4",
                   nlpath = nl_folder,
                   modelpath = "/Users/bibianaterra/Desktop/ff.nlogo",
                   jvmmem = 1024)
print(nl_crossings)

## Step2: Add Experiment
# Inspect the model available model parameters:
report_model_parameters(nl_crossings)

nl_crossings@experiment <- experiment(expname = "sobol2007_cenarios_fixos",
                                      outpath = here(),
                                      repetition = 1,      
                                      tickmetrics = "false",
                                      idsetup = "setup",   
                                      idgo = "go",         
                                      runtime = 0,
                                      stopcond = "ticks = steps",
                                      evalticks = NA_integer_,
                                      metrics = c("total_crossings",
                                                  "assess_top_sections"),
                                      variables = list("matrix_permeability" = list(min = 0.1, max = 0.9, qfun = "qunif"),
                                                       "perceptual_range" = list(min = 5, max = 42, qfun = "qunif"),
                                                       "vision_angle" = list(min = 90, max = 180, qfun = "qunif")),
                                      constants = list("scenario" =  c(1, 2, 3, 4, 5, 6, 7)))

eval_variables_constants(nl_crossings)

nl_crossings@simdesign <- simdesign_sobol(nl = nl_crossings,
                                          samples = 3500,
                                          sobolorder = 1,
                                          sobolnboot = 300,
                                          sobolconf = 0.95,
                                          nseeds = 1,
                                          precision = 3)

nl_crossings@simdesign
hist(nl_crossings@simdesign@siminput$matrix_permeability)
print(nl_crossings)

## Run parallel
plan(multisession)
progressr::handlers("progress")
results_crossings <- progressr::with_progress(nlrx::run_nl_all(nl = nl_crossings))

# Attach results to nl
setsim(nl_crossings, "simoutput") <- results_crossings

# Store nl object
saveRDS(nl_crossings, "sobol_fixos.rds")
# 
