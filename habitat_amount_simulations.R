#
#
#
### script para rodar as simulacoes no netlogo
### submodel habitat amount
#
#
#

# Sensitivity analysis - Sobol 2007
library(nlrx)
library(dplyr)
library(future)
library(here)

## Step1: Create a nl obejct:
nl_folder <- file.path("C:/Program Files/NetLogo 6.0.4")
nl_crossings <- nl(nlversion = "6.0.4",
         nlpath = nl_folder,
         modelpath = here("crossings.nlogo"),
         jvmmem = 1024)
print(nl_crossings)

## Step2: Add Experiment
# Inspect the model available model parameters:
report_model_parameters(nl_crossings)

nl_crossings@experiment <- experiment(expname = "sobol2007",
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
                            variables = list("proportion_of_habitat" = list(min = 10, max = 90, qfun = "qunif"),
                                             "matrix_permeability" = list(min = 10, max = 90, qfun = "qunif"),
                                             "perceptual_range" = list(min = 5, max = 42, qfun = "qunif"),
                                             "vision_angle" = list(min = 90, max = 180, qfun = "qunif")))
eval_variables_constants(nl_crossings)

nl_crossings@simdesign <- simdesign_sobol2007(nl = nl_crossings,
                                   samples = 10000,
                                   sobolnboot = 300,
                                   sobolconf = 0.95,
                                   nseeds = 1,
                                   precision = 0)

nl_crossings@simdesign
hist(nl_crossings@simdesign@siminput$proportion_of_habitat)
hist(nl_crossings@simdesign@siminput$matrix_permeability)
print(nl_crossings)

## Run parallel
plan(multisession, workers = 6)
progressr::handlers("progress")
results_crossings <- progressr::with_progress(nlrx::run_nl_all(nl = nl_crossings, split = 6))

# Attach results to nl
setsim(nl_crossings, "simoutput") <- results_crossings

# Store nl object
saveRDS(nl_crossings, here("results", "sobol2007_7000.rds"))
# 
