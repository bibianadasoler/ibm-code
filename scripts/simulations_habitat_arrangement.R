##
## Article: Wildlife road crossings are not everywhere: a theoretical approach 
## for maximizing mitigation
## doi: 
#
## Script to run and save model simulations on NetLogo
## Model: Habitat arrangement
#
## The NetLogo code used here ("crossings_simulations.nlogo") does the same than the 
## "crossings_original.nlogo" available in the folder. However, the latter has the 
## variables information wrote in a format not supported in sensitivity analysis.
## Because of this, here we used the code "crossings_simulations.nlogo" to perform the simulations.
#
## We used the "nlrx" to sample parameters according to "sobol2007" function needs 
#


## Create a netlogo object
# indicate netlogo program path
nl_folder <- file.path("C:/Program Files/NetLogo 6.0.4")

# indicate netlogo code and other informations
nl_crossings <- nlrx::nl(nlversion = "6.0.4",
                         nlpath = nl_folder,
                         modelpath = here::here("netlogo_code" , "crossings_simulations.nlogo"),
                         jvmmem = 1024)
print(nl_crossings)

## Add parameters values to an experiment 
# inspect the model available model parameters:
nlrx::report_model_parameters(nl_crossings)

# define model paremeters values
nl_crossings@experiment <- nlrx::experiment(expname = "sobol2007",
                                            outpath = here::here(),
                                            repetition = 1,      
                                            tickmetrics = "false",
                                            idsetup = "setup",   
                                            idgo = "go",         
                                            runtime = 0,
                                            stopcond = "ticks = steps",
                                            evalticks = NA_integer_,
                                            metrics = c("total_crossings",
                                                        "assess_top_sections"),
                                            variables = list("matrix_permeability" = list(min = 10, max = 90, qfun = "qunif"),
                                                             "perceptual_range" = list(min = 5, max = 42, qfun = "qunif"),
                                                             "vision_angle" = list(min = 90, max = 180, qfun = "qunif"),
                                                             "scenario" = list(min = 1, max = 7, qfun = "qunif")))
# inspect parameters that will not vary
nlrx::eval_variables_constants(nl_crossings)

## Define a sensitivity design to create the sample of the parameters 
nl_crossings@simdesign <- nlrx::simdesign_sobol2007(nl = nl_crossings,
                                                    samples = 10000,
                                                    sobolnboot = 300,
                                                    sobolconf = 0.95,
                                                    nseeds = 1,
                                                    precision = 0)
nl_crossings@simdesign

# inspect the netlogo object which will have parameters values to run the simulations
print(nl_crossings)

## Run and save simulations
# to run in parallel
plan(multisession)
progressr::handlers("progress")

# run all simulations
results_crossings <- progressr::with_progress(nlrx::run_nl_all(nl = nl_crossings, split = 6))

# attach results of the simulations to netlogo object
setsim(nl_crossings, "simoutput") <- results_crossings

# save object
saveRDS(nl_crossings, here::here("results", "habitat_arrangement_simulations.rds"))

## end 
=======
  ##
  ## Article: Wildlife road crossings are not everywhere: a theoretical approach 
  ## for maximizing mitigation
  ## doi: 
  #
  ## Script to run and save model simulations on NetLogo
  ## Model: Habitat arrangement
  #
  ## The NetLogo code used here ("crossings.nlogo") does the same than the 
  ## "crossings_original.nlogo" available in the folder. However, the latter has the 
  ## variables information wrote in a format not supported in sensitivity analysis.
## Because of this, here we used the code "crossings.nlogo" to perform the simulations.
#
## We used the "nlrx" to sample parameters according to "sobol2007" function needs 
#


## Create a netlogo object
# indicate netlogo program path
nl_folder <- file.path("C:/Program Files/NetLogo 6.0.4")

# indicate netlogo code and other informations
nl_crossings <- nlrx::nl(nlversion = "6.0.4",
                         nlpath = nl_folder,
                         modelpath = here::here("crossings.nlogo"),
                         jvmmem = 1024)
print(nl_crossings)

## Add parameters values to an experiment 
# inspect the model available model parameters:
nlrx::report_model_parameters(nl_crossings)

# define model paremeters values
nl_crossings@experiment <- nlrx::experiment(expname = "sobol2007",
                                            outpath = here::here(),
                                            repetition = 1,      
                                            tickmetrics = "false",
                                            idsetup = "setup",   
                                            idgo = "go",         
                                            runtime = 0,
                                            stopcond = "ticks = steps",
                                            evalticks = NA_integer_,
                                            metrics = c("total_crossings",
                                                        "assess_top_sections"),
                                            variables = list("matrix_permeability" = list(min = 10, max = 90, qfun = "qunif"),
                                                             "perceptual_range" = list(min = 5, max = 42, qfun = "qunif"),
                                                             "vision_angle" = list(min = 90, max = 180, qfun = "qunif"),
                                                             "scenario" = list(min = 1, max = 7, qfun = "qunif")))
# inspect parameters that will not vary
nlrx::eval_variables_constants(nl_crossings)

## Define a sensitivity design to create the sample of the parameters 
nl_crossings@simdesign <- nlrx::simdesign_sobol2007(nl = nl_crossings,
                                                    samples = 10000,
                                                    sobolnboot = 300,
                                                    sobolconf = 0.95,
                                                    nseeds = 1,
                                                    precision = 0)
nl_crossings@simdesign

# inspect the netlogo object which will have parameters values to run the simulations
print(nl_crossings)

## Run and save simulations
# to run in parallel
plan(multisession)
progressr::handlers("progress")

# run all simulations
results_crossings <- progressr::with_progress(nlrx::run_nl_all(nl = nl_crossings, split = 6))

# attach results of the simulations to netlogo object
setsim(nl_crossings, "simoutput") <- results_crossings

# save object
saveRDS(nl_crossings, here::here("results", "habitat_arrangement_simulations.rds"))

## end 
>>>>>>> a6440e5850e1344c6b7da7ca8ec79541993a403e
