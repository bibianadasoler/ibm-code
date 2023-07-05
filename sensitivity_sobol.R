# Sensitivity analysis - Sobol based on ants_nlrx_script
library(nlrx)
library(dplyr)
library(hrbrthemes)

## Step1: Create a nl obejct:
nl_crossings <- nl(nlversion = "6.0.4",
         nlpath = "/Applications/NetLogo 6.0.4",
         modelpath = "/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/crossings.nlogo",
         jvmmem = 1024)
print(nl_crossings)

## Step2: Add Experiment
# Inspect the model available model parameters:
report_model_parameters(nl_crossings)

nl_crossings@experiment <- experiment(expname = "ants_sobol",
                            outpath = "/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/",
                            repetition = 1,      
                            tickmetrics = "false",
                            idsetup = "setup",   
                            idgo = "go",         
                            runtime = 0,
                            stopcond = "ticks = steps",
                            evalticks = NA_integer_,
                            metrics = c("assess-top-sections"),
                            variables = list("proportion-of-habitat" = list(min = 30, max = 70, qfun = "qunif"),
                                             #"matrix-permeability" = list(min = 0.1, max = 0.4, qfun = "qunif"),
                                             "perceptual-range" = list(min = 5, max = 15, qfun = "qunif"),
                                             "vision-angle" = list(min = 90, max = 120, qfun = "qunif")))
eval_variables_constants(nl_crossings)

nl_crossings@simdesign <- simdesign_sobol(nl = nl_crossings,
                                   samples = 2,
                                   sobolorder = 2,
                                   sobolnboot = 100,
                                   sobolconf = 0.95,
                                   nseeds = 2,
                                   precision = 3)

nl_crossings@simdesign
## Run parallel
plan(multisession)
results_crossings <- nlrx::run_nl_all(nl = nl_crossings, split = 2)

# Attach results to nl
setsim(nl_crossings, "simoutput") <- results_crossings

# Store nl object
#saveRDS(nl, "3_Results/Ants_sobol_nl.rds")

# Sobol indices:
sobol.out_bibs <- analyze_nl(nl_crossings)

sobol.out_bibs$facet <- ifelse(sobol.out_bibs$parameter %in% c("proportion-of-habitat", "matrix-permeability", "perceptual-range", "vision-angle"), "main effect", "interaction")
sobol.out_bibs$facet <- factor(sobol.out_bibs$facet, levels = c("main effect", "interaction"))

ggplot(sobol.out_bibs, aes(x=parameter, y=original)) +
  facet_wrap(~facet, scales="free", ncol = 1) +
  coord_flip() +
  geom_boxplot()

sobol.out.agg_bibs <- sobol.out_bibs %>%
  group_by(parameter) %>%
  summarise(original.mu = mean(original),
            original.sd = sd(original)
  ) %>%
  mutate(facet = ifelse((.)$parameter %in% c("proportion-of-habitat", "matrix-permeability", "perceptual-range", "vision-angle"), "main effect", "interaction"),
         original.min =  ifelse(((.)$original.mu - (.)$original.sd) < 0, 0, (.)$original.mu - (.)$original.sd),
         original.max = (.)$original.mu + (.)$original.sd)

ggplot(sobol.out.agg_bibs, aes(x=parameter, y=original.mu)) +
  facet_wrap(~facet, scales="free", ncol=1) +
  coord_flip() +
  geom_linerange(aes(ymin=original.min, ymax=original.max), linewidth =1) +
  geom_point(size=2) +
  theme_ipsum(base_size = 11, axis_text_size = 11, axis_title_size = 11, strip_text_size = 14) +
  theme(panel.spacing = unit(0.5, "lines"))

ggsave("4_Plots/Ants_sobol.png", width = 6.0, height = 4.0, dpi=300)
