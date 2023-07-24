#
#
#
### script para rodar as simulacoes no netlogo
#
#
#


# Sensitivity analysis - Sobol based on ants_nlrx_script
library(nlrx)
library(dplyr)
library(hrbrthemes)
library(future)
library(ggplot2)
library(here)

## Step1: Create a nl obejct:
nl_folder <- file.path("C:/Program Files/NetLogo 6.0.4")
nl_crossings <- nl(nlversion = "6.0.4",
         nlpath = nl_folder,
         modelpath = here("crossings_atualizadanerf.nlogo"),
         jvmmem = 1024)
print(nl_crossings)

## Step2: Add Experiment
# Inspect the model available model parameters:
report_model_parameters(nl_crossings)

nl_crossings@experiment <- experiment(expname = "sobol_scenario8",
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
                                             "matrix_permeability" = list(min = 0.1, max = 0.9, qfun = "qunif"),
                                             "perceptual_range" = list(min = 5, max = 20, qfun = "qunif"),
                                             "vision_angle" = list(min = 90, max = 180, qfun = "qunif")))
eval_variables_constants(nl_crossings)

nl_crossings@simdesign <- simdesign_sobol(nl = nl_crossings,
                                   samples = 3000,
                                   sobolorder = 2,
                                   sobolnboot = 1000,
                                   sobolconf = 0.95,
                                   nseeds = 1,
                                   precision = 3)

nl_crossings@simdesign
hist(nl_crossings@simdesign@siminput$proportion_of_habitat)
hist(nl_crossings@simdesign@siminput$matrix_permeability)
print(nl_crossings)

## Run parallel
plan(multisession)
progressr::handlers("progress")
results_crossings <- progressr::with_progress(nlrx::run_nl_all(nl = nl_crossings))

### ATENCAO AO NUMERO DE SEEDS
# seed1 <- getsim(nl_crossings, "simseeds")[1]
# seed2 <- getsim(nl_crossings, "simseeds")[2]
# seed3 <- getsim(nl_crossings, "simseeds")[3]
# seed4 <- getsim(nl_crossings, "simseeds")[4]
# seed5 <- getsim(nl_crossings, "simseeds")[5]
# 
# corrected_seed <- results_crossings %>%
#  mutate(`random-seed` = case_when((row_number() <= 4400) ~ seed1,
#                                   (row_number() > 4400 & row_number() <= 8800) ~ seed2,
#                                   (row_number() > 8800 & row_number() <= 13200) ~ seed3,
#                                   (row_number() > 13200 & row_number() <= 17600) ~ seed4,
#                                   TRUE ~ seed5)) %>%
#   select(-`[step]`)

# Attach results to nl
setsim(nl_crossings, "simoutput") <- results_crossings

# Store nl object
saveRDS(nl_crossings, here("results", "amostras3000.rds"))
# 
# # Sobol indices:
# sobol.out<- analyze_nl(nl_crossings)
# saveRDS(sobol.out, here("results", "sobol_indices.rds"))
# 
# #### graficos ----
# sobol.out$facet <- ifelse(sobol.out$parameter %in% c("proportion_of_habitat", "matrix_permeability", "perceptual_range", "vision_angle"), "main effect", "interaction")
# sobol.out$facet <- factor(sobol.out$facet, levels = c("main effect", "interaction"))
# 
# ggplot(sobol.out, aes(x = parameter, y = original)) +
#   facet_wrap(~facet, scales="free", ncol = 1) +
#   coord_flip() +
#   geom_boxplot()
# 
# sobol.out_assess <- sobol.out %>%
#   filter(metric == "assess_top_sections_mean")
# ggplot(sobol.out_assess, aes(x = parameter, y = original)) +
#   facet_wrap(~facet, scales="free", ncol = 1) +
#   coord_flip() +
#   geom_boxplot()
# 
# sobol.out_crossings <- sobol.out %>%
#   filter(metric ==  "sum [visits] of patches with [habitat = 0]_mean")
# ggplot(sobol.out_crossings, aes(x = parameter, y = original)) +
#   facet_wrap(~facet, scales="free", ncol = 1) +
#   coord_flip() +
#   geom_boxplot()
# 
# 
# ggplot(sobol.out, aes(x = parameter, y = original)) +
#   facet_wrap(~facet, scales="free", ncol = 1) +
#   coord_flip() +
#   geom_boxplot()
# 
# sobol.out.agg <- sobol.out %>%
#   group_by(parameter, metric) %>%
#   summarise(original.mu = mean(original),
#             original.sd = sd(original)) %>%
#   mutate(original.min =  ifelse(((.)$original.mu - (.)$original.sd) < 0, 0, (.)$original.mu - (.)$original.sd),
#          original.max = (.)$original.mu + (.)$original.sd)
# 
# sobol.out.agg_assess <- sobol.out %>%
#   filter(metric == "assess_top_sections_mean") %>%
#   group_by(parameter) %>%
#   summarise(original.mu = mean(original),
#             original.sd = sd(original)) %>%
#   mutate(facet = ifelse((.)$parameter %in% c("proportion_of_habitat", "matrix_permeability", "perceptual_range", "vision_angle"), "main effect", "interaction"),
#          original.min =  ifelse(((.)$original.mu - (.)$original.sd) < 0, 0, (.)$original.mu - (.)$original.sd),
#          original.max = (.)$original.mu + (.)$original.sd)
# 
# ggplot(sobol.out.agg_assess, aes(x = parameter, y = original.mu)) +
#   facet_wrap(~facet, scales = "free", ncol = 1) +
#   coord_flip() +
#   geom_linerange(aes(ymin = original.min, ymax = original.max), linewidth = 1) +
#   geom_point(size = 2) +
#   theme_ipsum(base_size = 11, axis_text_size = 11, axis_title_size = 11, strip_text_size = 14) +
#   theme(panel.spacing = unit(0.5, "lines"))
# 
# sobol.out.agg_crossings <- sobol.out %>%
#   filter(metric == "sum [visits] of patches with [habitat = 0]_mean") %>%
#   group_by(parameter) %>%
#   summarise(original.mu = mean(original),
#             original.sd = sd(original)) %>%
#   mutate(facet = ifelse((.)$parameter %in% c("proportion_of_habitat", "matrix_permeability", "perceptual_range", "vision_angle"), "main effect", "interaction"),
#          original.min =  ifelse(((.)$original.mu - (.)$original.sd) < 0, 0, (.)$original.mu - (.)$original.sd),
#          original.max = (.)$original.mu + (.)$original.sd)
# 
# ggplot(sobol.out.agg_crossings, aes(x = parameter, y = original.mu)) +
#   facet_wrap(~facet, scales = "free", ncol = 1) +
#   coord_flip() +
#   geom_linerange(aes(ymin = original.min, ymax = original.max), linewidth = 1) +
#   geom_point(size = 2) +
#   theme_ipsum(base_size = 11, axis_text_size = 11, axis_title_size = 11, strip_text_size = 14) +
#   theme(panel.spacing = unit(0.5, "lines"))
# 
# #ggsave(here("results", "sobol_index.tiff"), width = 6.0, height = 4.0, dpi=300)
