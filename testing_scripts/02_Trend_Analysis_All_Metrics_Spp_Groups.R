#----------------------------------
# Plotting/Analysis checks against NPSForVeg
#----------------------------------

#devtools::install_github("KateMMiller/forestTrends") #uncomment to install
library(forestTrends)
library(tidyverse)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/"
dens_df <- read.csv(paste0(datapath, "EFWG_full_dataset.csv"))

# Added park b/c Unit_Code gets dropped from data after nested. Only need 'park' for progress in console
# Don't need to drop SAHI or WOTR b/c case_boot_lmer will exclude parks with < 7 plots from
# the bootstrap, but will fit a model to the raw data for plotting

#---- Set up lists to iterate over ----
# column names for response
names(dens_df)
metrics <- c(names(dens_df[, c(17:ncol(dens_df))]))
metrcs <- c(names(dens_df[, c(17:68)]))

# response titles for plotting
metric_names <- c(rep("Tree BA (m2/ha)", 4), 
                  rep("Tree Density (stems/ha)", 4),
                  rep("Tree BA 10 cm (m2/ha)", 4),  
                  rep("Tree BA 20 cm (m2/ha)", 4), 
                  rep("Tree BA 30 cm (m2/ha)", 4),
                  rep("Tree BA 40 cm+ (m2/ha)", 4),
                  rep("Tree Dens. 10 cm (stems/ha)", 4),  
                  rep("Tree Dens. 20 cm (stems/ha)", 4), 
                  rep("Tree Dens. 30 cm (stems/ha)", 4),
                  rep("Tree Dens. 40 cm+ (stems/ha)", 4),
                  rep("Sapling Density (stems/m2)", 4),
                  rep("Sapling BA (m2/ha)", 4),
                  rep("Seedling Density (m2/ha)", 4)) 
                  # "Stocking Index", "Sorensen Sapling", 
                  # "Horn Sapling", "Sorensen Seedling", "Horn Seedling")

# match metric columns and titles
met_df <- data.frame(metrics, metric_names)

park_met_list <- data.frame(expand.grid(unique(dens_df$Unit_Code), metrics)) %>% 
  set_names("Unit_Code", "metrics") %>% 
  arrange(Unit_Code, metrics) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  left_join(., met_df, by = "metrics")

park_list <- c(park_met_list[,1])
met_list <- c(park_met_list[,2])

# Run the random intercept model for each combination of park and metric.
# This will take a few hours...
boot_results <- map2_df(park_list, met_list,
                        function(park, y){case_boot_lmer(df = dens_df %>% filter(Unit_Code == park),
                                                         x = "cycle", y = y, ID = "Plot_Name",
                                                         random_type = 'intercept',
                                                         num_reps = 1000, chatty = TRUE) %>%
                            mutate(park = paste(park), resp = paste(y))})

write.csv(boot_results, paste0(datapath, "/results/intercept/all_metrics_randint_results.csv"), row.names = F)


#----- Added metrics to model -----
datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/"
dens_df <- read.csv(paste0(datapath, "EFWG_full_dataset.csv"))

# Added park b/c Unit_Code gets dropped from data after nested. Only need 'park' for progress in console
# Don't need to drop SAHI or WOTR b/c case_boot_lmer will exclude parks with < 7 plots from
# the bootstrap, but will fit a model to the raw data for plotting

#---- Set up lists to iterate over ----
# column names for response
metrics <- c(names(dens_df[, c(25:56, 69:73)]))
metrics
# response titles for plotting
metric_names <- c(rep("Tree BA 10 cm (m2/ha)", 4),  
                  rep("Tree BA 20 cm (m2/ha)", 4), 
                  rep("Tree BA 30 cm (m2/ha)", 4),
                  rep("Tree BA 40 cm+ (m2/ha)", 4),
                  rep("Tree Dens. 10 cm (stems/ha)", 4),  
                  rep("Tree Dens. 20 cm (stems/ha)", 4), 
                  rep("Tree Dens. 30 cm (stems/ha)", 4),
                  rep("Tree Dens. 40 cm+ (stems/ha)", 4),
                  "Stocking Index", "Sorensen Sapling", 
                  "Horn Sapling", "Sorensen Seedling", 
                  "Horn Seedling")

# match metric columns and titles
met_df <- data.frame(metrics, metric_names)

park_met_list <- data.frame(expand.grid(unique(dens_df$Unit_Code), metrics)) %>% 
  set_names("Unit_Code", "metrics") %>% 
  arrange(Unit_Code, metrics) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  left_join(., met_df, by = "metrics")

park_list <- c(park_met_list[,1])
met_list <- c(park_met_list[,2])

# Run the random intercept model for each combination of park and metric.
# This will take a few hours...
boot_results <- map2_df(park_list, met_list,
                        function(park, y){case_boot_lmer(df = dens_df %>% filter(Unit_Code == park),
                                                         x = "cycle", y = y, ID = "Plot_Name",
                                                         random_type = 'intercept',
                                                         num_reps = 1000, chatty = TRUE) %>%
                            mutate(park = paste(park), resp = paste(y))})

write.csv(boot_results, paste0(datapath, "/results/intercept/all_metrics_randint_results_sim.csv"), row.names = F)
