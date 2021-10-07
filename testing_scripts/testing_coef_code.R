
datapath <-  "D:/NETN/R_Dev/NPSForVeg_Data/"

plots <- read.csv(paste0(datapath, "/EFWG_full_dataset.csv")) %>% select(Unit_Code, Network) %>% unique()
boot_results <- read.csv(paste0(datapath, "/results/intercept/all_metrics_randint_results.csv"))
dbi <- read.csv(paste0(datapath, "/EFWG_park-level_DBI_rank.csv")) %>% select(Unit_Code, DBI_rank)
boot_results_final <- left_join(boot_results, dbi, by = c("park" = "Unit_Code")) %>%
                      left_join(., plots, by = c("park" = "Unit_Code"))

lat_order <- read.csv(
  "D:/NETN/Monitoring_Projects/Forest_Health/manuscripts/invasive_trend_analysis/data/NETN-MIDN-ERMN-NCRN_Total_invasives.csv") %>%
  select(network, park, lat.rank) %>% unique() %>% rename(lat_rank = lat.rank)

head(boot_results_final)
names(lat_order)

boot_results_final <- left_join(boot_results_final, lat_order, by = c("park"))
boot_results_final$park_ord <- reorder(boot_results_final$park, desc(boot_results_final$lat_rank))

boot_results_final$park_orddbi <- reorder(boot_results_final$park, boot_results_final$DBI_rank)
head(boot_results_final)

unique(boot_results_final$resp)

plot_slopes(boot_results_final, ylabel = "Change in Sapling Density (stems/ha) per cycle",
            metric = "Sap_Dens_NatCan", order = "park_ord", group = "Network", sign_only = F,
            legend_position = 'bottom')

plot_slopes(boot_results_final, ylabel = 'test', metric = "Seed_Dens_NatCan",
            order = 'park_ord', sign_only = T)
