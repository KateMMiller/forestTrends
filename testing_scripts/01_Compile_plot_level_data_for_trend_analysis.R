#--------------------------------------------
# Compiling Tree, Sapling and Seedling Data for Eastern Forest Regen Project
#   Code written by Kate Miller 202010916 to check results against NPSForVeg package
#--------------------------------------------

#---- Dependencies and import data using NPSForVeg ----
#devtools::install_github("NCRN/NPSForVeg")
library(NPSForVeg)
library(tidyverse)
library(rlang)
library(vegan)

options(scipen = 100)

datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/"
ncrn <- importNCRN(paste0(datapath,"NCRN_data"))
ermn <- importERMN(paste0(datapath, "ERMN_data"))                   
midn <- importMIDN(paste0(datapath, "MIDN_data"))
netn <- importNETN(paste0(datapath, "NETN_data"))

spp_list <- read.csv(paste0(datapath, "NPS_tree_species_groups.csv"))

#arglist <- list(years = 2008:2019, status = 'alive')
yrs_all <- 2008:2019
c1 <- 2008:2011; c2 <- c1+4; c3 <- c2+4
cycles <- list(c1, c2, c3)
cycle_names <- c("C1", "C2", "C3")

network_codes <- c("ERMN", "MIDN", "NCRN", "NETN")

sum_by_cycle <- function(network, network_name, group, year_span, value, units, value_name, cycle_name){
  df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
                  area = units, status = 'alive') %>% 
        pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
        mutate(cycle = cycle_name, Network = network_name)
return(data.frame(df))
}

sum_by_cycle_dbh <- function(network, network_name, group, year_span, value, units, 
                             value_name, cycle_name, size_min, size_max){
  df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
                  area = units, status = 'alive', size.min = size_min, size.max = size_max) %>% 
    pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
    mutate(cycle = cycle_name, Network = network_name)
  return(data.frame(df))
}
  
#---- Set up plot x visit df for left join ----
plot_visit_df1 <- rbind(getEvents(ermn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "ERMN"),
                        getEvents(midn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "MIDN"),
                        getEvents(ncrn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "NCRN"),
                        getEvents(netn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "NETN")) %>% 
#                  filter(!Plot_Name %in% plot_exclude) %>% 
                  rename(Year = Event_Year) %>% 
                  mutate(cycle = case_when(Year %in% 2008:2011 ~ 1,
                                           Year %in% 2012:2015 ~ 2,
                                           Year %in% 2016:2019 ~ 3,
                                           TRUE ~ NA_real_))

plot_sizes <- rbind(read.csv(paste0(datapath, "ERMN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "MIDN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "NCRN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "NETN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize)
                    )

plot_sizes$Network[plot_sizes$Network == "NCBN"] <- "MIDN"

# Relate subplot numbers and areas to each visit
  # Set up NCRN Events so same columns as rest of networks to ensure plots missing seeds/saps are included
  # and plots missing a subplot are also specified
ncrn_events <- left_join(read.csv(paste0(datapath, "NCRN_data/Events.csv")) %>% 
                 mutate(numHerbPlots = numSeedPlots), 
               read.csv(paste0(datapath, "NCRN_data/MetaData.csv")) %>% 
                 select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                        ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize), 
               by = c("Unit_Code" = "ParkCode")) %>% 
               mutate(numSapPlots = SapPlotNum, excludeEvent = 0) %>% 
               select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, excludeEvent)


event_info <- rbind(read.csv(paste0(datapath, "ERMN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, 
                             excludeEvent),
                    read.csv(paste0(datapath, "MIDN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, 
                             excludeEvent),
                    ncrn_events,
                    read.csv(paste0(datapath, "NETN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots,
                             excludeEvent)
                    )

plot_visit_df2 <- left_join(plot_visit_df1, plot_sizes, by = c("Unit_Code" = "ParkCode", "Network"))
head(plot_visit_df2)
head(event_info)
# mutates take event-level number of subplots if not NA. Otherwise, take the park-level number of subplots
# from the metadata file. Use excludeEvent from Events.csv to drop ACAD-029-2010
plot_visit_df <- left_join(plot_visit_df2, event_info, by = c("Plot_Name", "Year" = "Event_Year")) %>% 
                 mutate(SapPlotNum = ifelse(!is.na(numSapPlots), numSapPlots, SapPlotNum),
                        SeedPlotNum = ifelse(!is.na(numSeedPlots), numSeedPlots, SeedPlotNum),
                        HPlotNum = ifelse(!is.na(numHerbPlots), numHerbPlots, HPlotNum)) %>% 
                 select(-starts_with("num")) #%>% 
                 #filter(excludeEvent != 1) #excludes ACAD-029-2010 and COLO-380-2018

#write.csv(plot_visit_df, "EFWG_plot_visit_left_join.csv", row.names = F)

head(plot_visit_df)

# Check for duplicates
plot_visit_df %>% 
  group_by(Plot_Name, Network, Unit_Code, Year) %>% 
  summarize(num_events = n(), .groups = 'drop')%>% 
  filter(num_events > 1) #0- no duplicate events

table(plot_visit_df$Unit_Code, plot_visit_df$Year)
table(plot_visit_df$Unit_Code, plot_visit_df$cycle)
length(unique(plot_visit_df$Plot_Name)) #1515 unique plots
nrow(plot_visit_df) # 4464 plot x cycle rows
table(complete.cases(plot_visit_df)) # all 4462 T
head(plot_visit_df)

# Find max DBH of trees (partial check on ERMN's 999999 and for dbh dist.)
t(rbind(lapply(seq_along((ermn)), function(x){
  max(ermn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #137.7

t(rbind(lapply(seq_along((midn)), function(x){
  max(midn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #147.8

t(rbind(lapply(seq_along((ncrn)), function(x){
  max(ncrn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #172.5

t(rbind(lapply(seq_along((netn)), function(x){
  max(netn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #132.4

# Bring in deer browse index, so can order facets by Deer Browse Impacts in most recent cycle
dbi <- rbind(read.csv(paste0(datapath, "/ERMN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID), 
             read.csv(paste0(datapath, "/MIDN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID), 
             read.csv(paste0(datapath, "/NCRN_data/Plot_Visit_Data.csv")) %>% 
               select(Plot_Name, Sample_Year, Deer_Impact) %>% 
               rename(Event_Year = Sample_Year, DBI = Deer_Impact),
             read.csv(paste0(datapath, "/NETN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID)) %>% 
       filter(Event_Year >= 2016)


names(plot_visit_df)
dbi2 <- left_join(plot_visit_df %>% select(Plot_Name, Unit_Code, Year, Network, excludeEvent), 
                  dbi, by = c("Plot_Name", "Year" = "Event_Year")) %>% 
        filter(Year >= 2016)

plots <- c(unique(dbi$Plot_Name))
missing_plots <- plot_visit_df %>% filter(!Plot_Name %in% plots) %>% select(Plot_Name, Unit_Code)
missing_plots # checking with ERMN to make sure it's okay that these don't have cycle 3 visit data
#     Plot_Name Unit_Code
# 1   ALPO-012      ALPO
# 2   ALPO-012      ALPO
# 3   ALPO-027      ALPO
# 4   ALPO-027      ALPO
# 5   BLUE-088      BLUE
# 6   BLUE-088      BLUE
# 7   GARI-085      GARI
# 8   GARI-085      GARI
# 9   GARI-152      GARI
# 10  GARI-152      GARI
# 11  NERI-212      NERI
# 12  NERI-213      NERI

table(dbi2$Unit_Code, dbi2$Year)
table(dbi2$DBI)
table(dbi2$excludeEvent)
length(unique(dbi2$Plot_Name)) #1508, different than 1515. Not sure why
dbi2$DBI[dbi2$DBI >= 9] <- NA

dbi_sum <- dbi2 %>% group_by(Unit_Code) %>% 
                    summarize(mean_DBI = mean(DBI, na.rm = T)) %>% 
                    ungroup() %>% mutate(DBI_rank = rank(mean_DBI, ties.method = "first")) %>% 
                    arrange(DBI_rank)

write.csv(dbi_sum, paste0(datapath, "EFWG_park-level_DBI_rank.csv"), row.names = F)

plot_visit_df <- left_join(plot_visit_df, dbi_sum, by = "Unit_Code")
head(plot_visit_df)
#filter(excludeEvent != 1) #excludes ACAD-029-2010 and COLO-380-2018

write.csv(plot_visit_df, "EFWG_plot_visit_left_join.csv", row.names = F)

#---- Live tree density by species group in stems/ha ----
live_tree_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens <- rbind(live_tree_dens_ermn, live_tree_dens_midn, live_tree_dens_ncrn, live_tree_dens_netn)
live_tree_dens_spp <- right_join(spp_list, live_tree_dens, by = "Species") %>% 
                      mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_dens_spp$Species, live_tree_dens_spp$Group))

live_tree_dens_total <- live_tree_dens_spp %>% 
                          filter(Group == "Total") %>% 
                          select(Network, Plot_Name, cycle, Dens) %>% 
                          rename(Tree_Dens_Total = Dens)

live_tree_dens_native <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 1) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_Native = sum(Dens, na.rm = T), 
                                     .groups = 'drop')

live_tree_dens_natcan <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 1 & Canopy_Tree == 1) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_NatCan = sum(Dens, na.rm = T), 
                                     .groups = 'drop')

live_tree_dens_exotic <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 0) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_Exotic = sum(Dens, na.rm = T), 
                                    .groups = 'drop')
head(live_tree_dens_natcan)

live_tree_dens_final <- list(plot_visit_df, live_tree_dens_total, live_tree_dens_native, 
                             live_tree_dens_natcan, live_tree_dens_exotic) %>% 
                        reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  
length(unique(live_tree_dens_final$Plot_Name))
nrow(plot_visit_df) #4464
nrow(live_tree_dens_final)#4464
table(complete.cases(live_tree_dens_final))

#---- Live tree BA by species group in m2/ha ----
live_tree_ba_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 3))

live_tree_ba <- rbind(live_tree_ba_ermn, live_tree_ba_midn, live_tree_ba_ncrn, live_tree_ba_netn)
live_tree_ba_spp <- right_join(spp_list, live_tree_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_ba_spp$Species, live_tree_ba_spp$Group))

live_tree_ba_total <- live_tree_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(Tree_BA_Total = BA)

live_tree_ba_native <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_ba_natcan <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_ba_exotic <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_ba_natcan)

live_tree_ba_final <- list(plot_visit_df, live_tree_ba_total, live_tree_ba_native, 
                             live_tree_ba_natcan, live_tree_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_ba_final)#4464
table(complete.cases(live_tree_ba_final)) #4464 T

#---- Live tree density in pole-small (10-19.9 cm) size class by species group in stems/ha ----
live_tree_10cm_dens_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(ermn, network_name = "ERMN",  
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_10cm_dens_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_10cm_dens_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_10cm_dens_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_10cm_dens <- rbind(live_tree_10cm_dens_ermn, live_tree_10cm_dens_midn, live_tree_10cm_dens_ncrn, live_tree_10cm_dens_netn)
live_tree_10cm_dens_spp <- right_join(spp_list, live_tree_10cm_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_10cm_dens_spp$Species, live_tree_10cm_dens_spp$Group))

live_tree_10cm_dens_total <- live_tree_10cm_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(tree_10cm_Dens_Total = Dens)

live_tree_10cm_dens_native <- live_tree_10cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_dens_natcan <- live_tree_10cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_dens_exotic <- live_tree_10cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

head(live_tree_10cm_dens_natcan)

live_tree_10cm_dens_final <- list(plot_visit_df, live_tree_10cm_dens_total, live_tree_10cm_dens_native, 
                                    live_tree_10cm_dens_natcan, live_tree_10cm_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_10cm_dens_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_tree_10cm_dens_final)#4464
table(complete.cases(live_tree_10cm_dens_final)) #4464 T

#---- Live tree BA in pole-small (10-19.9 cm) by species group in m2/ha ----
live_tree_10cm_ba_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(ermn, network_name = "ERMN",    
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_10cm_ba_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_10cm_ba_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_10cm_ba_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_10cm_ba <- rbind(live_tree_10cm_ba_ermn, live_tree_10cm_ba_midn, live_tree_10cm_ba_ncrn, live_tree_10cm_ba_netn)
live_tree_10cm_ba_spp <- right_join(spp_list, live_tree_10cm_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_10cm_ba_spp$Species, live_tree_10cm_ba_spp$Group))

live_tree_10cm_ba_total <- live_tree_10cm_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(tree_10cm_BA_Total = BA)

live_tree_10cm_ba_native <- live_tree_10cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_ba_natcan <- live_tree_10cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_ba_exotic <- live_tree_10cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_10cm_ba_natcan)
head(live_tree_ba_natcan)

live_tree_10cm_ba_final <- list(plot_visit_df, live_tree_10cm_ba_total, live_tree_10cm_ba_native, 
                                  live_tree_10cm_ba_natcan, live_tree_10cm_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_10cm_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_10cm_ba_final)#4464
table(complete.cases(live_tree_10cm_ba_final)) #4464 T

#---- Live tree density in pole-large (20.0-29.9 cm) size class by species group in stems/ha ----
live_tree_20cm_dens_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(ermn, network_name = "ERMN",  
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_20cm_dens_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_20cm_dens_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_20cm_dens_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_20cm_dens <- rbind(live_tree_20cm_dens_ermn, live_tree_20cm_dens_midn, live_tree_20cm_dens_ncrn, live_tree_20cm_dens_netn)
live_tree_20cm_dens_spp <- right_join(spp_list, live_tree_20cm_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_20cm_dens_spp$Species, live_tree_20cm_dens_spp$Group))

live_tree_20cm_dens_total <- live_tree_20cm_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(tree_20cm_Dens_Total = Dens)

live_tree_20cm_dens_native <- live_tree_20cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_20cm_dens_natcan <- live_tree_20cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_20cm_dens_exotic <- live_tree_20cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

head(live_tree_20cm_dens_natcan)

live_tree_20cm_dens_final <- list(plot_visit_df, live_tree_20cm_dens_total, live_tree_20cm_dens_native, 
                                    live_tree_20cm_dens_natcan, live_tree_20cm_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_20cm_dens_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_tree_20cm_dens_final)#4464
table(complete.cases(live_tree_20cm_dens_final)) #4464 T

#---- Live tree BA in pole-large (20.0 - 29.9 cm) by species group in m2/ha ----
live_tree_20cm_ba_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(ermn, network_name = "ERMN",    
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_20cm_ba_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_20cm_ba_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_20cm_ba_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_20cm_ba <- rbind(live_tree_20cm_ba_ermn, live_tree_20cm_ba_midn, live_tree_20cm_ba_ncrn, live_tree_20cm_ba_netn)
live_tree_20cm_ba_spp <- right_join(spp_list, live_tree_20cm_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_20cm_ba_spp$Species, live_tree_20cm_ba_spp$Group))

live_tree_20cm_ba_total <- live_tree_20cm_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(tree_20cm_BA_Total = BA)

live_tree_20cm_ba_native <- live_tree_20cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_20cm_ba_natcan <- live_tree_20cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_20cm_ba_exotic <- live_tree_20cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_20cm_ba_natcan)
head(live_tree_ba_natcan)

live_tree_20cm_ba_final <- list(plot_visit_df, live_tree_20cm_ba_total, live_tree_20cm_ba_native, 
                                  live_tree_20cm_ba_natcan, live_tree_20cm_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_20cm_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_20cm_ba_final)#4464
table(complete.cases(live_tree_20cm_ba_final)) #4464 T

#---- Live tree density in sawtimber-small (30.0 - 39.9 cm) size class by species group in stems/ha ----
live_tree_30cm_dens_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(ermn, network_name = "ERMN",  
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_30cm_dens_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_30cm_dens_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_30cm_dens_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_30cm_dens <- rbind(live_tree_30cm_dens_ermn, live_tree_30cm_dens_midn, live_tree_30cm_dens_ncrn, live_tree_30cm_dens_netn)
live_tree_30cm_dens_spp <- right_join(spp_list, live_tree_30cm_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_30cm_dens_spp$Species, live_tree_30cm_dens_spp$Group))

live_tree_30cm_dens_total <- live_tree_30cm_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(tree_30cm_Dens_Total = Dens)

live_tree_30cm_dens_native <- live_tree_30cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_30cm_dens_natcan <- live_tree_30cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_30cm_dens_exotic <- live_tree_30cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

head(live_tree_30cm_dens_natcan)

live_tree_30cm_dens_final <- list(plot_visit_df, live_tree_30cm_dens_total, live_tree_30cm_dens_native, 
                                   live_tree_30cm_dens_natcan, live_tree_30cm_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_30cm_dens_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_tree_30cm_dens_final)#4464
table(complete.cases(live_tree_30cm_dens_final)) #4464 T

#---- Live tree BA in sawtimber-small (30.0 - 39.9 cm) by species group in m2/ha ----
live_tree_30cm_ba_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(ermn, network_name = "ERMN",    
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 3))

live_tree_30cm_ba_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 3))

live_tree_30cm_ba_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 3))

live_tree_30cm_ba_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 3))

live_tree_30cm_ba <- rbind(live_tree_30cm_ba_ermn, live_tree_30cm_ba_midn, live_tree_30cm_ba_ncrn, live_tree_30cm_ba_netn)
live_tree_30cm_ba_spp <- right_join(spp_list, live_tree_30cm_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_30cm_ba_spp$Species, live_tree_30cm_ba_spp$Group))

live_tree_30cm_ba_total <- live_tree_30cm_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(tree_30cm_BA_Total = BA)

live_tree_30cm_ba_native <- live_tree_30cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_30cm_ba_natcan <- live_tree_30cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_30cm_ba_exotic <- live_tree_30cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_30cm_ba_natcan)
head(live_tree_ba_natcan)

live_tree_30cm_ba_final <- list(plot_visit_df, live_tree_30cm_ba_total, live_tree_30cm_ba_native, 
                                 live_tree_30cm_ba_natcan, live_tree_30cm_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_30cm_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_30cm_ba_final)#4464
table(complete.cases(live_tree_30cm_ba_final)) #4464 T

#---- Live tree density in sawtimber-lg (39.9+cm) size class by species group in stems/ha ----
live_tree_40cm_dens_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(ermn, network_name = "ERMN",  
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_40cm_dens_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_40cm_dens_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_40cm_dens_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_40cm_dens <- rbind(live_tree_40cm_dens_ermn, live_tree_40cm_dens_midn, live_tree_40cm_dens_ncrn, live_tree_40cm_dens_netn)
live_tree_40cm_dens_spp <- right_join(spp_list, live_tree_40cm_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_40cm_dens_spp$Species, live_tree_40cm_dens_spp$Group))

live_tree_40cm_dens_total <- live_tree_40cm_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(tree_40cm_Dens_Total = Dens)

live_tree_40cm_dens_native <- live_tree_40cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_40cm_dens_natcan <- live_tree_40cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_40cm_dens_exotic <- live_tree_40cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

head(live_tree_40cm_dens_natcan)

live_tree_40cm_dens_final <- list(plot_visit_df, live_tree_40cm_dens_total, live_tree_40cm_dens_native, 
                                   live_tree_40cm_dens_natcan, live_tree_40cm_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_40cm_dens_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_tree_40cm_dens_final)#4464
table(complete.cases(live_tree_40cm_dens_final)) #4464 T

#---- Live tree BA in sawtimber-lg (39.9+ cm) by species group in m2/ha ----
live_tree_40cm_ba_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(ermn, network_name = "ERMN",    
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 3))

live_tree_40cm_ba_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 3))

live_tree_40cm_ba_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 3))

live_tree_40cm_ba_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 3))

live_tree_40cm_ba <- rbind(live_tree_40cm_ba_ermn, live_tree_40cm_ba_midn, live_tree_40cm_ba_ncrn, live_tree_40cm_ba_netn)
live_tree_40cm_ba_spp <- right_join(spp_list, live_tree_40cm_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_tree_40cm_ba_spp$Species, live_tree_40cm_ba_spp$Group))

live_tree_40cm_ba_total <- live_tree_40cm_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(tree_40cm_BA_Total = BA)

live_tree_40cm_ba_native <- live_tree_40cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_40cm_ba_natcan <- live_tree_40cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_40cm_ba_exotic <- live_tree_40cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_40cm_ba_natcan)
head(live_tree_ba_natcan)

live_tree_40cm_ba_final <- list(plot_visit_df, live_tree_40cm_ba_total, live_tree_40cm_ba_native, 
                                 live_tree_40cm_ba_natcan, live_tree_40cm_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_40cm_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_40cm_ba_final)#4464
table(complete.cases(live_tree_40cm_ba_final)) #4464 T

#---- Live sapling density by species group in stems/m2 ----
live_sap_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens <- rbind(live_sap_dens_ermn, live_sap_dens_midn, live_sap_dens_ncrn, live_sap_dens_netn)
live_sap_dens_spp <- right_join(spp_list, live_sap_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group)) 

head(live_sap_dens_spp)

spp_list2 <- unique(data.frame(live_sap_dens_spp$Species, live_sap_dens_spp$Group))

sort(unique(live_sap_dens_spp$Group))

live_sap_dens_total <- live_sap_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(Sap_Dens_Total = Dens)

live_sap_dens_native <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_natcan <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_exotic <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_comb <- list(plot_visit_df, live_sap_dens_total, live_sap_dens_native, 
                             live_sap_dens_natcan, live_sap_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_sap_dens_final <- live_sap_dens_comb %>% mutate(Sap_Dens_Total = Sap_Dens_Total/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_Native = Sap_Dens_Native/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_NatCan = Sap_Dens_NatCan/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_Exotic = Sap_Dens_Exotic/(SapPlotNum * SapPlotSize)) %>% 
                                              select(Plot_Name:cycle, Sap_Dens_Total, Sap_Dens_Native, 
                                                     Sap_Dens_NatCan, Sap_Dens_Exotic)


# Checking work
length(unique(live_sap_dens_final$Plot_Name)) #1515
nrow(live_sap_dens_final) #4464
table(complete.cases(live_sap_dens_final)) # 6F: b/c plots are missing regen data, which is correct.

#---- Live sapling BA by species group in m2/ha----
live_sap_ba_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(ermn, network_name = "ERMN",  
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(ermn, network_name = "ERMN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 3))

live_sap_ba <- rbind(live_sap_ba_ermn, live_sap_ba_midn, live_sap_ba_ncrn, live_sap_ba_netn)
live_sap_ba_spp <- right_join(spp_list, live_sap_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_sap_ba_spp$Species, live_sap_ba_spp$Group))

live_sap_ba_total <- live_sap_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(Sap_BA_Total = BA)

live_sap_ba_native <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_sap_ba_natcan <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_sap_ba_exotic <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_sap_ba_natcan)

live_sap_ba_comb <- list(plot_visit_df, live_sap_ba_total, live_sap_ba_native, 
                         live_sap_ba_natcan, live_sap_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_sap_ba_final <- live_sap_ba_comb %>% mutate(Sap_BA_Total = (10000*Sap_BA_Total)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_Native = (10000*Sap_BA_Native)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_NatCan = (10000*Sap_BA_NatCan)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_Exotic = (10000*Sap_BA_Exotic)/(SapPlotNum * SapPlotSize)) %>% 
  select(Plot_Name:cycle, Sap_BA_Total, Sap_BA_Native, 
         Sap_BA_NatCan, Sap_BA_Exotic)

# Checking work
length(unique(plot_visit_df$Plot_Name)) #1515 
length(unique(live_sap_ba_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_sap_ba_final)#4464
table(complete.cases(live_sap_ba_final)) # 6F: b/c plots are missing regen data, which is correct.

#---- Live seedling density by species group in stems/m2 ----
live_seed_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(ermn, network_name = "ERMN",  
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(ermn, network_name = "ERMN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens <- rbind(live_seed_dens_ermn, live_seed_dens_midn, live_seed_dens_ncrn, live_seed_dens_netn)
live_seed_dens_spp <- right_join(spp_list, live_seed_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

spp_list2 <- unique(data.frame(live_seed_dens_spp$Species, live_seed_dens_spp$Group))

sort(unique(live_seed_dens_spp$Group))

live_seed_dens_total <- live_seed_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(Seed_Dens_Total = Dens)

live_seed_dens_native <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_natcan <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_exotic <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_comb <- list(plot_visit_df, live_seed_dens_total, live_seed_dens_native, 
                           live_seed_dens_natcan, live_seed_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_seed_dens_final <- live_seed_dens_comb %>% mutate(Seed_Dens_Total = Seed_Dens_Total/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_Native = Seed_Dens_Native/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_NatCan = Seed_Dens_NatCan/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_Exotic = Seed_Dens_Exotic/(SeedPlotNum * SeedPlotSize)) %>% 
  select(Plot_Name:cycle, Seed_Dens_Total, Seed_Dens_Native, 
         Seed_Dens_NatCan, Seed_Dens_Exotic)


# Checking work
length(unique(live_seed_dens_final$Plot_Name)) #1515
nrow(live_seed_dens_final) #4464
table(complete.cases(live_seed_dens_final)) #10 FALSE; correct
live_seed_dens_final[which(!complete.cases(live_seed_dens_final)), c("Plot_Name", "Year", "Seed_Dens_Total")]
# These are the 10 events that should have missing seedling data
# Plot_Name Year Seed_Dens_Total
# DEWA-159 2017             NaN
# DEWA-304 2017             NaN
# GARI-206 2018             NaN
# COLO-380 2018             NaN
# CATO-0331 2018             NaN
# CHOH-0015 2017             NaN
# CHOH-1191 2010             NaN
# NACE-0493 2017             NaN
# ACAD-029 2010             NaN
# SAGA-008 2010             NaN
names(plot_visit_df)

tree_sap_seed <- list(plot_visit_df %>% select(Plot_Name, Network, Unit_Code, Year, cycle, excludeEvent, DBI_rank),
                      live_tree_ba_final %>% select(Network, Plot_Name, cycle, Tree_BA_Total:Tree_BA_Exotic), 
                      live_tree_dens_final %>% select(Network, Plot_Name, cycle, Tree_Dens_Total:Tree_Dens_Exotic),
                      
                      live_tree_10cm_ba_final %>% select(Network, Plot_Name, cycle, tree_10cm_BA_Total:tree_10cm_BA_Exotic),
                      live_tree_20cm_ba_final %>% select(Network, Plot_Name, cycle, tree_20cm_BA_Total:tree_20cm_BA_Exotic),
                      live_tree_30cm_ba_final %>% select(Network, Plot_Name, cycle, tree_30cm_BA_Total:tree_30cm_BA_Exotic),
                      live_tree_40cm_ba_final %>% select(Network, Plot_Name, cycle, tree_40cm_BA_Total:tree_40cm_BA_Exotic),
                      
                      live_tree_10cm_dens_final %>% select(Network, Plot_Name, cycle, tree_10cm_Dens_Total:tree_10cm_Dens_Exotic),
                      live_tree_20cm_dens_final %>% select(Network, Plot_Name, cycle, tree_20cm_Dens_Total:tree_20cm_Dens_Exotic),
                      live_tree_30cm_dens_final %>% select(Network, Plot_Name, cycle, tree_30cm_Dens_Total:tree_30cm_Dens_Exotic),
                      live_tree_40cm_dens_final %>% select(Network, Plot_Name, cycle, tree_40cm_Dens_Total:tree_40cm_Dens_Exotic),
                      
                      live_sap_dens_final %>% select(Network, Plot_Name, cycle, Sap_Dens_Total:Sap_Dens_Exotic), 
                      live_sap_ba_final %>% select(Network, Plot_Name, cycle, Sap_BA_Total:Sap_BA_Exotic),
                      live_seed_dens_final %>% select(Network, Plot_Name, cycle, Seed_Dens_Total:Seed_Dens_Exotic)) %>% 
                 reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Turn plots that were excluded at the full event level to NA
tree_sap_seed[tree_sap_seed$excludeEvent == 1,
              c("Tree_BA_Total", "Tree_BA_Native", "Tree_BA_NatCan", "Tree_BA_Exotic", 
                "Tree_Dens_Total", "Tree_Dens_Native", "Tree_Dens_NatCan", "Tree_Dens_Exotic",
                
                "tree_10cm_BA_Total", "tree_10cm_BA_Native", "tree_10cm_BA_NatCan", "tree_10cm_BA_Exotic", 
                "tree_10cm_Dens_Total", "tree_10cm_Dens_Native", "tree_10cm_Dens_NatCan", "tree_10cm_Dens_Exotic",
                "tree_20cm_BA_Total", "tree_20cm_BA_Native", "tree_20cm_BA_NatCan", "tree_20cm_BA_Exotic", 
                "tree_20cm_Dens_Total", "tree_20cm_Dens_Native", "tree_20cm_Dens_NatCan", "tree_20cm_Dens_Exotic",
                "tree_30cm_BA_Total", "tree_30cm_BA_Native", "tree_30cm_BA_NatCan", "tree_30cm_BA_Exotic", 
                "tree_30cm_Dens_Total", "tree_30cm_Dens_Native", "tree_30cm_Dens_NatCan", "tree_30cm_Dens_Exotic",
                "tree_40cm_BA_Total", "tree_40cm_BA_Native", "tree_40cm_BA_NatCan", "tree_40cm_BA_Exotic", 
                "tree_40cm_Dens_Total", "tree_40cm_Dens_Native", "tree_40cm_Dens_NatCan", "tree_40cm_Dens_Exotic",

                "Sap_Dens_Total", "Sap_Dens_Native", "Sap_Dens_NatCan", "Sap_Dens_Exotic",
                "Sap_BA_Total", "Sap_BA_Native", "Sap_BA_NatCan", "Sap_BA_Exotic", 
                "Seed_Dens_Total", "Seed_Dens_Native", "Seed_Dens_NatCan", "Seed_Dens_Exotic")] <- NA_real_

write.csv(tree_sap_seed, paste0(datapath, "EFWG_tree_sapling_seedling_BA_dens.csv"), row.names = F)
names(tree_sap_seed)

#---- SiteXSpecies dfs for similarity ----
names(live_tree_dens_spp)
names(live_sap_dens_spp)
names(live_seed_dens_spp)

# Need to clean up species list a little
dens_comb1 <- rbind(live_tree_dens_spp %>% mutate(strata = "tree"),
                    live_sap_dens_spp %>% mutate(strata = "sapling"),
                    live_seed_dens_spp %>% mutate(strata = "seedling")) %>% 
              select(-Group, -Canopy_Tree, -Native)

dens_comb1$Species <- gsub("- ", "", dens_comb1$Species)
dens_comb1$Species <- gsub(" ", "_", dens_comb1$Species)
dens_comb1$Species <- gsub("_sp.", "", dens_comb1$Species)
dens_comb1$Species[grepl("Unknown", dens_comb1$Species)] <- "Unknown"
dens_comb1$Species <- gsub("_spp.", "", dens_comb1$Species)

dens_comb <- dens_comb1 %>% group_by(Plot_Name, Network, cycle, Species, strata) %>% 
                            summarize(Dens = sum(Dens, na.rm = T), .groups = 'drop') %>% 
             left_join(plot_visit_df %>% select(Plot_Name, Network, Unit_Code, cycle), ., 
                       by = c("Plot_Name", "Network", "cycle")) # a bit slow but works

#sort(unique(dens_comb$Species))
length(unique(dens_comb$Plot_Name)) #1515

dens_wide <- dens_comb %>% arrange(Species) %>% 
                           pivot_wider(names_from = Species, 
                                       values_from = Dens, 
                                       values_fill = 0) %>% select(-Total) %>% 
                           arrange(Network, Plot_Name, cycle, strata)

length(unique(dens_wide$Plot_Name)) #1515
table(complete.cases(dens_wide)) #all T
head(dens_wide[,1:10])

# Create siteXspec matrices for each network and drop species that sum to 0
ERMN_siteXspec <- dens_wide %>% filter(Network == "ERMN") %>% select_if(~!is.numeric(.) || sum(.) != 0)
MIDN_siteXspec <- dens_wide %>% filter(Network == "MIDN") %>% select_if(~!is.numeric(.) || sum(.) != 0)
NCRN_siteXspec <- dens_wide %>% filter(Network == "NCRN") %>% select_if(~!is.numeric(.) || sum(.) != 0)
NETN_siteXspec <- dens_wide %>% filter(Network == "NETN") %>% select_if(~!is.numeric(.) || sum(.) != 0)

sor_fun <- function(df){
  df2 <- df %>% select_if(~is.numeric(.) && sum(.) != 0) # remove species that sum to 0
  sor <-  betadiver(df2, method = 'sor')
  return(sor)
}

horn_fun <- function(df){
  df2 <- df %>% select_if(~is.numeric(.) && sum(.) != 0)  #remove species that sum to 0
  hor <-  vegdist(df2, method = 'horn')
  return(hor)
}

ERMN_treesap_nest <- ERMN_siteXspec %>% filter(strata %in% c("tree", "sapling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_sap = map(data, sor_fun),
         Hor_sap = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_sap, Hor_sap)) %>% select(-data)

ERMN_treeseed_nest <- ERMN_siteXspec %>% filter(strata %in% c("tree", "seedling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_seed = map(data, sor_fun),
         Hor_seed = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_seed, Hor_seed)) %>% select(-data)

MIDN_treesap_nest <- MIDN_siteXspec %>% filter(strata %in% c("tree", "sapling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_sap = map(data, sor_fun),
         Hor_sap = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_sap, Hor_sap)) %>% select(-data)

MIDN_treeseed_nest <- MIDN_siteXspec %>% filter(strata %in% c("tree", "seedling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_seed = map(data, sor_fun),
         Hor_seed = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_seed, Hor_seed)) %>% select(-data)

NCRN_treesap_nest <- NCRN_siteXspec %>% filter(strata %in% c("tree", "sapling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_sap = map(data, sor_fun),
         Hor_sap = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_sap, Hor_sap)) %>% select(-data)

NCRN_treeseed_nest <- NCRN_siteXspec %>% filter(strata %in% c("tree", "seedling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_seed = map(data, sor_fun),
         Hor_seed = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_seed, Hor_seed)) %>% select(-data)

NETN_treesap_nest <- NETN_siteXspec %>% filter(strata %in% c("tree", "sapling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_sap = map(data, sor_fun),
         Hor_sap = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_sap, Hor_sap)) %>% select(-data)

NETN_treeseed_nest <- NETN_siteXspec %>% filter(strata %in% c("tree", "seedling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_seed = map(data, sor_fun),
         Hor_seed = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_seed, Hor_seed)) %>% select(-data)

sim_data_sap <- rbind(ERMN_treesap_nest,
                      MIDN_treesap_nest,
                      NCRN_treesap_nest, 
                      NETN_treesap_nest)

sim_data_seed <- rbind(ERMN_treeseed_nest,
                       MIDN_treeseed_nest,
                       NCRN_treeseed_nest, 
                       NETN_treeseed_nest)

head(sim_data_sap)
head(sim_data_seed)

sim_comb <- list(plot_visit_df %>% select(Plot_Name, Network, Unit_Code, cycle, excludeEvent,
                                          SapPlotNum, SeedPlotNum),
                 sim_data_sap, sim_data_seed) %>% 
            reduce(left_join, by = c("Plot_Name", "Network", "Unit_Code", "cycle")) %>% 
            data.frame()

sim_cols <- c("Sor_sap", "Sor_seed", "Hor_sap", "Hor_seed")

sim_comb[,sim_cols][is.na(sim_comb[,sim_cols])] <- 0 

# Plots missing partial or all visit data are converted to NA below
sim_comb[sim_comb$excludeEvent == 1,
              c("Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed")] <- NA_real_

sim_comb[sim_comb$SapPlotNum == 0,
         c("Sor_sap", "Hor_sap")] <- NA_real_

sim_comb[sim_comb$SeedPlotNum == 0,
         c("Sor_seed", "Hor_seed")] <- NA_real_

table(complete.cases(sim_comb)) #4454T; 10F

write.csv(sim_comb, paste0(datapath, "EFWG_Similarity_by_plot_cycle.csv"), row.names = F)

#---- Combine all plot-level datasets ----
sim <- read.csv(paste0(datapath, "EFWG_Similarity_by_plot_cycle.csv"))
tss <- read.csv(paste0(datapath, "EFWG_tree_sapling_seedling_BA_dens.csv"))
stock <- read.csv(paste0(datapath, "EFWG_stocking_index.csv"))
plot_lj <- read.csv(paste0(datapath, "EFWG_plot_visit_left_join.csv"))

reg_comb <- list(plot_lj,
                  tss %>% select(-Year, -excludeEvent, -DBI_rank),
                  stock %>% select(Plot_Name, Network, cycle, stock_final),
                  sim %>% select(Plot_Name, Network, cycle, Sor_sap:Hor_seed)) %>% 
                  reduce(left_join, by = c("Network", "Plot_Name", "cycle")) %>% 
              rename(Unit_Code = Unit_Code.x) %>% select(-Unit_Code.y)

head(reg_comb)
head(dbi)
dbi <- read.csv(paste0(datapath, "EFWG_park-level_DBI_rank.csv")) %>% select(Unit_Code, DBI_rank)
lat_order <- read.csv(
  "D:/NETN/Monitoring_Projects/Forest_Health/manuscripts/invasive_trend_analysis/data/NETN-MIDN-ERMN-NCRN_Total_invasives.csv") %>% 
  select(network, park, lat.rank) %>% unique() %>% rename(lat_rank = lat.rank)

reg_final <- left_join(reg_comb, dbi, by = c("Unit_Code")) %>% 
  left_join(., lat_order, by = c("Network" = "network", "Unit_Code" = "park")) %>% 
  select(Plot_Name:cycle, DBI_rank, lat_rank, everything())

head(reg_final)
write.csv(reg_final, paste0(datapath, "EFWG_full_dataset.csv"), row.names = FALSE)
