#--------------------------------------------
# Compiling Live tree data for dbh distribution plots
#--------------------------------------------

#---- Dependencies and import data using NPSForVeg ----
#devtools::install_github("NCRN/NPSForVeg")
library(NPSForVeg)
library(tidyverse)
#library(rlang)
#library(vegan)

options(scipen = 100)

datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/"
# ncrn <- importNCRN(paste0(datapath,"NCRN_data"))
# ermn <- importERMN(paste0(datapath, "ERMN_data"))                   
# midn <- importMIDN(paste0(datapath, "MIDN_data"))
# netn <- importNETN(paste0(datapath, "NETN_data"))

#arglist <- list(years = 2008:2019, status = 'alive')
yrs_all <- 2008:2019
c1 <- 2008:2011; c2 <- c1+4; c3 <- c2+4
cycles <- list(c1, c2, c3)
cycle_names <- c("C1", "C2", "C3")

network_codes <- c("ERMN", "MIDN", "NCRN", "NETN")

# sum_by_cycle <- function(network, network_name, group, year_span, value, units, value_name, cycle_name){
#   df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
#                   area = units, status = 'alive') %>% 
#     pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
#     mutate(cycle = cycle_name, Network = network_name)
#   return(data.frame(df))
# }
# 
# sum_by_cycle_dbh <- function(network, network_name, group, year_span, value, units, 
#                              value_name, cycle_name, size_min, size_max){
#   df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
#                   area = units, status = 'alive', size.min = size_min, size.max = size_max) %>% 
#     pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
#     mutate(cycle = cycle_name, Network = network_name)
#   return(data.frame(df))
# }

plot_visit_df <- read.csv(paste0(datapath, "EFWG_plot_visit_left_join.csv")) # compiled in previous step

length(unique(plot_visit_df$Plot_Name)) #1515 unique plots
nrow(plot_visit_df) # 4464 plot x cycle rows
table(complete.cases(plot_visit_df)) # all 4462 T
head(plot_visit_df)

#---- Live tree density by species group in stems/ha ----
network = c("ERMN", "MIDN", "NCRN", "NETN")

load_data <- function(network, file, path = datapath){
  read.csv(paste0(datapath, network, "_data/", file, ".csv")) %>% mutate(network = network)
}

#------ Load and combine seedling and sapling data -----
# Metadata
meta <- rbind( 
  load_data("ERMN", "MetaData") %>% select(ParkCode, TPlotSize),
  load_data("MIDN", "MetaData") %>% select(ParkCode, TPlotSize),
  load_data("NCRN", "MetaData") %>% select(ParkCode, TPlotSize),
  load_data("NETN", "MetaData") %>% select(ParkCode, TPlotSize)
)

# Tree data
ermn_trees <- load_data("ERMN", "Trees")
ermn_trees$Equiv_Dead_DBH_cm[ermn_trees$Equiv_Dead_DBH_cm==999999]<-NA
ermn_trees$Equiv_Live_DBH_cm[ermn_trees$Equiv_Live_DBH_cm==999999]<-NA
ermn_trees$SumDeadBasalArea_cm2[ermn_trees$SumDeadBasalArea_cm2==785398000000]<-NA
ermn_trees$SumLiveBasalArea_cm2[ermn_trees$SumLiveBasalArea_cm2==785398000000]<-NA

midn_trees <- load_data("MIDN", "Trees")
netn_trees <- load_data("NETN", "Trees")
ncrn_trees <- load_data("NCRN", "Trees")
names(ncrn_trees)

tree_names <- c("Plot_Name", "Unit_Code", "Sample_Year", "Equiv_Live_DBH_cm")

trees_comb <- rbind(ermn_trees[, tree_names],
                   midn_trees[, tree_names],
                   ncrn_trees[, tree_names],
                   netn_trees[, tree_names]) %>% 
  filter(Equiv_Live_DBH_cm >= 10.0) %>% # remove treeslings <15cm tall in ERMN 
  filter(Sample_Year >= 2008) # takes 3 most recent cycles

head(plot_visit_df)

trees_comb2 <- left_join(plot_visit_df %>% select(Plot_Name, Unit_Code, Year, Network, cycle, excludeEvent), 
                         trees_comb, 
                         by = c("Plot_Name", "Unit_Code", "Year" = "Sample_Year")) %>% 
               left_join(., meta, by = c("Unit_Code" = "ParkCode"))

head(trees_comb2)
hist(trees_comb2$Equiv_Live_DBH_cm)
summary(trees_comb2$Equiv_Live_DBH_cm)
nrow(trees_comb2[trees_comb2$Equiv_Live_DBH_cm > 110,])/nrow(trees_comb2) * 100
#0.05% of trees >110 cm DBH

head(trees_comb2)
summary(trees_comb2$Equiv_Live_DBH_cm)

# ggplot(trees_comb2 %>% filter(Unit_Code == "SARA"), 
#        aes(x = Equiv_Live_DBH_cm, group = as.factor(cycle), fill = as.factor(cycle)))+
#   geom_density(alpha = 0.2)+
#   scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
#                      labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))

# Setting up diameter distribution
trees_comb2 <- trees_comb2 %>% mutate(dbh_10cm = ifelse(Equiv_Live_DBH_cm <20, 1, 0),
                                      dbh_20cm = ifelse(between(Equiv_Live_DBH_cm, 20, 29.9), 1, 0),
                                      dbh_30cm = ifelse(between(Equiv_Live_DBH_cm, 30, 39.9), 1, 0),
                                      dbh_40cm = ifelse(between(Equiv_Live_DBH_cm, 40, 49.9), 1, 0),
                                      dbh_50cm = ifelse(between(Equiv_Live_DBH_cm, 50, 59.9), 1, 0),
                                      dbh_60cm = ifelse(between(Equiv_Live_DBH_cm, 60, 69.9), 1, 0),
                                      dbh_70cm = ifelse(between(Equiv_Live_DBH_cm, 70, 79.9), 1, 0),
                                      dbh_80cm = ifelse(between(Equiv_Live_DBH_cm, 80, 89.9), 1, 0),
                                      dbh_90cm = ifelse(between(Equiv_Live_DBH_cm, 90, 99.9), 1, 0),
                                      dbh_100cm = ifelse(Equiv_Live_DBH_cm >=100, 1, 0),
                                      conv = 10000/TPlotSize) 
head(trees_comb2)
trees_comb2$check <- rowSums(trees_comb2[,9:18], na.rm = T)
table(trees_comb2$check) # none > 1, good

tree_dbh_dist_plot <- trees_comb2 %>% group_by(Plot_Name, Unit_Code, Year, Network, cycle, excludeEvent) %>% 
                                      summarize(across(starts_with("dbh_"), 
                                                ~sum(.x, na.rm = TRUE)*first(conv)),
                                                .groups = 'drop')

tree_dbh_dist_plot[tree_dbh_dist_plot$excludeEvent == 1, 7:16] <- NA_real_
names(tree_dbh_dist_plot)

tree_dbh_dist_park <- tree_dbh_dist_plot %>% group_by(Unit_Code, Network, cycle) %>% 
                                             summarize(#num_plots = sum(!is.na(dbh_10cm)),
                                                       across(starts_with("dbh_"),
                                                             ~mean(.x, na.rm = TRUE)),
                                                       .groups = 'drop')
head(tree_dbh_dist_park)

tree_dbh_dist <- tree_dbh_dist_park %>% 
  pivot_longer(cols = starts_with("dbh"), names_to = "size_class", values_to = "density")

unique(tree_dbh_dist$size_class)
head(tree_dbh_dist)

tree_dbh_dist$size_class <- ordered(tree_dbh_dist$size_class,
                                         levels = c("dbh_10cm", "dbh_20cm", "dbh_30cm",
                                                    "dbh_40cm", "dbh_50cm", 
                                                    "dbh_60cm", "dbh_70cm", "dbh_80cm",
                                                    "dbh_90cm", "dbh_100cm"))

tree_dbh_dist$class <- as.numeric(gsub("\\D", "", tree_dbh_dist$size_class))

dbi <- read.csv(paste0(datapath, "/EFWG_park-level_DBI_rank.csv")) %>% select(Unit_Code, DBI_rank)
tree_dbh_dist_final <- left_join(tree_dbh_dist, dbi, by = "Unit_Code")

head(tree_dbh_dist_final)
tree_dbh_dist_final$park_ord <- reorder(tree_dbh_dist_final$Unit_Code, tree_dbh_dist_final$DBI_rank)
head(tree_dbh_dist_final)

ggplot(data = tree_dbh_dist_final, aes(x = class, y = density, fill = as.factor(cycle),
                                 color = as.factor(cycle)))+
  # geom_area(aes(x = class, y = density, fill = as.factor(cycle),
  #               color = as.factor(cycle)), stat = 'identity', alpha = 0.2)+
  geom_bar(stat = 'identity', position = "dodge")+
  facet_wrap(~park_ord, scales = 'fixed')+ forestNETN::theme_FHM()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0, size = 9))+
  scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "cycle")+
  scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = 'cycle')+
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))+
  labs(x = "DBH size classes in 10cm increments", y = "Tree Density (stems/ha)")

ggsave(paste0(datapath, "results/intercept_dbi/Diam_Dist_bar.png"),
       height = 7, width = 11, units = 'in', dpi = 600)

ggplot(data = tree_dbh_dist_final, aes(x = class, y = density, fill = as.factor(cycle),
                                 color = as.factor(cycle)))+
  #geom_bar(stat = 'identity', position = 'dodge')+
  geom_line(aes(color = as.factor(cycle)), size = 1)+
  #geom_area(aes(x = class, y = density, color = as.factor(cycle)), stat = 'identity', alpha = 0.2)+
  scale_color_manual(values = c("#B4B4B4", "#27AE60", "#196F3D"), name = "cycle")+
  # scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D", name = 'cycle'))+
  # scale_color_viridis_d(direction = -1)+
  facet_wrap(~park_ord, scales = 'fixed')+ 
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))+
  labs(x = "DBH size classes in 10cm increments", y = "Tree Density (stems/ha)")+
  forestNETN::theme_FHM()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10))

ggsave(paste0(datapath, "results/intercept_dbi/Diam_Dist.png"),
       height = 7, width = 11, units = 'in', dpi = 600)


# To get the y axis to be 0-400 for all but ACAD, have to make fake data for all but ACAD that goes to 400.
y400 <- data.frame(tree_dbh_dist_final %>% filter(size_class == "dbh_10cm"))
y400$class <- 0
y400$size_class <- "dbh_0cm"
y400$density <- ifelse(y400$Unit_Code == "ACAD", 640, 400)
head(y400)

tree_dbh_dist2 <- rbind(tree_dbh_dist_final, y400) %>% arrange(Unit_Code, cycle, size_class)

tree_dbh_dist2$size_class <- ordered(tree_dbh_dist2$size_class,
                                     levels = c("dbh_0cm", "dbh_10cm", "dbh_20cm", 
                                                "dbh_30cm", "dbh_40cm", "dbh_50cm", 
                                                "dbh_60cm", "dbh_70cm", "dbh_80cm",
                                                "dbh_90cm", "dbh_100cm"))

head(tree_dbh_dist2)

ggplot(data = tree_dbh_dist2, aes(x = class, y = density, fill = as.factor(cycle),
                                      color = as.factor(cycle)))+
  geom_bar(aes(color = as.factor(cycle)), stat = 'identity', position = 'dodge')+
  #geom_line(aes(color = as.factor(cycle)), size = 0.5)+
  scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "cycle")+
  scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D", name = 'cycle'))+
  facet_wrap(~park_ord, scales = 'free_y')+ 
  scale_x_continuous(limits = c(10, 100),
                     breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))+
  labs(x = "DBH size classes in 10cm increments", y = "Tree Density (stems/ha)")+
  forestNETN::theme_FHM()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10), 
        legend.position = 'bottom')

ggsave(paste0(datapath, "results/intercept_dbi/Diam_Dist_free_y.png"),
       height = 7, width = 11, units = 'in', dpi = 600)




head(y400)
# y400 <- 

p <- 
  ggplot(data = tree_dbh_dist_final, aes(x = class, y = density, fill = as.factor(cycle),
                                       color = as.factor(cycle)))+
  #geom_bar(stat = 'identity', position = 'dodge')+
  geom_line(aes(color = as.factor(cycle)), size = 1)+
  #geom_area(aes(x = class, y = density, color = as.factor(cycle)), stat = 'identity', alpha = 0.2)+
  scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "cycle")+
  scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D", name = 'cycle'))+
  facet_wrap(~park_ord, scales = 'free_y')+ 
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))+
  labs(x = "DBH size classes in 10cm increments", y = "Tree Density (stems/ha)")+
  forestNETN::theme_FHM()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10))

p

ggsave(paste0(datapath, "results/intercept_dbi/Diam_Dist_free_y.png"),
       height = 7, width = 11, units = 'in', dpi = 600)


list.files(paste0(datapath, "results/intercept/"))
