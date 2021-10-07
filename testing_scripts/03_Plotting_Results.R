library(forestTrends)
library(tidyverse)
library(grid) # for digging into str of ggplot grobs
library(gridExtra) # for ggarrange
library(gtable)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/"
dens_df <- read.csv(paste0(datapath, "EFWG_full_dataset.csv"))
boot_results <- read.csv(paste0(datapath, "results/intercept/all_metrics_randint_results.csv"))
dbi <- read.csv(paste0(datapath, "/EFWG_park-level_DBI_rank.csv")) %>% select(Unit_Code, DBI_rank)
boot_results_final <- left_join(boot_results, dbi, by = c("park" = "Unit_Code"))
boot_results_final$park_orddbi <- reorder(boot_results_final$park, boot_results_final$DBI_rank)
lat_order <- read.csv(
  "D:/NETN/Monitoring_Projects/Forest_Health/manuscripts/invasive_trend_analysis/data/NETN-MIDN-ERMN-NCRN_Total_invasives.csv") %>% 
  select(network, park, lat.rank) %>% unique() %>% rename(lat_rank = lat.rank)

boot_results_final <- left_join(boot_results_final, lat_order, by = c("park"))
boot_results_final$park_ord <- reorder(boot_results_final$park, desc(boot_results_final$lat_rank))
boot_results_final$network[boot_results_final$park %in% c("COLO", "SAHI", "THST")] <- "NCBN"
boot_results_final$network_ord <- factor(boot_results_final$network, levels = c("NETN", "ERMN", "NCRN", "NCBN", "MIDN"))
table(boot_results_final$network)

#---- Set up lists to iterate over ----
# column names for response
names(dens_df)
metrics <- c(names(dens_df[, c(17:ncol(dens_df))]))

# response titles for plotting
metric_names <- c(rep("Tree BA (sq.m/ha)", 4), 
                  rep("Tree Density (stems/ha)", 4),
                  rep("Tree BA 10 cm (sq.m/ha)", 4),  
                  rep("Tree BA 20 cm (sq.m/ha)", 4), 
                  rep("Tree BA 30 cm (sq.m/ha)", 4),
                  rep("Tree BA 40 cm+ (sq.m/ha)", 4),
                  rep("Tree Dens. 10 cm (stems/ha)", 4),  
                  rep("Tree Dens. 20 cm (stems/ha)", 4), 
                  rep("Tree Dens. 30 cm (stems/ha)", 4),
                  rep("Tree Dens. 40 cm+ (stems/ha)", 4),
                  rep("Sapling Density (stems/sq.m)", 4),
                  rep("Sapling BA (sq.m/ha)", 4),
                  rep("Seedling Density (sq.m/ha)", 4),
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


#---- Set up lists to iterate over ----
#+++++++ TEMPORARY UNTIL FINAL METRICS FINISH +++++++++
# column names for response
names(dens_df)
metrics <- c(names(dens_df[, c(17:24, 57:68)]))
metrics

# response titles for plotting
metric_names <- c(rep("Tree BA (sq.m/ha)", 4), 
                  rep("Tree Density (stems/ha)", 4),
                  rep("Sapling Density (stems/sq.m)", 4),
                  rep("Sapling BA (sq.m/ha)", 4),
                  rep("Seedling Density (sq.m/ha)", 4))

# match metric columns and titles
met_df <- data.frame(metrics, metric_names)

park_met_list <- data.frame(expand.grid(unique(dens_df$Unit_Code), metrics)) %>% 
  set_names("Unit_Code", "metrics") %>% 
  arrange(Unit_Code, metrics) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  left_join(., met_df, by = "metrics")

park_list <- c(park_met_list[,1])
met_list <- c(park_met_list[,2])

boot_results_final$strp_col <- case_when(boot_results_final$network == "ERMN" ~ "#A5BDCD",
                                         boot_results_final$network == "MIDN" ~ "#E7CDA4",
                                         boot_results_final$network == "NCBN" ~ "#CFB9D9",
                                         boot_results_final$network == "NCRN" ~ "#E1E59B",
                                         boot_results_final$network == "NETN" ~ "#AACCA7") 

boot_results_final <- boot_results_final %>% rename(Network = network)
table(boot_results_final$Network)

#----- Setting up colors to color code facets by network 
# First matching networks to colors
park_cols <- rbind(data.frame(park_ord = "BLNK", strp_col = "white"),
                   unique(data.frame(park_ord = boot_results_final$park_ord,
                                     strp_col = boot_results_final$strp_col)) %>% arrange(desc(park_ord))
                   )

park_cols <- park_cols %>% mutate(facet_ord = c(rev(202:209), rev(210:217), rev(218:225), rev(226:233), rev(234:241))) %>% 
                           arrange(facet_ord)
fills <- c(park_cols$strp_col) # for last strip that's blank b/c 39 not 40 parks

# Create fake plots to customize the legend
leg_net <- ggplot(data = data.frame(network = c("ERMN", "MIDN", "NCBN", "NCRN", "NETN"),
                                    x = c(1, 2, 3, 4, 5)),
                   aes(x = x, fill = network))+
            geom_histogram()+
            scale_fill_manual(values = c("#A5BDCD", "#E7CDA4", "#CFB9D9", "#E1E59B", "#AACCA7"), 
                              name = "Network:")+ theme_bw()+
            theme(legend.position = 'bottom', 
                  legend.title = element_text(size = 8), 
                  legend.text=element_text(size = 8),
                  #legend.background = element_blank()
                  legend.background = element_rect(color = "#B9B9B9", size = 0.25)
                  )

sign_color = c("#979797", "#686868", "#6FAB6A", "#BB6363")

leg_line <- ggplot(data = data.frame(sign = c("notmod", "nonsign", "signinc", "signdec"),
                                     x = c(1, 2, 3, 4)), 
                   aes(x = x, y = x, color = sign, fill = sign, linetype = sign, shape = sign))+
            theme_bw()+
            geom_line(size = 0.5)+
            geom_point(size = 1, shape = 21, alpha = 0.8)+
            scale_fill_manual(values = c("notmod" = "white", "nonsign" =  sign_color[2],
                                         "signinc" = sign_color[3], "signdec" = sign_color[4]), 
                              labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                              name = "Trends:",
                              drop = FALSE)+
            scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
                                         "signinc" = sign_color[3], "signdec" = sign_color[4]), 
                               labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                               name = "Trends:",
                               drop = FALSE)+
            scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
                                            "signinc" = 'solid', "signdec" = 'solid'), 
                                  labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                                  name = "Trends:",
                                  drop = FALSE)+
            theme(legend.position = 'bottom', 
                  legend.title = element_text(size = 8), 
                  legend.text = element_text(size = 8),
                  #legend.background = element_blank()
                  legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
                  )

leg_gline <- gtable_filter(ggplot_gtable(ggplot_build(leg_line)), "guide-box")
leg_gnet <- gtable_filter(ggplot_gtable(ggplot_build(leg_net)), "guide-box")

# Plot and save the results for each metric that facets on park with fixed axes
walk(metrics, function(metric){
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  p <- plot_trend_response(df = boot_results_final %>% filter(resp == metric), 
                           xlab = "Cycle", ylab = title, group = "park_ord",
                           model_type = "lmer", facet_cols = 8, 
                           sign_color = c("#979797", "#686868", "#6FAB6A", "#BB6363"))+
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
  cat(metric, "\n")
  
  g <- ggplot_gtable(ggplot_build(p))
  strp_col <- which(grepl('strip-t', g$layout$name))
  
  k <- 1
  for (i in strp_col) {
    g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  png(paste0(datapath, "/results/intercept_lat_net/TSS_by_sppgrp/", metric, ".png"), 
      height = 7, width = 10, units = 'in', res = 600)  
  
  grid.arrange(grobs = list(g, leg_gline, leg_gnet),
               heights = c(6.5, 0.5),
               widths = c(0.6, 5.1, 3.7, 0.6),
               layout_matrix = rbind(c(1, 1, 1, 1),
                                     c(NA, 2, 3, NA)))
  
  dev.off()
})


# Plot and save the results for each metric that facets on park with free_y axes
walk(metrics, function(metric){
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  p <- plot_trend_response(df = boot_results_final %>% filter(resp == metric), 
                           xlab = "Cycle", ylab = title, group = "park_ord",
                           model_type = "lmer", facet_cols = 8, facet_scales = 'free_y',
                           sign_color = c("#D3D3D3", "#BABABA", "#AACCA7", "#CD9494"))+
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
  cat(metric, "\n")
  
  g <- ggplot_gtable(ggplot_build(p))
  strp_col <- which(grepl('strip-t', g$layout$name))
  
  k <- 1
  for (i in strp_col) {
    g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  png(paste0(datapath, "/results/intercept_lat_net/TSS_by_sppgrp/", metric, "_free_y.png"), 
      height = 7, width = 10, units = 'in', res = 600) 
  
  grid.arrange(grobs = list(g, leg_gline, leg_gnet),
               heights = c(6.5, 0.5),
               widths = c(0.6, 5.1, 3.7, 0.6),
               layout_matrix = rbind(c(1, 1, 1, 1),
                                     c(NA, 2, 3, NA)))
  
  dev.off()
})

#---- Coefficient plots for each metric -----
metrics
met_df

plot_slopes(boot_results_final, ylabel = met_df$metric_names[1], metric = metrics[1],
            order = "park_ord", group = "Network", sign_only = TRUE, legend_position = 'bottom')

walk(metrics, function(metric){
  
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  met = as.character(met_df$metrics[met_df$metrics == metric])
  
  p <- plot_slopes(df = boot_results_final, 
                   ylabel = title,
                   metric = met,
                   order = "park_ord",
                   group = "Network",
                   sign_only = TRUE, 
                   legend_position = 'bottom')
  
  ggsave(paste0(datapath, "/results/intercept_lat_net/coef_plots/", metric, "_sign_only.png"), 
         height = 7, width = 10, units = 'in', dpi = 600)


  p <- plot_slopes(df = boot_results_final, 
                   ylabel = title,
                   metric = met,
                   order = "park_ord",
                   group = "Network",
                   sign_only = FALSE, 
                   legend_position = 'bottom')
  
  ggsave(paste0(datapath, "/results/intercept_lat_net/coef_plots/all/", metric, ".png"), 
         height = 7, width = 10, units = 'in', dpi = 600)
  
  cat(metric, "\n")  
  })

# Plot function for park-level plot for tree metrics by spp type
plot_trends_by_grp <- function(df, xlab, ylab, group, var, facet_scales = 'fixed'){ 
  
  if(!is.na(group)){group_sym <- sym(group)}
  var_sym <- sym(var)
  
  df$time <- as.numeric(gsub("\\D", "", df$term))
  
  df1 <- df %>% filter(metric == var) 
  
  df_sign <- df1 %>% filter(term == "Slope") %>% 
    mutate(sign = case_when(lower95 > 0 | upper95 < 0 ~ "sign",
                            is.na(lower95) ~ "notmod",
                            TRUE ~ "nonsign")) %>% select(park, !!group_sym, metric, sign)
  
  df2 <- 
    left_join(df1 %>% filter(!term %in% c("Slope", "Intercept")), 
              df_sign, 
              by = c("park", group, "metric")) %>%
    filter(!term %in% c("Intercept", "Slope"))

  # hacky way to plot groups that didn't get modeled and so don't have errorbars or ribbons
  df2$upper95 <- ifelse(is.na(df2$upper95), df2$estimate, df2$upper95)
  df2$lower95 <- ifelse(is.na(df2$lower95), df2$estimate, df2$lower95)
  
  p <- 
    ggplot(df2, aes(x = time, y = estimate))+ 
    facet_wrap(~park_ord, scales = facet_scales, ncol = 8)+
    geom_point(aes(color = factor(!!group_sym), shape = !!group_sym, fill = !!group_sym), 
               size = 2, na.rm = TRUE)+
    geom_errorbar(aes(ymin = lower95, ymax = upper95, x = time,
                      colour = !!group_sym), width = 0.1, size = 0.5, na.rm = TRUE)+
    geom_line(aes(y = estimate, x = time, colour = !!group_sym, linetype = sign), na.rm = TRUE)+
    # geom_point(aes(y = estimate, x = time, colour = !!group_sym, shape = !!group_sym), 
    #            size = 1.5, na.rm = TRUE)+
    # scale_shape_manual(values = c('notmod' = 21, 'nonsign' = 21, 
    #                               'sign' = 19))+
    # scale_fill_manual(values = c('notmod' = "white", 'nonsign' = "white", 
    #                              'sign' = NULL))+
    scale_linetype_manual(values = c('notmod' = 'dotted', 'nonsign' = 'dashed', 
                                     'sign' = 'solid'))+
    scale_color_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"))+
    scale_fill_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"))+
    scale_shape_manual(values = c("NatCan" = 24, "NatOth" = 21, "Exotic" = 25))+
    theme_bw()+
    theme(axis.text = element_text(size = 11), 
          axis.title = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 12, margin = margin(t = 10, b = -15)),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(), 
          legend.position = 'none') + 
    labs(x = xlab, y = ylab)#+
  return(p)
  } 

#---- Set up lists to iterate over ----
names(boot_results)

# Plot trends by species group
boot_results$sppgrp <- gsub("^.*_", "", boot_results$resp)
sppgrp_list <- c(paste0("_", unique(boot_results$sppgrp), collapse = "|"))
sppgrp_list
boot_results$metric <- gsub(sppgrp_list, "", boot_results$resp)

# Need to pull out other native species from total, native, native canopy.
boot_nat <- boot_results %>% filter(sppgrp == "Native") %>% select(-resp, -sppgrp, -num_boots) %>% 
  rename(est_nat = estimate, l95_nat = lower95, u95_nat = upper95)
boot_exo <- boot_results %>% filter(sppgrp == "Exotic") %>% select(-resp, -sppgrp, -num_boots) %>% 
  rename(est_exo = estimate, l95_exo = lower95, u95_exo = upper95)
boot_nc <- boot_results %>% filter(sppgrp == "NatCan") %>% select(-resp, -sppgrp, -num_boots) %>% 
  rename(est_nc = estimate, l95_nc = lower95, u95_nc = upper95)
boot_tot <- boot_results %>% filter(sppgrp == "Total") %>% select(-resp, -sppgrp, -num_boots) %>% 
  rename(est_tot = estimate, l95_tot = lower95, u95_tot = upper95)

boot_natoth <- list(boot_nat, boot_exo, boot_nc, boot_tot) %>% 
  reduce(left_join, by = c("term", "park", "metric")) %>% 
  mutate(estimate = est_tot - est_exo - est_nc,
         lower95 = l95_tot - l95_exo - l95_nc,
         upper95 = u95_tot - u95_exo - u95_nc,
         sppgrp = "NatOth", 
         resp = paste0(metric, "_", sppgrp)) %>% 
  select(term, estimate, lower95, upper95, park, resp, metric, sppgrp)

boot_results_comb1 <- rbind(boot_results %>% select(-num_boots) %>% filter(!sppgrp %in% c("Native", "Total")), 
                            boot_natoth) %>% arrange(park, resp, term)

# lat_order <- read.csv(
#   "D:/NETN/Monitoring_Projects/Forest_Health/manuscripts/invasive_trend_analysis/data/NETN-MIDN-ERMN-NCRN_Total_invasives.csv") %>% 
#   select(network, park, lat.rank) %>% unique() %>% rename(lat_rank = lat.rank)

boot_results_comb <- left_join(boot_results_comb1, lat_order, by = c("park")) %>% rename(Network = network)
boot_results_comb$park_ord <- reorder(boot_results_comb$park, desc(boot_results_comb$lat_rank))

head(boot_results_comb)

metrics <- c(unique(boot_results_comb$metric))
metrics[1:5]

# response titles for plotting
metric_names <- c("Sapling BA (sq.m/ha)",
                  "Sapling Density (stems/sq.m)",
                  "Seedling Density (stems/sq.m)",
                  "Tree BA (sq.m/ha)", 
                  "Tree Density (stems/ha)"#,
                  # "Tree BA 17.8-25.4cm (sq.m/ha)", 
                  # "Tree Dens. 17.8-25.4cm (stems/ha)", 
                  # "Tree BA 10-17.7cm (sq.m/ha)",  
                  # "Tree Dens. 10-17.7cm (stems/ha)",  
                  # "Tree BA >40.1cm (sq.m/ha)",
                  # "Tree Dens. >40.1cm (stems/ha)",
                  # "Tree Dens. 25.5-40cm (stems/ha)",
                  # "Tree BA 25.5-40cm (sq.m/ha)"
                  )

# Matching networks to colors
park_cols <- rbind(data.frame(park_ord = "BLNK", strp_col = "white"),
                   unique(data.frame(park_ord = boot_results_final$park_ord,
                                     strp_col = boot_results_final$strp_col)) %>% arrange(desc(park_ord))
)

park_cols <- park_cols %>% mutate(facet_ord = c(rev(202:209), rev(210:217), rev(218:225), rev(226:233), rev(234:241))) %>% 
  arrange(facet_ord)

fills <- c(park_cols$strp_col) # for last strip that's blank b/c 39 not 40 parks

leg_net <- ggplot(data = data.frame(network = c("ERMN", "MIDN", "NCBN", "NCRN", "NETN"),
                                    x = c(1, 2, 3, 4, 5)),
                  aes(x = x, fill = network))+
  geom_histogram()+
  scale_fill_manual(values = c("#A5BDCD", "#E7CDA4", "#CFB9D9", "#E1E59B", "#AACCA7"), 
                    name = "Network:")+ theme_bw()+
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 7), 
        legend.text=element_text(size = 7),
        #legend.background = element_blank()
        legend.background = element_rect(color = "#B9B9B9", size = 0.25)
  )

leg_line2 <- ggplot(data = data.frame(sign = c("notmod", "nonsign", "sign"),
                                     x = c(1, 2, 3)), 
                   aes(x = x, y = x, color = sign, fill = sign, linetype = sign))+
  theme_bw()+ geom_line(size = 0.5)+ # geom_point(size = 1.5, shape = 21, alpha = 0.8)+
  scale_fill_manual(values = c("notmod" = "#686868", "nonsign" =  "#686868", "sign" = "#686868"), 
                     labels = c("Not Modeled", "Not Sign.", "Sign."),
                     name = "Trends:",
                     drop = FALSE)+
  scale_color_manual(values = c("notmod" = "#686868", "nonsign" =  "#686868", "sign" = "#686868"), 
                    labels = c("Not Modeled", "Not Sign.", "Sign."),
                    name = "Trends:",
                    drop = FALSE)+
  scale_linetype_manual(values = c("notmod" = 'dotted', "nonsign" = 'dashed', "sign" = 'solid'), 
                        labels = c("Not Modeled", "Not Sign.", "Sign."),
                        name = "Trends:",
                        drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

leg_linesp <- ggplot(data = data.frame(spgrp = c("Exotic", "NatCan", "NatOth"),
                                      x = c(1, 2, 3)), 
                    aes(x = x, y = x, color = spgrp, fill = spgrp, shape = spgrp))+
  theme_bw()+ geom_line(size = 0.5)+  
  geom_point(size = 1.5)+
  scale_fill_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"), 
                    labels = c("Native Canopy", "Native Other", "Exotic"),
                    name = "Groups:",
                    drop = FALSE)+
  scale_color_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"), 
                    labels = c("Native Canopy", "Native Other", "Exotic"),
                    name = "Groups:",
                    drop = FALSE)+
  scale_shape_manual(values = c("NatCan" = 24, "NatOth" = 21, "Exotic" = 25),
                     labels = c("Native Canopy", "Native Other", "Exotic"),
                     name = "Groups:",
                     drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

leg_gnet <- gtable_filter(ggplot_gtable(ggplot_build(leg_net)), "guide-box")
leg_gline2 <- gtable_filter(ggplot_gtable(ggplot_build(leg_line2)), "guide-box")
leg_glinesp <- gtable_filter(ggplot_gtable(ggplot_build(leg_linesp)), "guide-box")

# match metric columns and titles
met_df <- data.frame(metrics = metrics[1:5], metric_names)
met_df

head(boot_results_comb)

walk(metrics, function(metric){
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  var = as.character(met_df$metrics[met_df$metrics == metric])
  sppgrp = as.character(unique(boot_results_comb$sppgrp[boot_results_comb$metric == metric]))
 
  p <- plot_trends_by_grp(boot_results_comb, 
                          xlab = "Cycle", ylab = title,
                          group = "sppgrp", var = var, 
                          facet_scales = 'fixed')
  
  g <- ggplot_gtable(ggplot_build(p))
  strp_col <- which(grepl('strip-t', g$layout$name))
  
  k <- 1
  for (i in strp_col) {
    g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  png(paste0(datapath, "/results/intercept_lat_net/spp_grp_plots/", metric, ".png"), 
      height = 7, width = 10, units = 'in', res = 600) 
  
  grid.arrange(grobs = list(g, leg_glinesp, leg_gline2, leg_gnet),
               heights = c(6.5, 0.5),
               widths = c(0.55, 2.75, 3.125, 3.35, 0.25),
               layout_matrix = rbind(c(1, 1, 1, 1, 1),
                                     c(NA, 2, 3, 4, NA)))
  dev.off()
  })

walk(metrics, function(metric){
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  var = as.character(met_df$metrics[met_df$metrics == metric])
  sppgrp = as.character(unique(boot_results_comb$sppgrp[boot_results_comb$metric == metric]))
  
  p <- plot_trends_by_grp(boot_results_comb, 
                          xlab = "Cycle", ylab = title,
                          group = "sppgrp", var = var, 
                          facet_scales = 'free_y')
  
  g <- ggplot_gtable(ggplot_build(p))
  strp_col <- which(grepl('strip-t', g$layout$name))
  
  k <- 1
  for (i in strp_col) {
    g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  png(paste0(datapath, "/results/intercept_lat_net/spp_grp_plots/free_y/", metric, "_free_y.png"), 
      height = 7, width = 10, units = 'in', res = 600) 
  
  grid.arrange(grobs = list(g, leg_glinesp, leg_gline2, leg_gnet),
               heights = c(6.5, 0.5),
               widths = c(0.55, 2.75, 3.125, 3.35, 0.25),
               layout_matrix = rbind(c(1, 1, 1, 1, 1),
                                     c(NA, 2, 3, 4, NA)))
  dev.off()
})


