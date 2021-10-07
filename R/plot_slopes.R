#' @title plot_slopes: Plots slopes and 95\% CIs using output from case_boot_lmer
#'
#' @description Function plots the slopes from case_boot_lmer output
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable. If Unit_Code is in the data frame, then a facet by park will be plotted.
#' @param xlab Quoted title for x axis.
#' @param ylab Quoted title for y axis.
#' @param order Quoted column plot will sort by if specified.
#' @param group Group to color code by. If not specified, all points and lines will be black. If "Network" is specified,
#' then points and lines will be color coded by network, where ERMN is blue, MIDN is orange, NCRN is yellow, and NETN is green.
#' @param sign_only TRUE/FALSE. Denotes whether to plot all trends (FALSE; Default) or only significant trends (TRUE).
#' @param legend_position Quoted position for legend following ggplot positions. Default is 'none'.
#' @import ggplot2
#' @importFrom dplyr arrange filter mutate
#'
#' @examples
#' \dontrun{
#' #----- Example if running for first time -----
#' boot_results <- map2_df(park_list, met_list,
#'                         function(park, y){case_boot_lmer(df = dens_df %>% filter(Unit_Code == park),
#'                                                          x = "cycle", y = y, ID = "Plot_Name",
#'                                                          random_type = 'intercept',
#'                                                          num_reps = 1000, chatty = TRUE) %>%
#'                             mutate(park = paste(park), resp = paste(y))})
#' write.csv(boot_results, "all_metrics_randint_results.csv")
#'
#' plot_slopes(boot_results, ylabel = "Change in Sapling Density (stems/ha) per cycle",
#'             metric = "Sap_Dens_Total", order = "park", sign_only = TRUE)
#'
#'
#' #----- Example for loading results of previous step -----
#' # Takes output in form of csv and uses deer browse index to order parks
#' # and color code by Network (requires joining some dataframes together).
#'
#' boot_results <- read.csv("all_metrics_randint_results.csv")
#' dbi <- read.csv("EFWG_park-level_DBI_rank.csv") %>% select(Unit_Code, DBI_rank)
#' plots <- read.csv("EFWG_full_dataset.csv") %>% select(Unit_Code, Network) %>% unique()
#'
#' boot_results_final <- left_join(boot_results, dbi, by = c("park" = "Unit_Code")) %>%
#'                       left_join(., plots, by = c("park" = "Unit_Code"))
#'
#' boot_results_final$park_ord <- reorder(boot_results_final$park, boot_results_final$DBI_rank)
#'
#' #--- Plot slopes using the park_ord field, which is sorted high to low on DBI_rank, and color code by Network
#' plot_slopes(boot_results_final, ylabel = "Change in Sapling Density (stems/ha) per cycle",
#'            metric = "Sap_Dens_NatCan", order = "park_ord", group = "Network")
#'
#' #--- Plot slopes using park_ord field, and color code by whether significant trend or not
#' plot_slopes(boot_results_final, ylabel = 'test', metric = "Seed_Dens_NatCan", order = 'park_ord',
#' group = 'sign')
#'
#' #--- Plot only significant slopes using the park_ord field, which is sorted high to low on DBI_rank,
#' # and color code by Network
#'
#' plot_slopes(boot_results_final, ylabel = "Change in Sapling Density (stems/ha) per cycle",
#'            metric = "Sap_Dens_NatCan", order = "park_ord", group = "Network", sign_only = TRUE)
#' }
#' @export


plot_slopes <- function(df, ylabel, metric, order = NA, group = NA, sign_only = FALSE, legend_position = 'none'){

  if(!is.na(group)){group_sym <- sym(group)}
  if(!is.na(order)){order_sym <- sym(order)}

  df1 <- if(!is.na(order)) {
    df %>% filter(term == "Slope" & resp == metric) %>% arrange(desc(!!order_sym))
  } else {df}

  df2 <- df1 %>% mutate(sign = ifelse(lower95 > 0 | upper95 < 0, "sign", "nonsign")) %>%
                 filter(!is.na(lower95))

  df3 <- if(sign_only == TRUE){df2 %>% filter(sign == "sign")} else {df2}


  p <-
    ggplot(df3, aes(x = if(!is.na(order)){!!order_sym} else {park},
                    y = estimate, shape = sign,
                    color = {if(!is.na(group)){!!group_sym} else {NULL}},
                    fill = {if(!is.na(group)){!!group_sym} else {NULL}})) +
          geom_hline(yintercept = 0, lwd = 1, color = 'DimGrey') +
          geom_errorbar(aes(ymin = lower95, ymax = upper95,
                            color = {if(!is.na(group)){!!group_sym} else {NULL}}),
                        width = 1.5, size = 1, show.legend = F)+
          # Have to add 2 geom_points, so significant ones can be solid and color coded by group
          # and non-sign are filled white
          geom_point(aes(fill = {if(!is.na(group)){!!group_sym} else {NULL}}),
                     stroke = 1, size = 2, fill = 'white', color = 'DimGrey', shape = 21)+
          geom_point(data = df2 %>% filter(sign == "sign"),
                     aes(x = if(!is.na(order)){!!order_sym} else {park},
                         y = estimate,
                         fill = {if(!is.na(group)){!!group_sym} else {NULL}}),
                     stroke = 1, size = 2, shape = 21, color = 'DimGrey')+
          {if(!is.na(group)){
              if(group == "Network"){
                scale_fill_manual(values = c("ERMN" = "#97BCF7", "MIDN" = "#F9AD51", "NCBN" = "#DC84F8",
                                             "NCRN" = "#E9E905", "NETN" = "#78E43C"),
                                  name = "Network")
              } else {scale_fill_brewer()}
            }}+
          {if(!is.na(group)){
              if(group == "Network"){
                scale_color_manual(values = c("ERMN" = "#97BCF7", "MIDN" = "#F9AD51", "NCBN" = "#DC84F8",
                                              "NCRN" = "#E9E905", "NETN" = "#78E43C"),
                                   name = "Network")
            } else {scale_color_brewer()}
            }}+
         theme_bw()+
         theme(axis.text = element_text(size = 11),
               axis.title = element_text(size = 12),
               panel.background = element_blank(),
               #panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_rect(colour = "black", fill = NA, size = 0.1),
               legend.position = legend_position)+
          coord_flip()+
          scale_x_discrete(limits = rev)+
          labs(y = paste0('Slope: ', ylabel), x = NULL) #ylim(yrange)+

 return(p)
}
