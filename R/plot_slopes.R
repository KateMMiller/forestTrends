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
#'
#' library(forestTrends)
#' library(tidyverse)
#' #----- Example if running for first time -----
#' fake_2pk <- data.frame(Plot_Name = c(rep(paste0(rep("APRK-", 12), sprintf("%02d", 1:12)), each = 3),
#'                                      rep(paste0(rep("BPRK-", 40), sprintf("%02d", 13:53)), each = 3)),
#'                      Park = c(rep("APRK", 36), rep("BPRK", 123)),
#'                        cycle = rep(1:3, times = 53),
#'                        resp1 = runif(159, 0, 30))
#'
#' fake_2pk$resp2 <- 1 + fake_2pk$cycle * 0.5 + rnorm(159)
#'
#'
#'
#' # Run case_boot_lmer iterating on park and response variable
#' boot2 <- map2_df(rep(c("APRK", "BPRK"), each = 2), rep(c('resp1', 'resp2'), times = 2),
#'                  function(park, y){case_boot_lmer(df = fake_2pk %>% filter(Park == park),
#'                                                   x = "cycle", y = y, ID = "Plot_Name",
#'                                                   random_type = 'intercept',
#'                                                   num_reps = 100, chatty = TRUE) %>%
#'                      mutate(park = paste(park), resp = paste(y))})
#'
#'
#' # Plot results
#' # Mostly default settings
#' plot_slopes(boot2 %>% filter(resp == "resp2"), ylabel = 'Response', sign_only = TRUE)
#'
#' # Order parks different than alphabetical
#' boot2$park_ord <- factor(boot2$park, levels = c("BPRK", "APRK"))
#' plot_slopes(boot2, ylabel = 'Response', order = 'park_ord')
#'
#' # Color code by network
#' boot2$Network <- ifelse(boot2$park == "APRK", "ERMN", "MIDN")
#' plot_slopes(boot2, ylabel = 'Response',  order = 'park_ord', group = "Network")
#'
#'
#' }
#' @export


plot_slopes <- function(df, ylabel, order = NA, group = NA, sign_only = FALSE, legend_position = 'none'){

  group_sym  <- if(!missing(group)){sym(group)} else {NULL}
  order_sym <- if(!missing(order)){sym(order)} else {sym("park")}

  df1 <- if(!is.na(order)){
    df %>% filter(term == "Slope") %>% arrange(desc(!!order_sym))
  } else {
    df %>% filter(term == "Slope")}

  df2 <- df1 %>% mutate(sign = ifelse(lower95 > 0 | upper95 < 0, "sign", "nonsign")) %>%
                 filter(!is.na(lower95))

  df3 <- if(sign_only == TRUE){df2 %>% filter(sign == "sign")} else {df2}


  p <-
    ggplot(df3, aes(x = !!order_sym,
                    y = estimate, shape = sign,
                    color = !!group_sym,
                    fill = !!group_sym)) +
          geom_hline(yintercept = 0, lwd = 1, color = 'DimGrey') +
          geom_errorbar(aes(ymin = lower95, ymax = upper95,
                            color = !!group_sym),
                        width = 1.5, size = 1, show.legend = F)+
          # Have to add 2 geom_points, so significant ones can be solid and color coded by group
          # and non-sign are filled white
          {if(!missing(group))
            geom_point(aes(fill = !!group_sym),
                       stroke = 1, size = 2, fill = 'white', color = 'DimGrey', shape = 21)}+
          {if(!missing(order))
            geom_point(data = df2 %>% filter(sign == "sign"),
                     aes(x = !!order_sym,
                         y = estimate,
                         fill = !!group_sym),
                     stroke = 1, size = 2, shape = 21, color = 'DimGrey')}+
          {if(missing(order))
            geom_point(stroke = 1, size = 2, fill = 'white', color = 'DimGrey', shape = 21)}+

          {if(!missing(group) & group == "Network"){
                scale_fill_manual(values = c("ERMN" = "#97BCF7", "MIDN" = "#F9AD51", "NCBN" = "#DC84F8",
                                             "NCRN" = "#E9E905", "NETN" = "#78E43C"),
                                  name = "Network")} else {scale_fill_brewer()}}+
          {if(!missing(group) & group == "Network"){
                scale_color_manual(values = c("ERMN" = "#97BCF7", "MIDN" = "#F9AD51", "NCBN" = "#DC84F8",
                                              "NCRN" = "#E9E905", "NETN" = "#78E43C"),
                                   name = "Network")} else {scale_color_brewer()}}+
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
