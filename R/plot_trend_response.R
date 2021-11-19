#' @title plot_trend_response: Plots response with CIs using output from case_boot_lmer/loess
#'
#' @description Function plots predicted responses for each time step in the data and 95% confidence intervals
#' around the response derived from case bootstrapping. For the lmer model, a trend is considered significant
#' if the confidence interval of the slope does not contain 0. For the loess model, a trend is considered significant
#' if the confidence intervals of the first and last time steps don't overlap. Note that specifying a window to test
#' for significance (e.g. compare first 4 years vs. last 4 years) is in development. Significant trends are plotted
#' as solid lines and filled symbols and a separate color (if specified) from non-significant trends. Non-significant
#' trends are plotted as dashed lines with open symbols. For models with insufficient sample size for case bootstrap
#' (<7 plots), the coefficients from the model on the raw data were plotted as dashed, light-grey lines without
#' confidence intervals.
#'
#' @param df Data frame containing an ID column that identifies each sample unit (e.g., Plot_Name), a column containing a time
#' variable, and at least one column with a response variable. If Unit_Code is in the data frame, then a facet by park will be plotted.
#' @param xlab Quoted title for x axis.
#' @param ylab Quoted title for y axis.
#' @param group Quoted column for facet wraps. If not specified, only 1 plot will be returned
#' @param ribbon Options are TRUE or FALSE (Default). If TRUE, will plot error as a ribbon instead of errorbars.
#' @param sign_color String of 4 colors to indicate a trend that is not modeled, not significant or significant increase
#' or significant decrease. Default is c("#D3D3D3", "#696969", "#228B22", "#CD5C5C"), which are light grey, dark grey,
#' forest green and a shade of red. Lines and outlines will be color coded based on this parameter. Ribbons and error bars
#' will only plot if trends were modeled (eg park had > 6 plots). Note that for the loess model, significance is determined by
#' comparing the confidence intervals of the first time step to the last time step. If there is no overlap, the trend is
#' considered significant.
#' @param facet_scales Options are "fixed" (Default), "free", "free_y", "free_x". Fixed means all axes will be
#' identical among facets. Free means axes will vary by facets.
#' @param facet_cols Number of columns for facet wrap. Default is 4.
#' @param ptsize Size of points to be plotted. Default is 1. Must be numeric.
#'
#' @import ggplot2
#' @importFrom dplyr case_when filter first last left_join mutate select
#'
#' @examples
#' \dontrun{
#'
#' #----- Dataset with 1 park -----
#' fake_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
#'                       cycle = rep(1:3, times = 9),
#'                       resp = runif(27, 0, 20))
#'
#' boot1 <- case_boot_lmer(fake_df, y = "resp", num_reps = 10, random_type = 'intercept', chatty = TRUE)
#'
#' plot_trend_response(boot1, xlab = 'cycle', ylab = 'BA', model_type = 'lmer') +
#'   scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
#'
#'
#' #----- Dataset with 2 parks iterating through each park with purrr -----
#' # Create fake dataset
#' fake_2pk <- data.frame(Plot_Name = c(rep(paste0(rep("APRK-", 12), sprintf("%02d", 1:12)), each = 3),
#'                        rep(paste0(rep("BPRK-", 12), sprintf("%02d", 13:24)), each = 3)),
#'                        park = c(rep("APRK", 36), rep("BPRK", 36)),
#'                        cycle = rep(1:3, times = 24),
#'                        resp = runif(72, 0, 30))
#'
#' # Nest dataset by park
#' nested_df <- fake_2pk %>% mutate(grp = park) %>% group_by(park) %>% nest()
#'
#' # Run case_boot_lmer on nested dataset
#' boot2 <- nested_df %>% mutate(
#'   model = map(data, ~case_boot_loess(., x = "cycle", y = "resp", ID = "Plot_Name",
#'                                     span = 0.75, group = "grp",
#'                                     num_reps = 100, chatty = TRUE)))
#'
#' # Compile results
#' boot_results <- boot2 %>% select(park, model) %>% unnest(model) %>% select(-num_boots)
#'
#' # Plot results
#' plot_trend_response(boot_results, xlab = "Cycle", ylab = "BA", group = "park", ribbon = T,
#'                     facet_scales = "free") +
#'   scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
#' }
#'
#' @export

plot_trend_response <- function(df, xlab, ylab, model_type = c('lmer', 'loess'), group = NA,
                                ribbon = FALSE, sign_color = c("#D3D3D3", "#696969", "#228B22", "#CD5C5C"),
                                facet_scales = c("fixed"), facet_cols = 4, ptsize = 1){

    match.arg(facet_scales, c("fixed", "free", "free_y", "free_x"))

    if(!is.na(group)){group_sym <- sym(group)}

    if(length(sign_color) < 4){
      warning(paste0("Only specified ", length(sign_color), " colors instead of 4. Will replace missing color with black by default"))
      sign_color <- c(sign_color, rep("black", 4-length(sign_color)))}

    df$time <- as.numeric(gsub("\\D", "", df$term))

    df2 <- if(model_type == "lmer"){
               if(!is.na(group)){
                 left_join(df, df %>% filter(term == "Slope") %>%
                                      mutate(sign = case_when(lower95 > 0 ~ "signinc",
                                                              upper95 < 0 ~ "signdec",
                                                              is.na(lower95) ~ "notmod",
                                                              TRUE ~ "nonsign")) %>%
                                      select(!!group_sym, sign),
                            by = group) %>%
                 filter(!term %in% c("Intercept", "Slope"))
         } else {cbind(df, df %>% filter(term == "Slope") %>%
                                  mutate(sign = case_when(lower95 > 0 ~ "signinc",
                                                          upper95 < 0 ~ "signdec",
                                                          is.na(lower95) ~ "notmod",
                                                          TRUE ~ "nonsign")) %>%
                                  select(sign)) %>%
                 filter(!term %in% c("Intercept", "Slope"))
         }
    } else if (model_type == "loess"){
               if(!is.na(group)){
                 left_join(df, df %>% arrange(time) %>% group_by(!!group_sym) %>%
                                 summarize(up_first = first(upper95),
                                           up_last = last(upper95),
                                           lo_first = first(lower95),
                                           lo_last = last(lower95),
                                           sign = case_when(up_first < lo_last ~ "signinc",
                                                            lo_first > up_last ~ "signdec",
                                                            is.na(up_first) ~ "notmod",
                                                            TRUE ~ "nonsign")) %>%
                                 select(!!group_sym, sign), by = group)
               } else {
                 cbind(df, df %>% arrange(time) %>%
                               summarize(up_first = first(upper95),
                                         up_last = last(upper95),
                                         lo_first = first(lower95),
                                         lo_last = last(lower95),
                                         sign = case_when(up_first < lo_last ~ "signinc",
                                                          lo_first > up_last ~ "signdec",
                                                          is.na(up_first) ~ "notmod",
                                                          TRUE ~ "nonsign")) %>% select(sign)
                 )


               }
  }
    #df2$time <- as.numeric(gsub("\\D", "", df2$term))
    # hacky way to plot groups that didn't get modeled and so don't have errorbars or ribbons
    df2$upper95 <- ifelse(is.na(df2$upper95), df2$estimate, df2$upper95)
    df2$lower95 <- ifelse(is.na(df2$lower95), df2$estimate, df2$lower95)

  p <-
    if(model_type == "lmer"){
    ggplot(df2, aes(x = time, y = estimate, linetype = sign, color = sign, fill = sign))+
      {if(ribbon == FALSE) geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, size = 0.5,
                                         linetype = 'solid', na.rm = TRUE)}+
      {if(ribbon == TRUE) geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = sign, color = sign),
                                      #fill = "#CACACA",
                                      #color = "#CACACA",
                                      lty = 1, alpha = 0.2, na.rm = TRUE)}+
       geom_line(size = 0.5)+
       geom_point(size = ptsize, shape = 21, alpha = 0.8)+
       scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
                                        "signinc" = 'solid', "signdec" = 'solid'))+
       scale_fill_manual(values = c("notmod" = "white", "nonsign" =  sign_color[2],
                                    "signinc" = sign_color[3], "signdec" = sign_color[4]), drop = FALSE)+
       scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
                                     "signinc" = sign_color[3], "signdec" = sign_color[4]), drop = FALSE)+
       # scale_shape_manual(values = c("notmod" = 21, "nonsign" = 21,
       #                               "signinc" = 21, "signdec" = 21))+
       #scale_fill_manual(values = c("notmod" = 'white', "nonsign" = 'white', "sign" = 'black'))+
       #scale_color_manual(values = c("notmod" = "#ACACAC", "nonsign" = "black", "sign" = "black"))+
       theme(axis.text = element_text(size = 11),
             axis.title = element_text(size = 12),
             panel.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_rect(colour = "black", fill = NA, size = 0.1),
             legend.position = 'none')+
       {if(!is.na(group)) facet_wrap(~df2[[group]], drop = FALSE, scales = facet_scales, ncol = facet_cols)}+
       #scale_x_continuous(breaks = xbreaks, labels = xlabels)+
       labs(x = xlab, y = ylab)
    } else if(model_type == "loess"){
      ggplot(df2, aes(x = time, y = estimate, linetype = sign, color = sign, fill = sign))+
        {if(ribbon == FALSE) geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, size = 0.5,
                                           linetype = 'solid', na.rm = TRUE)}+
        {if(ribbon == TRUE) geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = sign, color = sign),
                                        #fill = "#CACACA",
                                        #color = "#CACACA",
                                        lty = 1, alpha = 0.2, na.rm = TRUE)}+
        geom_line(size = 0.5)+
        geom_point(size = ptsize, shape = 21, alpha = 0.8)+
        scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
                                         "signinc" = 'solid', "signdec" = 'solid'), drop = FALSE)+
        #scale_fill_manual(values = c("notmod" = 'white', "nonsign" = 'white', "sign" = 'black'))+
        #scale_color_manual(values = c("notmod" = "#ACACAC", "nonsign" = "black", "sign" = "black"))+
        scale_fill_manual(values = c("notmod" = "white", "nonsign" =  sign_color[2],
                                     "signinc" = sign_color[3], "signdec" = sign_color[4]), drop = FALSE)+
        scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
                                      "signinc" = sign_color[3], "signdec" = sign_color[4]), drop = FALSE)+
        theme(axis.text = element_text(size = 11),
              axis.title = element_text(size = 12),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 0.1),
              legend.position = 'none')+
        {if(!is.na(group)) facet_wrap(~df2[[group]], drop = FALSE, scales = facet_scales, ncol = facet_cols)}+
        #scale_x_continuous(breaks = xbreaks, labels = xlabels)+
        labs(x = xlab, y = ylab)
      }


  return(p)
}
