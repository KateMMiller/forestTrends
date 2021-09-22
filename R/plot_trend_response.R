#' @title plot_trend_response: Plots response with CIs using output from case_boot_lmer
#'
#' @description Function plots response of
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable. If Unit_Code is in the data frame, then a facet by park will be plotted.
#' @param xlab Quoted title for x axis.
#' @param ylab Quoted title for y axis.
#' @param group Quoted column for facet wraps. If not specified, only 1 plot will be returned
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate
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
#' plot_trend_response(boot1, xlab = 'cycle', ylab = 'BA') +
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
#'   model = map(data, ~case_boot_lmer(., x = "cycle", y = "resp", ID = "Plot_Name",
#'                                     random_type = 'intercept', group = "grp",
#'                                     num_reps = 100, chatty = TRUE)))
#'
#' # Compile results
#' boot_results <- boot2 %>% select(park, model) %>% unnest(model) %>% select(-num_boots)
#'
#' # Plot results
#' plot_trend_response(boot_results, xlab = "Cycle", ylab = "BA", group = "park") +
#'   scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))

#' }
#'
#' @export

plot_trend_response <- function(df, xlab, ylab, group = NA){

    group_sym <- sym(group)

    df2 <- if(!is.na(group)){
            left_join(df,
                      df %>% filter(term == "Slope") %>%
                             mutate(sign = ifelse(lower95 > 0 | upper95 < 0, "sign", "nonsign")) %>%
                             select(!!group_sym, sign),
                      by = group) %>%
             filter(!term %in% c("Intercept", "Slope"))
         } else {
            df %>% mutate(sign = ifelse(term == "Slope" & (lower95 > 0 | upper95 < 0), "sign", "nonsign")) %>%
            filter(!term %in% c("Intercept", "Slope"))
           }

  df2$time <- as.numeric(gsub("\\D", "", df2$term))

  p <-
    ggplot(df2, aes(x = time, y = estimate, shape = sign, linetype = sign))+
       geom_point(na.rm = F)+
       geom_errorbar(aes(ymin = lower95, ymax = upper95),
                     width = 0.2, size = 0.5, linetype = 'solid',
                     na.rm = FALSE)+
       geom_line()+
       scale_linetype_manual(values = c('dashed', 'solid'))+
       scale_shape_manual(values = c(21, 19))+
       theme(axis.text = element_text(size = 11),
             axis.title = element_text(size = 12),
             panel.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_rect(colour = "black", fill = NA, size = 0.1),
             legend.position = 'none')+
       {if(!is.na(group)) facet_wrap(~df2[[group]])}+
       #scale_x_continuous(breaks = xbreaks, labels = xlabels)+
       labs(x = xlab, y = ylab)

  return(p)
}
