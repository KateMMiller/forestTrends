#' @title plot_trend_response: Plots response with CIs using output from case_boot_lmer
#'
#' @description Function plots response of
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable. If Unit_Code is in the data frame, then a facet by park will be plotted.
#' @param ylab Quoted title for y axis
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate
#'
#' @examples
#' \dontrun{
#'
#' fake_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
#'                       cycle = rep(1:3, times = 9),
#'                       resp = runif(27, 0, 20))
#'
#' boot1 <- case_boot_lmer(fake_df, y = "resp", num_reps = 10, random_type = 'intercept', chatty = TRUE)
#'
#' plot_trend_response(boot1, "Tree Basal Area")
#'
#'
#' }
#'
#' @export

plot_trend_response <- function(df, ylab){

  df2 <- df %>%
           mutate(sign = ifelse(term == "Slope" & (lower95 > 0 | upper95 < 0), "sign", "nonsign")) %>%
           filter(!term %in% c("Intercept", "Slope")) %>%
           mutate(cycle = as.numeric(substr(term, 2, 2)))

  p <- ggplot(df2, aes(x = cycle, y = estimate, shape = sign, linetype = sign))+
       geom_point()+
       geom_errorbar(aes(ymin = lower95, ymax = upper95),
                     width = 0.2, size = 0.5, linetype = 'solid', na.rm = TRUE)+
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
       {if("Unit_Code" %in% names(df)) facet_wrap(~Unit_Code)}+
       labs(x = "Cycle", y = ylab)+
       scale_x_continuous(breaks = c(1,2,3), labels = c("1", "2", "3"))

  return(p)
}
