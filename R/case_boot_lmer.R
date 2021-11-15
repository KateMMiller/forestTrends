#' @include case_boot_sample.R
#'
#' @title case_boot_lmer: run case bootstrap for random intercept model and return model output
#'
#' @description For each replicate, as specified by num_reps, function will generate a bootstrapped sample of data from the
#' original dataset, fit a random intercept model for plot, and return the estimates for intercept, slope, and predicted
#' values for mean response of each unique time step (i.e., cycle or year), along with 95 % confidence intervals for each of
#' these terms. The num_boots column is the number of bootstrapped samples that successfully fit an lmer model. If any singular
#' fits are returned, a warning message is printed in the console to indicate number of bootstraps that returned singular fits,
#' but the model results are stored and included in the confidence interval estimates. Note that this approach assumes that
#' y ~ x is a linear relationships. If that assumption is violated, results may be incorrect. If there are fewer than 7 plots,
#' less than 10% of the data are non-zero, or only 1 plot is non-zero, function will fit a model to the data but will not run
#' the bootstrap and calculate confidence intervals.
#'
#' @param df Data frame containing and ID column that identifies each sample unit (e.g., Plot_Name), a column containing a time
#' variable, and a column with at least one response variable.
#' @param x Quoted time variable for trend analysis. Default is "cycle", but can also model by year. Must be numeric.
#' @param y Quoted response variable in the data frame.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param group Including a group variable, like "Unit_ID", will also print that group to show progress in the console.
#' If not specified, will print the first 4 characters of the ID to the console, assuming the ID starts with a 4-letter park code.
#' @param random_type Specify intercept, slope, or custom. The intercept option (default) will fit a random intercept on plot with (1|Plot_Name) as
#' random component. The slope option will fit a random slope model with (1 + cycle|Plot_Name)
#' @param random_formula If random_type = "custom", must specify the random effects formula for the model in quotes. Otherwise leave blank.
#' @param nest_var Quote column containing a grouping variable for nested random effects.
#' @param num_reps Number of replicates to run in the bootstrap
#' @param chatty TRUE or FALSE. TRUE (default) will print progress in the console, including the first four characters
#' in the Plot_Name and a tick for every other replicate of the bootstrap. FALSE will not print progress in console.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join select
#' @importFrom purrr map map_df
#' @importFrom tidyr pivot_wider
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
#' #----- Dataset with 2 parks iterating through each park with purrr. Second park has more plots -----
#' library(tidyverse)
#' fake_2pk <- data.frame(Plot_Name = c(rep(paste0(rep("APRK-", 12), sprintf("%02d", 1:12)), each = 3),
#' rep(paste0(rep("BPRK-", 40), sprintf("%02d", 13:53)), each = 3)),
#' park = c(rep("APRK", 36), rep("BPRK", 123)),
#' cycle = rep(1:3, times = 53),
#' resp = runif(159, 0, 30))
#'
#'# Nest dataset by park
#'
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
#' #----- Dataset with 3 parks that will not go through bootstrap.
#'   #----- Park 1 has too few plots Park 2 only has 1 plot with non-zero; Park 3 has <10% of plots with non-zero  -----
#'
#' Plot_Name <- c(rep(paste0(rep("AAAA-", 6), sprintf("%02d", 1:6)), each = 3),
#' rep(paste0(rep("BBBB-", 12), sprintf("%02d", 1:12)), each = 3),
#' rep(paste0(rep("CCCC-", 24), sprintf("%02d", 1:24)), each = 3))
#' park <- substr(Plot_Name, 1, 4)
#' cycle <- rep(1:3, times = length(Plot_Name)/3)
#' resp <- c(runif(18, 0, 10),
#'         rep(0, times = 34), runif(2, 0, 10),
#'           rep(0, times = 72))
#' resp[sample(53:125, 7)] <- runif(7, 1, 10) # make <10% of CCCC non-zero
#'
#' no_boots <- data.frame(Plot_Name, park, cycle, resp)
#' nested_df <- no_boots %>% mutate(grp = park) %>% group_by(park) %>% nest()
#' # Run case_boot_lmer on nested dataset
#' boot2 <- nested_df %>% mutate(
#'   model = map(data, ~case_boot_lmer(., x = "cycle", y = "resp", ID = "Plot_Name",
#'                                     random_type = 'intercept', group = "grp",
#'                                     num_reps = 100, chatty = TRUE)))
#' # Compile results
#' boot_results <- boot2 %>% select(park, model) %>% unnest(model) %>% select(-num_boots)
#'
#' #----- Examples with custom random effects
#' park = rep(c("APRK", "BPRK", "CPRK"), each = 120)
#' plot_name = paste(park, sprintf("%02d", rep(c(1:40), each = 3)), sep = "-")
#' cycle = rep(1:3, times = 40)
#' b0 = 10
#' b1 = 5
#' grp = ifelse(park == "CPRK", 1, 0.5)
#' y = b0 + b1*cycle*grp # grp C has stronger resp.
#'
#' resp = rnorm(120, mean = y, sd = 2)
#'
#' test_df <- data.frame(park, plot_name, cycle, resp)
#'
#' # nested random intercept
#' test1 <- case_boot_lmer(test_df,
#'                         x = 'cycle', y = 'resp',
#'                         ID = 'plot_name', group = 'park',
#'                         random_type = 'custom',
#'                         random_formula = "(1|park/plot_name)",
#'                         nest_var = "park",
#'                         num_reps = 100, chatty = T)
#'
#' # nested random slope
#' test2 <- case_boot_lmer(test_df,
#'                         x = 'cycle', y = 'resp',
#'                         ID = 'plot_name', group = 'park',
#'                         random_type = 'custom',
#'                         random_formula = "(cycle|park/plot_name)",
#'                         nest_var = "park",
#'                         num_reps = 100, chatty = T)
#' }
#'
#' @export

case_boot_lmer <- function(df, x = "cycle", y, ID = "Plot_Name", group = NA,
                           random_type = c('intercept', 'slope', 'custom'),
                           random_formula = NA, nest_var = NA,
                           num_reps, chatty = TRUE){

  if(is.null(df)){stop("Must specify df to run function")}
  if(is.null(x)){stop("Must specify x variable to run function")}
  if(is.null(y)){stop("Must specify y variable to run function")}
  if(is.null(ID)){stop("Must specify ID variable to run function")}
  if(is.null(num_reps)){stop("Must specify num_reps (number of replicates) for bootstrap")}
  random_type <- match.arg(random_type)
  if(random_type == "custom" & is.na(random_formula)){stop("Must specify random formula of random_type = 'custom'")}
  stopifnot(c(x, y, ID) %in% names(df))
  stopifnot(is.numeric(df[,x]) | is.integer(df[,x]))
  stopifnot(is.numeric(df[,y]) | is.integer(df[,y]))

  pname1 <- substr(df[1, ID], 1, 4)

  grp <- ifelse(!is.na(group), paste(unique(df[,group])),
                                 paste(pname1))

  plots <- data.frame(unique(df[,ID]))

  # Set up path for parks/metrics with too few plots or too few non-zero values
  nplots <- nrow(unique(plots))
  num_zplots <- length(unique(df[,ID][df[,y] > 0]))
  prop_zero <- sum(df[,y] > 0, na.rm = T)/nrow(df)

  run_boot <- ifelse(nplots < 7 | num_zplots <= 1 | prop_zero <= 0.1, FALSE, TRUE)

  if(chatty == TRUE){cat(grp)}

  real_mod <- suppressWarnings(case_boot_sample(df, x = x, y = y, ID = ID, sample = F, sample_num = 1,
                                                group = group,
                                                random_type = random_type, random_formula = random_formula,
                                                nest_var = nest_var,
                                                model_type = 'lmer') %>%
                               dplyr::select(-boot_num, -isSingular))

  if(run_boot == TRUE){

    boot_mod <-
      suppressWarnings(purrr::map_df(seq_len(num_reps),
                                     ~case_boot_sample(df, x = x, y = y, ID = ID, sample = T, sample_num = .x,
                                                       group = group,
                                                       random_type = random_type, random_formula = random_formula,
                                                       nest_var = nest_var,
                                                       model_type = 'lmer')) %>%
      tidyr::pivot_wider(names_from = term, values_from = estimate)) %>% data.frame()


    boot_CIs <- data.frame(t(apply(boot_mod %>% dplyr::select(-boot_num, -isSingular), 2,
                                   quantile, probs = c(0.025, 0.975), na.rm = T)),
                           num_boots = sum((ifelse(is.na(boot_mod$isSingular), 0, 1))),
                           num_boots_sing = sum(boot_mod$isSingular, na.rm = T))

    num_boot_sing <- unique(boot_CIs$num_boots_sing)
    num_boot_fail <- num_reps - max(boot_CIs$num_boots)

    if(num_boot_fail == 0 & num_boot_sing > 0){
      warning(paste0("Singular fits occurred in ", num_boot_sing, " bootstrapped samples, but all models returned fits."))}

    if(num_boot_fail > 0 & num_boot_sing > 0){
      warning(paste0("Singular fits occurred in ", num_boot_sing, " bootstrapped samples, and ", num_boot_fail,
                   " bootstrapped samples failed to return a model fit."))}

    if(num_boot_fail > 0 & num_boot_sing == 0){
      warning(paste0(num_boot_fail, " bootstrapped samples failed to return a model fit."))}

    boot_CIs$term <- rownames(boot_CIs)
    boot_CIs <- boot_CIs %>% dplyr::select(-num_boots_sing)

    colnames(boot_CIs) <- c("lower95", "upper95", "num_boots", "term")

 } else if(run_boot == FALSE){
     ifelse(nplots < 7, warning("Fewer than 7 plots for case bootstrap. Returning data.frame with estimates but no CIs."),
                        warning("Too few non-zero values in the response. Returning data.frame with estimates but no CIs."))

   boot_CIs <- data.frame(term = real_mod$term, lower95 = NA, upper95 = NA, num_boots = NA)

   }

  results <- dplyr::left_join(real_mod, boot_CIs, by = "term")

  if(chatty == TRUE){cat("Done", "\n")}
  return(results)
}

