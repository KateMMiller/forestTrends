#' @include case_boot_sample.R
#'
#' @title case_boot_lmer: run case bootstrap for random intercept model and return model output
#'
#' @description For each replicate, as specified by num_reps, function will generate a bootstrapped sample of data from the
#' original dataset, fit a random intercept model for plot, and return the estimates for intercept, slope, and predicted
#' values for mean response of each cycle, along with 95 % confidence intervals for each of these terms. The num_boots column
#' is the number of bootstrapped samples that successfully fit an lmer model. If any singular fits are returned, a warning
#' message is printed in the console to indicate number of bootstraps that returned singular fits, but the model results are
#' stored and included in the confidence interval estimates.
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable.
#' @param x Quoted time variable for trend analysis. Default is "cycle", but can also model by year. Must be numeric.
#' @param y Quoted response variable in the data frame.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param group Quote column containing a grouping variable, like "Unit_ID" for printing progress to console. If not specified,
#' will print the first 4 characters of the ID to the console, assuming the ID starts with a 4-letter park code.
#' @param random_type intercept or slope. The intercept option (default) will fit a random intercept on plot with (1|Plot_Name) as
#' random component. The slope option will fit a random slope model with (1 + cycle|Plot_Name)
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
#'
#' }
#'
#' @export

case_boot_lmer <- function(df, x = "cycle", y, ID = "Plot_Name", group = NA,
                           random_type = c('intercept', 'slope'), num_reps, chatty = TRUE){

  if(missing(df)){stop("Must specify df to run function")}
  if(missing(x)){stop("Must specify x variable to run function")}
  if(missing(y)){stop("Must specify y variable to run function")}
  if(missing(ID)){stop("Must specify ID variable to run function")}
  if(missing(num_reps)){stop("Must specify num_reps (number of replicates) for bootstrap")}
  random_type <- match.arg(random_type)
  stopifnot(c(x, y, ID) %in% names(df))
  # stopifnot(is.numeric(df[,x]))
  # stopifnot(is.numeric(df[,y]))

  pname1 <- substr(df[1, ID], 1, 4)

  grp <- ifelse(!is.na(group), paste(unique(df[,group])),
                                 paste(pname1))

  plots <- data.frame(unique(df[,1]))

  nplots <- nrow(unique(plots))

  if(chatty == TRUE){cat(grp)}

  real_mod <- suppressWarnings(case_boot_sample(df, x = x, y = y, ID = ID,
                                                sample = F, sample_num = 1, random_type = random_type) %>%
                                 dplyr::select(-boot_num, -isSingular))

 if(nplots > 6){
   boot_mod <-
    suppressWarnings(purrr::map_df(seq_len(num_reps),
                                   ~case_boot_sample(df, x = x, y = y, ID = ID,
                                                     sample = T, sample_num = .x,
                                                     random_type = random_type)) %>%
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
    boot_CIs
 } else if(nplots <=6){
   warning("Fewer than 6 plots for case bootstrap. Returning data.frame with estimates but no CIs.")
   boot_CIs <- data.frame(term = real_mod$term, lower95 = NA, upper95 = NA, num_boots = NA)
   boot_CIs
   }

  results <- dplyr::left_join(real_mod, boot_CIs, by = "term")

  if(chatty == TRUE){cat("Done", "\n")}
  return(results)
}

