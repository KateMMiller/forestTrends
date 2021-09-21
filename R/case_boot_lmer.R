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
#' @param y Quoted response variable in the data frame.
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
#' fake_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
#'                       cycle = rep(1:3, times = 9),
#'                       resp = runif(27, 0, 20))
#'
#' boot1 <- case_boot_lmer(fake_df, y = "resp", num_reps = 10, chatty = TRUE)
#'
#' }
#'
#' @export

case_boot_lmer <- function(df, y, num_reps, chatty = TRUE){

  if(!"Plot_Name" %in% names(df)){stop('Must have column named "Plot_Name" to run function')}
  if(!"cycle" %in% names(df)){stop('Must have column named "cycle" to run function')}

  if(!requireNamespace("magrittr", quietly = TRUE)){
    stop("Package 'magrittr' must be installed.", call. = FALSE)}

  if(!requireNamespace("dplyr", quietly = TRUE)){
    stop("Package 'dplyr' must be installed.", call. = FALSE)}

  if(!requireNamespace("purrr", quietly = TRUE)){
    stop("Package 'purrr' must be installed.", call. = FALSE)}

  if(!requireNamespace("tidyr", quietly = TRUE)){
    stop("Package 'tidyr' must be installed.", call. = FALSE)}

  if(chatty == TRUE){cat(unique(substr(df$Plot_Name, 1, 4)))}

  boot_mod <- purrr::map_df(seq_len(num_reps),
                            ~case_boot_sample(df, y, sample = T, sample_num = .x)) %>%
    tidyr::pivot_wider(names_from = term, values_from = estimate)

  real_mod <- case_boot_sample(df, y, sample = F, sample_num = 1) %>% dplyr::select(-boot_num, -isSingular)

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

  results <- dplyr::left_join(real_mod, boot_CIs, by = "term")
  if(chatty == TRUE){cat("Done", "\n")}
  return(results)
}

