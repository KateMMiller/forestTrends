#' @include case_boot_power.R
#'
#' @title power_sim: Calculates power using simulation and case bootstrapping for hierarchical models.
#'
#' @description Fits models using a range of effect and samples sizes using non-parametric bootstrapping to assess significance. Returns summarized model output for each iteration. Only enabled for lmer(). Internal function for trend_power_sim().
#'
#' @importFrom purrr map_df
#' @importFrom dplyr case_when filter left_join select
#'
#' @param data Data frame containing an ID column that identifies each sample unit (e.g., Plot_Name), a column containing a time
#' variable, and at least one column with a response variable.
#' @param y Quoted response variable in the data frame. Must be numeric.
#' @param years Vector of years to run simulation out to. Default is 5 years.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param group Including a group variable, like "Unit_ID", will print that group to show progress in the console.
#' If not specified, will print the first 4 characters of the ID to the console, assuming the ID starts with a 4-letter park code. #### CHANGE THIS TO PRINT ITERATION PARAMS
#' @param random_type For model_type = "lmer", specify "intercept", "slope", or "custom". The intercept option (default) will fit a random intercept model
#' with (1|ID) as random component. The slope option will fit a random slope model with (1 + x|ID) as the random component.
#' If "custom" is used, must also specify random_formula.
#'
#' @param sample TRUE or FALSE. TRUE (default) will generate a bootstrapped sample of the specified data frame. FALSE will
#' run trend_lmer() on the original dataset.
#'
#' @param num_reps Number of replicate bootstraps to run for each level of effect and sample size. Default is 10 for faster testing. However, 500-1000 is the better number for real analyses.
#'
#' @param error_dist Either "nonpar" or "normal". If nonpar is chosen, must specify a dataset of repeated measures, like from QA/QC sampling, to generate an error distribution based on the data. If normal is chosen, then a normal distribution will be used. This will be used to add sampling error to simulated trends that are at an appropriate scale to the dataset.
#'
#' @param sampling_data If error_dist = 'nonpar', specify a dataset that has repeated measures for sites. Otherwise leave blank. The columns in the sampling data should include a unique ID column that is specified via the ID argument, and two columns for each sample named samp1 and samp2. The samp1 column is the first sample of the data. The samp2 column is the replicate sample of the site.
#'
#' @param sampling_sd If error_dist = 'normal', must specify the standard deviation  for the distribution. Otherwise leave blank.
#'
#' @param effect_size The range of effect sizes to test. The default is -50 to 50% change at 5% increments.
#'
#' @param sample_size The range of sample sizes to test. The default is 10 to 100 in increments of 10.
#'
#' @param num_reps Number of replicates to run in the bootstrap.
#'
#' @param chatty TRUE or FALSE. TRUE (default) will print progress in the console, including the first four characters
#' in the Plot_Name and a tick for every other replicate of the bootstrap. FALSE will not print progress in console.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange group_by left_join mutate select summarize
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
#'
power_sim <- function(data, y = NA, years = 1:5, ID = "Plot_Name", group = NA,
                      random_type = c("intercept", "slope"), sample = TRUE,
                      num_reps = 10, error_dist = c("nonpar", 'normal'),
                      sampling_data = NA, sampling_sd = NA, effect_size = seq(-50, 50, 5),
                      sample_size = seq(10, 100, 10),  chatty = TRUE,
                      sample_num = 1){

  if(!requireNamespace("pdqr", quietly = TRUE)){
    stop("Package 'pdqr' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(is.null(data)){stop("Must specify data to run function")}
  stopifnot(class(data) == "data.frame")
  stopifnot(!is.na(sampling_data) & class(sampling_data) == "data.frame")
  if(is.null(y)){stop("Must specify y variable to run function")}
  if(any(!is.null(sampling_data) & !c("samp1", "samp2") %in% names(sampling_data))){
    stop("The data.frame specified in sampling_data does not contain the required columns 'samp1' and 'samp2'")}
  stopifnot(all(is.numeric(years)))
  if(is.null(ID)){stop("Must specify ID variable to run function")}
  stopifnot(c(y, ID) %in% names(data))
  error_dist <- match.arg(error_dist)
  stopifnot(is.numeric(num_reps))

  if(chatty == TRUE){cat("Running bootstrap:", "\n")}

  # For error_dist = nonpar, create new distribution for sampling error
  sampling_data$diff <- sampling_data$samp1 - sampling_data$samp2

  rvar <- if(error_dist == 'nonpar'){
    new_r(sampling_data$diff, type = 'continuous')
  } else {rnorm(0, sampling_sd)}

  # Need to do 2 rounds of simulation. First, sample the data and simulate trends using case_boot_power times
  # the number of replicates and determine if there's a significant trend. Then repeat this the number of replicates
  # to calculate power as the percent of tests that were significant.

sim_full <- map_dfr(seq_len(num_reps), function(reps){
  sim_level1 <- map_dfr(seq_len(num_reps), function(n){
    cat(".")
   case_boot_power(data, y = y, years = years, ID = ID,
                   random_type = 'intercept', sample = T, #sample_num = x,
                   effect_size = effect_size, sample_size = sample_size,
                   error_dist = 'nonpar', sampling_data = sampling_data) %>%
     mutate(rep1 = n)}) # outside of sim level 1
    # Summarize and store output of sim_level1

  sim_level2 <- sim_level1 %>%
    mutate(rep2 = reps) %>%
     group_by(effect_size, sample_size, rep2) %>%
     summarize(est = mean(estimate, na.rm = T),
               lower95 = quantile(estimate, 0.025, na.rm = T),
               upper95 = quantile(estimate, 0.975, na.rm = T),
               sign_trend = ifelse(lower95 > 0 | upper95 < 0, 1, 0),
               num_sims = sum(!is.na(rep1)),
               num_boots_sing = sum(isSingular, na.rm = T),
                         .groups = 'drop')

})

head(sim_full)

  power_calc <- sim_full %>%
    group_by(effect_size, sample_size) %>%
    summarize(power_pct = sum(sign_trend, na.rm = T)/num_reps * 100,
              lower95 = mean(lower95, na.rm = T),
              upper95 = mean(upper95, na.rm = T),
              num_reps = num_reps,
              num_boots = sum(num_boots_sing),

              .groups = 'drop')


  head(power_calc)
  return(power_calc)

  #+ NEED TO FIGURE OUT HOW TO ADD TICKS FOR PROGRESS AND CHECK THAT REPLICATION IS WORKING
}
