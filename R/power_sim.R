#' @include case_boot_power.R
#'
#' @title power_sim: Calculates power using simulation and case bootstrapping for hierarchical models.
#'
#' @description  Calculates power for range of effect and samples sizes using non-parametric bootstrapping to assess significance.
#' Only enabled for lmer().
#'
#' @importFrom purrr map_df
#' @importFrom dplyr filter left_join select
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
#' @param sample TRUE or FALSE. TRUE (default) will generate a bootstrapped sample of the specified data frame. FALSE will
#' run trend_lmer() on the original dataset.
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
#' @importFrom dplyr arrange left_join mutate
#' @importFrom stringr str_pad
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
                      error_dist = c("nonpar", 'normal'),
                      sampling_data = NA, sampling_sd = NA, effect_size = seq(-50, 50, 5),
                      sample_size = seq(10, 100, 10), num_reps = 10, chatty = TRUE,
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
  # vectors of data

  sampling_data$diff <- sampling_data$samp1 - sampling_data$samp2

  rvar <- if(error_dist == 'nonpar'){
    new_r(sampling_data$diff, type = 'continuous')
  } else {rnorm(0, sampling_sd)}

  boot_mods <- map_dfr(seq_len(num_reps),
                       function(x){
                       if(chatty == TRUE){cat(x, "out of", num_reps, "\n")}
                       case_boot_power(data, y = y, years = years, ID = ID,
                                        random_type = 'intercept', sample = T, sample_num = x,
                                        effect_size = effect_size, sample_size = sample_size,
                                        error_dist = 'nonpar', sampling_data = sampling_data)

                       })

  boot_mods_wide <- boot_mods %>% select(-term, -isSingular) %>%
    pivot_wider(names_from = c(effect_size, sample_size),
                                              values_from = estimate) %>% data.frame()

  boot_CIs <- data.frame(t(apply(boot_mods_wide %>% select(-boot_num) , 2,
                                 quantile, probs = c(0.025, 0.975), na.rm = T)))#,
                         # num_boots = sum((ifelse(is.na(boot_mods$isSingular), 0, 1))),
                         # num_boots_sing = sum(boot_mods$isSingular, na.rm = T))
  boot_CIs$comb = rownames(boot_CIs)

  head(boot_CIs)

  #+++++ ENDED HERE. NEED TO ADD BACK IN THE NUMBER OF SUCCESSFUL BOOTSTRAPS AND ISSINGULAR FOR FINAL
  #+++++ THEN ADD COLUMN OF EFFECT SIZE, SAMPLE SIZE AND CALCULATE POWER.
#  head(boot_CIs)
#
#   num_boot_sing <- unique(boot_CIs$num_boots_sing)
#   num_boot_fail <- num_reps - max(boot_CIs$num_boots)


}
