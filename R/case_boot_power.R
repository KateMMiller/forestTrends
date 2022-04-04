#' @include trend_lmer.R
#'
#' @title case_boot_power: Samples original data and simulate trends over a range of effect and sample sizes
#'
#' @description Creates a bootstrapped sample of plots with replacement to simulate trends for a range of effect and sample sizes, and
#' returns model output for the specified effect, sample sizes and number of bootstraps specified by `num_reps`. Only enabled for lmer(). Internal function used in `power_sim()`.
#'
#' @importFrom purrr map_df map_dfc map_dfr map2_dfr
#' @importFrom dplyr case_when filter left_join mutate select
#' @importFrom tidyr pivot_wider
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
#' @param num_reps Number of replicate bootstraps to run for each level of effect and sample size. Default is 10 for faster testing. However, 500-1000 is the better number for real analyses. Will apply the `num_reps` that are specified by `power_sim()` when used internally to that function.
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
#' @param sample_num Used for iteration to indicate the replicate number of the bootstrap.
#' Do not need to specify if not running within case_boot_lmer().
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export

case_boot_power <- function(data, y = NA, years = 1:5, ID = "Plot_Name", group = NA,
                            random_type = c("intercept", "slope"),
                            sample = TRUE, num_reps = 10,
                            error_dist = c("nonpar", 'normal'),
                            sampling_data = NA, sampling_sd = NA, effect_size = seq(-50, 50, 5),
                            sample_size = seq(10, 100, 10)#, sample_num = 1
                            ){

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

  # Set up progress tracking
  chatty <- ifelse(exists("chatty"), chatty, TRUE)
  sample_num <- ifelse(exists("sample_num"), sample_num, 1)

  # For error_dist = nonpar, create new distribution for sampling error
  # This is actually performed in the power_sim() function, so it's only
  # generated once, instead of for each bootstrap, but I left it here too
  # in case this function is being used without power_sim()

  if(!exists('rvar')){
  sampling_data$diff <- sampling_data$samp1 - sampling_data$samp2

  rvar <- if(error_dist == 'nonpar'){
    new_r(sampling_data$diff, type = 'continuous')
  } else {rnorm(0, sampling_sd)}
  }

  # If years don't start at 1, rescale start at 1
  if(min(years) > 1){years <- years - min(years) + 1}

  # Set up bootstrap
  plots <- data.frame(ID = unique(data[,ID]))
  colnames(plots) <- "ID" # bug handling for purrr::map
  n <- nrow(plots) #<=== Use sample size and iterate through, if more than one specified.
  data$ID <- data[,ID]
  data <- data %>% select(ID, y) %>% mutate(year = 1)

  samp <- if(sample == TRUE){
    map_dfr(sample_size, function(x){
      data.frame(ID = sample(plots$ID, x, replace = TRUE)) %>%
                            mutate(sample_size = x)}
      )
  } else {data.frame(plots)} %>%
    dplyr::arrange(ID)

  # Set up unique naming column, so plots selected more than once have a unique ID.
  samp$case <- as.factor(stringr::str_pad(rownames(samp), nchar(n), side ="left", pad = 0))

  # Make sure nested variable in random effects is included in data_samp
  data_samp <- dplyr::left_join(samp, data, by = "ID") %>%
    dplyr::arrange(case)

  # Prepare vectors to iterate over
  sim_cols <- c(paste0("ysim", years[-1]))
  prev_cols <- c(y, sim_cols[1:length(sim_cols) - 1])
  es_cols <- paste0("ysim",
                    case_when(effect_size < 0 ~ "_dec",
                              effect_size == 0 ~ "_",
                              effect_size > 0 ~ "_inc"),
                    abs(effect_size))

  sim_mat <- expand.grid(sample_size = sample_size, effect_size = es_cols,
  #                       rep_num = 1:num_reps,
                         stringsAsFactors = F) %>% data.frame()

  # Build full wide dataset with years and number of samples for the for loop on simcols
  data_sim <- data_samp
  data_sim[, sim_cols] <- as.numeric(NA_real_)

  # Build long-version of data_sim to bind simulated columns to at end of for loop
  data_sim_long <- data_sim %>% select(-year) %>% rename(ysim1 = y) %>%
    pivot_longer(cols = -c(ID, sample_size, case),
                 names_to = "year",
                 values_to = "y") %>%
    mutate(year = as.numeric(substr(year, 5, 5))) %>% arrange(ID, sample_size, year) %>%
    select(-y) %>% right_join(., data_sim %>% select(ID, sample_size, case, y),
                              by = c("ID", "sample_size", "case"))

  # Nested for loop creates a dataframe that includes a column for each effect size, and simulates
  # trends by using the previous years trend, rather than time = 0 trend.
  for(es in effect_size){
    escol = paste0("ysim",
                   case_when(es < 0 ~ "_dec",
                             es == 0 ~ "_",
                             es > 0 ~ "_inc"),
                   abs(es))

  for(col in sim_cols){ # Vectorized approach to simulating consecutive trends based on previous year
    prevcol = which(names(data_sim) == col) - 1
    data_sim[, col] <- data_sim[,prevcol] +
       (data_sim[,prevcol] * (es/100)) + rvar(nrow(data_sim))
    es_dat <- data_sim %>% select(-year) %>% mutate(ysim1 = y) %>%
      pivot_longer(cols = c(ysim1, all_of(sim_cols)),
                   names_to = "year",
                   values_to = all_of(escol)) %>%
      mutate(year = as.numeric(substr(year, 5, 5))) %>% select(all_of(escol))

    data_sim_long[, escol] <- es_dat

    }
  }

  # Run the model for each sample size and effect size combination and number of reps
  boot_mod <-   map2_dfr(sim_mat[,1], sim_mat[,2],
                         function(sampsize, resp){
                           # repnum <-
                           #   (which(sim_mat$sample_size == sampsize)[1] +
                           #     which(sim_mat$effect_size == resp)[1] +
                           #     which(sim_mat$srep_num == sampnum)[1])
                           # if(chatty == TRUE & repnum %% 25 == 1){
                           #   cat(".")} #tick every 25th element

                           ss_dat <- data_sim_long %>% filter(sample_size == sampsize) %>%
                             select(case, sample_size, year, all_of(resp))
                           trend_lmer(ss_dat, x = 'year', y = resp, ID = "case",
                                      random_type = random_type) %>%
                           filter(term == "Slope") %>%
                           mutate(effect_size = resp,
                                  sample_size = sampsize)

                         })

  # if(chatty == TRUE){cat(".Done", "\n")}
  return(data.frame(boot_mod))
}

