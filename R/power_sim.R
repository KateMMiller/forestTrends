#' @include case_boot_power.R
#'
#' @title power_sim: Calculates power for hierarchical models using simulation and case bootstrap.
#'
#' @description Fits models using a range of effect and samples sizes using non-parametric bootstrapping to assess significance.
#' For each level of effect size and sample size, returns the power to detect the trend, along with average lower and upper 95%
#' confidence intervals for each combination across the replicate bootstraps.
#'
#' @importFrom dplyr arrange group_by mutate summarize
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#'
#' @param data Data frame containing an ID column that identifies each sample unit (e.g., Plot_Name), a column containing a time
#' variable, and at least one column with a response variable.
#' @param y Quoted response variable in the data frame. Must be numeric.
#' @param years Vector of years to run simulation out to. Default is 1:5 years.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param random_type Specify "intercept" or "slope". The intercept option (default) will fit a random intercept model
#' with (1|ID) as random component. The slope option will fit a random slope model with (1 + year|ID) as the random component.
#' @param num_reps Number of replicate bootstraps to run for each level of effect and sample size. Default is 10 for faster
#' testing. However, 500-1000 is the better number for real analyses (note this could take many hours).
#' @param error_dist Either "nonpar" or "normal". If nonpar is chosen, must specify a dataset of repeated measures, like
#' from QA/QC sampling, to generate an error distribution based on the data. If normal is chosen, then a normal distribution
#' will be used. This will be used to add sampling error to simulated trends that are at an appropriate scale to the dataset.
#' @param sampling_data If error_dist = 'nonpar', specify a dataset that has repeated measures for sites. Otherwise leave blank.
#' The columns in the sampling data should include a unique ID column that is specified via the ID argument, and two columns
#' for each sample named samp1 and samp2. The samp1 column is the first sample of the data. The samp2 column is the replicate
#' sample of the site.
#' @param sampling_sd If error_dist = 'normal', must specify the standard deviation  for the distribution. Otherwise leave blank.
#' @param effect_size The range of effect sizes to test. The default is -50 to 50% change at 5% increments.
#' @param sample_size The range of sample sizes to test. The default is 10 to 100 in increments of 10.
#' @param chatty TRUE or FALSE. TRUE (default) will print progress in the console, including the first four characters
#' in the Plot_Name and a tick for every other replicate of the bootstrap. FALSE will not print progress in console.
#'
#' @examples
#' \dontrun{
#'
#'  # Generate fake datasets
#'  # sample data
#'  site = paste0("site-", sprintf("%02d", rep(1:30))) # vector of 30 site names
#'  y = runif(30) # random data for 30 sites
#'  yq = y[1:10] + rnorm(10, mean = 0, sd = 0.2) # qaqc data for first 30 sites generated
#'    #  by y0 value plus random sampling error
#'  ydiff <- y[1:10] - yq # vector of differences between initial y and qaqc y
#'
#'  # QAQC error distribution
#'  rvar <- pdqr::new_r(ydiff, type = 'continuous')
#'  rvar(1) #check that it works- should generate a random value from the distribution of ydiff
#'
#'  mean(rvar(1000)) # should be ~ 0 with fake dataset
#'  sd(rvar(1000)) # should be ~ 0.2 with fake dataset
#'
#'  # combine data frames
#'  dat <- data.frame(site = site, y = y, qaqc = FALSE) # original dataframe
#'  dat_qc <- data.frame(site = site[1:10], y = yq, qaqc = TRUE) # qaqc dataframe from first 10 sites
#'
#'  dat_qc_wide <- right_join(dat, dat_qc, by = "site", suffix = c("1", "2")) %>%
#'    rename(samp1 = y1, samp2 = y2)
#'
#'  # Run function
#'  # Non-parametric sampling error
#'  pwr_np <- power_sim(dat, y = 'y', ID = 'site', random_type = 'intercept',
#'              error_dist = 'nonpar', sampling_data = data_qc_wide,
#'              effect_size = seq(-20, 20, 5), sample_size = c(10, 25, 50, 100))
#'
#'  # Normal sampling error
#'  sim1 <- case_boot_power(dat, y = 'y', ID = 'site', random_type = 'intercept',
#'            error_dist = 'norma', sampling_sd = 0.2,
#'            effect_size = seq(-20, 20, 5), sample_size = c(10, 25, 50, 100))
#' }
#'
#' @return A dataframe that contains a row for every effect size and sample size combination (power combination) and the
#' power to detect trends. The process uses case_boot_power() to bootstrap the original dataset with replacement for each
#' sample size, with each bootstrap sample size equaling the sample size. For each of these datasets, trends are simulated
#' using the specified effect sizes plus random sampling error, with each year's trend based on the previous year's value,
#' rather than the starting point. This process is repeated num_reps number of times to generate a sampling distribution
#' of slope estimates that are then used to calculate 95% confidence intervals of the slope estimate. If the resulting
#' confidence intervals do not contain 0, they are considered significant. This is then repeated num_reps number of times
#' to calculate power, which is the percent of trends that are significant divided by num_reps.
#'
#' @export

power_sim <- function(data, y = NA, years = 1:5, ID = "Plot_Name", group = NA,
                      random_type = c("intercept", "slope"),
                      num_reps = 10, error_dist = c("nonpar", 'normal'),
                      sampling_data = NA, sampling_sd = NA, effect_size = seq(-50, 50, 5),
                      sample_size = seq(10, 100, 10),  chatty = TRUE){

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
  stopifnot(is.numeric(effect_size))
  stopifnot(is.numeric(sample_size))

  effect_size <- effect_size[effect_size != 0]

  if(chatty == TRUE){cat("Running bootstrap:", "\n")}

  # For error_dist = nonpar, create new distribution for sampling error
  sampling_data$diff <- sampling_data$samp1 - sampling_data$samp2

  rvar <- if(error_dist == 'nonpar'){
    pdqr::new_r(sampling_data$diff, type = 'continuous')
  } else {rnorm(0, sampling_sd)}

  # Need to do 2 rounds of simulation. First, sample the data and simulate trends using case_boot_power times
  # the number of replicates and determine if there's a significant trend. Then repeat this the number of replicates
  # to calculate power as the percent of tests that were significant.

  sim_full <- map_dfr(seq_len(num_reps), function(reps){
    if(chatty == TRUE){cat(reps, "out of", num_reps)}
    sim_level1 <- map_dfr(seq_len(num_reps), function(n){
    if(chatty == TRUE){
      cat(".")
      if(n == num_reps){cat(".Done", "\n")}
      }

     case_boot_power(data, y = y, years = years, ID = ID,
                     random_type = random_type,
                     effect_size = effect_size,
                     sample_size = sample_size,
                     error_dist = error_dist,
                     sampling_data = sampling_data,
                     sampling_sd = sampling_sd) %>%
       mutate(rep1 = n) # within level 1 replicate
      }) # outside of sim level 1

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

    }) # end of sim_full

  # calculate power
  power_calc <- sim_full %>%
    group_by(effect_size, sample_size) %>%
    summarize(power_pct = sum(sign_trend, na.rm = T)/num_reps * 100,
              mean_est = mean(est, na.rm = T),
              num_years = length(years),
              lower95 = mean(lower95, na.rm = T),
              upper95 = mean(upper95, na.rm = T),
              num_reps = num_reps,
              pct_singular = sum(num_boots_sing)/(num_reps*num_reps),
              .groups = 'drop')

  power_calc$effect_size <- as.numeric(
    paste0(ifelse(grepl("dec", power_calc$effect_size), "-", ""),
      gsub("ysim_dec", "",
      gsub("ysim_inc", "",
      power_calc$effect_size))))

  power_final <- power_calc %>% arrange(effect_size, sample_size)

  return(data.frame(power_final))
}
