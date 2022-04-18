#' @include case_boot_power.R
#'
#' @title power_sim: Calculates power for hierarchical models using simulation and case bootstrap
#'
#' @description Fits models using a range of effect and samples sizes using non-parametric
#' bootstrapping to assess significance. For each level of effect size and sample size,
#' returns the power to detect the trend, along with average lower and upper 95\% confidence
#' intervals for each combination across the replicate bootstraps.
#'
#' @importFrom dplyr arrange first group_by mutate summarize
#' @importFrom magrittr %>%
#'
#' @param data Data frame containing an ID column that identifies each sample unit
#' (e.g., Plot_Name), and at least one column with a response variable.
#' @param y Quoted response variable in the data frame. Must be numeric.
#' @param years Vector of years to run simulation out to. Default is 1:5 years.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name",
#' and assumes the first 4 characters
#' are a park code.
#' @param random_type Specify "intercept" or "slope". The intercept option (default)
#' will fit a random intercept model
#' with (1|ID) as random component. The slope option will fit a random slope model
#' with (1 + year|ID) as the random component.
#' @param num_reps Number of replicate bootstraps to run for each level of effect
#' and sample size. Default is 100 for faster testing. However, 500-1000 is the
#' better number for real analyses (note this could take many hours).
#' @param num_pwr_reps Number of replicates to run for each level of effect
#' and sample size to calculate power. Default is 100 for faster testing.
#' However, 250+ is the better number for real analyses (note this could take many hours).
#' @param error_dist Either "nonpar" or "normal". If nonpar is chosen, must specify
#' a dataset of repeated measures, like from QA/QC sampling, to generate an error
#' distribution based on the data. If normal is chosen, then a normal distribution
#' will be used. This will be used to add sampling error to simulated trends that
#' are at an appropriate scale to the dataset.
#' @param sampling_data If error_dist = 'nonpar', specify a dataset that has
#' repeated measures for sites. Otherwise leave blank. The columns in the sampling
#' data should include a unique ID column that is specified via the ID argument,
#' and two columns for each sample named samp1 and samp2. The samp1 column is
#' the first sample of the data. The samp2 column is the replicate sample of the site.
#' @param var_hist TRUE or FALSE. If TRUE, plots a histogram of 1e6 calls from the random
#' variance function. If FALSE (default) doesn't plot anything.
#' @param sampling_sd If error_dist = 'normal', must specify the standard deviation
#' for the distribution. Otherwise leave blank.
#' @param effect_size The range of effect sizes to test. The default is -50 to 50\%
#' change at 5\% increments. Effect sizes represent the percent change per time step
#' rather than change across the entire study. If 0 is included, the resulting power
#' reflects the false positive rate.
#' @param pos_val TRUE (default) or FALSE. If TRUE, any simulated value that is negative
#' will be converted to 0. If FALSE, negative simulated values will be allowed.
#' @param upper_val Use this to specify a ceiling to the data. For example, if data
#' are percents and can't be greater than 1.0, then upper_val = 1. Otherwise leave blank.
#' @param sample_size The range of sample sizes to test. The default is 10 to 100
#' in increments of 10.
#' @param chatty TRUE or FALSE. TRUE (default) will print progress in the console,
#' including the number of the power sample currently running and a tick for every
#' replicate within the power bootstrap. FALSE will not print progress in console.
#' @param parallel TRUE or FALSE. If TRUE, power simulation will use parallel
#' processing across all but two of the machine's total number of cores.
#' @examples
#' \dontrun{
#'
#'  #--- Generate fake datasets
#'  # sample data
#'  site = paste0("site.", sprintf("%02d", rep(1:30))) # vector of 30 site names
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
#'  dat_qc_wide <- dplyr::right_join(dat, dat_qc, by = "site", suffix = c("1", "2")) %>%
#'    rename(samp1 = y1, samp2 = y2)
#'
#'  #-- Run function
#'  # Non-parametric sampling error
#'  pwr_np <- forestTrends::power_sim(dat, y = 'y', ID = 'site', random_type = 'intercept',
#'              error_dist = 'nonpar', sampling_data = dat_qc_wide, num_reps = 100,
#'              effect_size = seq(-20, 20, 5), pos_val = TRUE, var_hist = TRUE,
#'              sample_size = c(10, 25, 50, 100))
#'
#'  # Normal sampling error that allows negative simulated values
#'  pwr_norm <- forestTrends::power_sim(dat, y = 'y', ID = 'site', random_type = 'intercept',
#'                error_dist = 'normal', sampling_sd = 0.2, num_reps = 100,
#'                effect_size = seq(-20, 20, 5), pos_val = FALSE, var_hist = TRUE,
#'                sample_size = c(10, 25, 50, 100))
#' }
#'
#' @return A data frame that contains a row for every effect size and sample size
#' combination (power combination) and the power to detect trends. The process uses
#' case_boot_power() to bootstrap the original dataset with replacement for each
#' sample size, with each bootstrap sample size equaling the specified sample size.
#' For each of these datasets, trends are simulated using the specified effect sizes
#' plus random sampling error, with each year's trend based on the previous year's
#' value, rather than the starting point. Then case_boot_lmer() is used to determine
#' whether there's a significant trend for each power combination. Finally, this process
#' is repeated num_reps number of times to calculate power, which is the percent
#' of trends that are significant divided by the num_reps.
#'
#' @export

power_sim <- function(data, y = NA, years = 1:5, ID = "Plot_Name",
                      random_type = c("intercept", "slope"),
                      num_reps = 100, num_pwr_reps = 100, error_dist = c("nonpar", 'normal'),
                      sampling_data = NA, sampling_sd = NA, var_hist = FALSE,
                      effect_size = seq(-50, 50, 5), pos_val = TRUE, upper_val = NA,
                      sample_size = seq(10, 100, 10), chatty = TRUE, parallel = TRUE){

  if(!requireNamespace("fishmethods", quietly = TRUE)){
    stop("Package 'fishmethods' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(is.null(data)){stop("Must specify data to run function")}
  stopifnot("data.frame" %in% class(data))
  stopifnot(is.na(sampling_data) | is.data.frame(sampling_data))
  if(is.na(y)){stop("Must specify y variable to run function")}
  if(!is.na(sampling_data) && !c("samp1", "samp2") %in% names(sampling_data)){
    stop("The data.frame specified in sampling_data does not contain the required columns 'samp1' and 'samp2'")}
  stopifnot(all(is.numeric(years) & !is.na(years)))
  if(is.na(ID)){stop("Must specify ID variable to run function")}
  stopifnot(c(y, ID) %in% names(data))
  error_dist <- match.arg(error_dist)
  stopifnot(is.numeric(num_reps))
  stopifnot(all(is.numeric(sample_size) & !is.na(sample_size)))
  stopifnot(all(is.numeric(effect_size) & !is.na(effect_size)))
  stopifnot(is.numeric(upper_val) | is.na(upper_val))

  #effect_size <- effect_size[effect_size != 0]

  sample_num <- ifelse(exists("sample_num"), sample_num, 1) # for case_boot_lmer()
  if(chatty == TRUE){cat("Starting power simulation", "\n")}

  # For error_dist = nonpar, create new distribution for sampling error as a percentage
  # QAQC data often has bias in 2nd sample, so function randomly assigns sign to
  # remove bias.
  rvar <- if(error_dist == 'nonpar'){
    sampling_data$diff <- (abs(sampling_data$samp1 - sampling_data$samp2))/
      ((sampling_data$samp1 + sampling_data$samp2)/2)
    function(n){fishmethods::remp(n, c(sampling_data$diff, -sampling_data$diff))}
  } else {function(n){rnorm(n, 0, sampling_sd)}}

  if(var_hist == TRUE){hist(rvar(1e5), main = "Histogram of error function")}
  # code is considerably slower for 1e6

  # First, sample the data, simulate trends, and determine if significant
  # using case_boot_power. Repeat process num_reps number of times
  # to calculate power as the percent of tests that were significant.

  if(parallel == TRUE){
    num_workers <- as.numeric(length(future::availableWorkers()) - 2)
    future::plan(future::multisession, gc = TRUE,
                                    workers = num_workers)
  }

  sim_mod <- furrr::future_map_dfr(seq_len(num_pwr_reps),
               #.id = 'pwr_rep',
               .options = furrr::furrr_options(seed = TRUE),
               .progress = chatty,
    function(reps){
      #if(chatty == TRUE){cat(reps, "out of", num_pwr_reps)}

        case_boot_power(data, y = y, years = years, ID = ID,
                        random_type = random_type,
                        num_reps = num_reps,
                        effect_size = effect_size,
                        sample_size = sample_size,
                        error_dist = error_dist,
                        sampling_data = sampling_data,
                        sampling_sd = sampling_sd,
                        pos_val = pos_val,
                        upper_val = upper_val) %>%
        mutate(pwr_rep = reps)
  })

  # clean up columns
  sim_mod2 <- sim_mod %>%
    mutate(effect_size = as.numeric(paste0(
      ifelse(grepl("dec", sim_mod$effect_size), "-", ""),
           gsub("ysim_", "",
              gsub("dec", "",
                gsub("inc", "",
                     sim_mod$effect_size))))),

      signif_cor = ifelse(effect_size < 0 & upper95 < 0 & signif == 1 |
                            effect_size > 0 & lower95 > 0 & signif == 1, 1, 0),
      false_pos = ifelse(effect_size == 0 & signif == 1, 1, 0))


  # calculate power
  power_calc <- sim_mod2 %>%
    group_by(effect_size, sample_size) %>%
    summarize(power_pct = sum(signif_cor, na.rm = T)/sum(!is.na(pwr_rep)) * 100,
              #num_boots instead of num_reps in case some boots fail to return results
              false_pos_pct = sum(false_pos, na.rm = T)/sum(!is.na(pwr_rep)) * 100,
              incor_trend_pct = (sum(signif, na.rm = T) -
                                   sum(signif_cor, na.rm = T) -
                                         sum(false_pos, na.rm = T))/
                sum(!is.na(pwr_rep)) * 100,
              mean_est = mean(estimate, na.rm = T),
              num_years = length(years),
              lower95 = mean(lower95, na.rm = T),
              upper95 = mean(upper95, na.rm = T),
              num_boots = num_reps,
              num_pwr_reps = sum(!is.na(pwr_rep)),
              .groups = 'drop')


  power_final <- power_calc %>% arrange(effect_size, sample_size)

  return(data.frame(power_final))
}
