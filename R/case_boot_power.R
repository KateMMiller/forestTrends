#' @include trend_lmer.R
#'
#' @title case_boot_power: Samples original data and simulate trends over a range of effect and sample sizes
#'
#' @description Creates a bootstrapped sample of plots with replacement to simulate trends for a range of effect
#' and sample sizes, and returns model output for the specified effect, sample sizes and number of bootstraps
#' specified by num_reps. Only enabled for lmer(). Internal function used in power_sim().
#'
#' @importFrom dplyr filter left_join mutate right_join select
#' @importFrom purrr map_dfr map2_dfr
#' @importFrom magrittr %>%
#' @importFrom stringr str_pad
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_longer
#'
#' @param data Data frame containing an ID column that identifies each sample unit (e.g., Plot_Name), and at least one
#' column with a response variable.
#' @param y Quoted response variable in the data frame. Must be numeric.
#' @param years Vector of years to run simulation out to. Default is 1:5 years.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param random_type Specify "intercept" or "slope". The intercept option (default) will fit a random intercept model
#' with (1|ID) as random component. The slope option will fit a random slope model with (1 + year|ID) as the random component.
#' @param error_dist Either "nonpar" or "normal". If nonpar is chosen, must specify a dataset of repeated measures, like
#' from QA/QC sampling, to generate an error distribution based on the data. If normal is chosen, then a normal distribution
#' will be used. This will be used to add sampling error to simulated trends that are at an appropriate scale to the dataset.
#' @param sampling_data If error_dist = 'nonpar', specify a dataset that has repeated measures for sites. Otherwise leave blank.
#' The columns in the sampling data should include a unique ID column that is specified via the ID argument, and two columns
#' for each sample named samp1 and samp2. The samp1 column is the first sample of the data. The samp2 column is the replicate
#' sample of the site.
#' @param sampling_sd If error_dist = 'normal', must specify the standard deviation  for the distribution. Otherwise leave blank.
#' @param effect_size The range of effect sizes to test. The default is -50 to 50\% change at 5\% increments.
#' @param sample_size The range of sample sizes to test. The default is 10 to 100 in increments of 10.
#'
#' @examples
#' \dontrun{
#'  ### Generate fake datasets
#'  # sample data
#'  site = paste0("site.", sprintf("\%02d", rep(1:30))) # vector of 30 site names
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
#'  dat_qc_wide <- dplyr::right_join(dat, dat_qc, by = "site", suffix = c("1", "2")) \%>\%
#'    rename(samp1 = y1, samp2 = y2)
#'
#'  ### Run function
#'  # Non-parametric sampling error
#'  sim_np <- forestTrends::case_boot_power(dat, y = 'y', ID = 'site', random_type = 'intercept',
#'              error_dist = 'nonpar', sampling_data = dat_qc_wide,
#'              effect_size = seq(-20, 20, 5), sample_size = c(10, 25, 50, 100))
#'
#'  # Normal sampling error
#'  sim_norm <- forestTrends::case_boot_power(dat, y = 'y', ID = 'site', random_type = 'intercept',
#'                error_dist = 'normal', sampling_sd = 0.2,
#'                effect_size = seq(-20, 20, 5), sample_size = c(10, 25, 50, 100))
#' }
#'
#'
#' @return a data.frame with simulated trends
#'
#' @export

case_boot_power <- function(data, y = NA, years = 1:5, ID = "Plot_Name",
                            random_type = c("intercept", "slope"),
                            error_dist = c("nonpar", 'normal'),
                            sampling_data = NA, sampling_sd = NA,
                            effect_size = seq(-50, 50, 5),
                            sample_size = seq(10, 100, 10)
                            ){

  if(!requireNamespace("pdqr", quietly = TRUE)){
    stop("Package 'pdqr' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(is.null(data)){stop("Must specify data to run function")}
  stopifnot(is.data.frame(data))
  stopifnot(is.na(sampling_data) | is.data.frame(sampling_data))
  if(is.null(y)){stop("Must specify y variable to run function")}
  if(!is.na(sampling_data) && !c("samp1", "samp2") %in% names(sampling_data)){
    stop("The data.frame specified in sampling_data does not contain the required columns 'samp1' and 'samp2'")}
  stopifnot(all(is.numeric(years)))
  if(is.null(ID)){stop("Must specify ID variable to run function")}
  stopifnot(c(y, ID) %in% names(data))
  error_dist <- match.arg(error_dist)

  #sample_num <- ifelse(exists("sample_num"), sample_num, 1)

  # For error_dist = nonpar, create new distribution for sampling error
  # This is actually performed in the power_sim() function, so it's only
  # generated once, instead of for each bootstrap, but I left it here too
  # in case this function is being used without power_sim()

  if(!exists('rvar')){
    rvar <- if(error_dist == 'nonpar'){
    sampling_data$diff <- sampling_data$samp1 - sampling_data$samp2
    pdqr::new_r(sampling_data$diff, type = 'continuous')
  } else {rnorm(0, sampling_sd)}
  }

  # If years don't start at 1, rescale start at 1
  if(min(years) > 1){years <- years - min(years) + 1}

  # Set up bootstrap
  plots <- data.frame(ID = unique(data[,ID]))
  colnames(plots) <- "ID" # bug handling for purrr::map
  n <- nrow(plots) # for strpad
  data$ID <- data[,ID]
  data <- data %>% select(ID, y) %>% mutate(year = 1)

  samp <-
    map_dfr(sample_size, function(x){
      data.frame(ID = sample(plots$ID, x, replace = TRUE)) %>%
                             mutate(sample_size = x)}
      )

  # Set up unique naming column, so plots selected more than once have a unique ID.
  samp$case <- as.factor(stringr::str_pad(rownames(samp), nchar(n), side ="left", pad = 0))

  # Make sure nested variable in random effects is included in data_samp
  data_samp <- dplyr::left_join(samp, data, by = "ID")

  # Prepare vectors to iterate over
  sim_cols <- c(paste0("ysim", years[-1]))
  es_cols <- paste0("ysim",
                    ifelse(effect_size < 0, "_dec", "_inc"),
                    abs(effect_size))

  sim_mat <- expand.grid(sample_size = sample_size, effect_size = es_cols,
                         stringsAsFactors = F) %>% data.frame()

  # Build full wide dataset with years and number of samples for the for loop on simcols
  data_sim <- data_samp
  data_sim[, sim_cols] <- as.numeric(NA_real_)

  # Build long-version of data_sim to bind simulated columns to at end of for loop
  data_sim_long <- data_sim %>% select(-year) %>% rename(ysim1 = y) %>%
    pivot_longer(cols = -c(ID, sample_size, case),
                 names_to = "year",
                 values_to = "y") %>%
    mutate(year = as.numeric(substr(year, 5, 5))) %>%
    select(-y) %>%
    right_join(., data_sim %>% select(ID, sample_size, case, y),
               by = c("ID", "sample_size", "case"))

  # Nested for loop creates a dataframe that includes a column for each effect size, and simulates
  # trends by using the previous years trend, rather than time = 0 trend.
  for(es in effect_size){
    escol = paste0("ysim",
                   ifelse(es < 0, "_dec", "_inc"),
                   abs(es))

  for(col in sim_cols){ # Vectorized approach to simulating consecutive trends based on previous year
    prevcol = which(names(data_sim) == col) - 1

    data_sim[, col] <- data_sim[, prevcol] +
       (data_sim[,prevcol] * (es/100)) + rvar(nrow(data_sim))

    es_dat <- data_sim %>% select(-year) %>% mutate(ysim1 = y) %>%
      pivot_longer(cols = c(ysim1, all_of(sim_cols)),
                   names_to = "year",
                   values_to = all_of(escol)) %>%
      mutate(year = as.numeric(substr(year, 5, 5))) %>%
      select(all_of(escol))

    data_sim_long[, escol] <- es_dat

    }
  }

  # Run the model for each sample size and effect size combination and number of reps
  boot_mod <-   map2_dfr(sim_mat[,1], sim_mat[,2],
                  function(sampsize, resp){
                     ss_dat <- data_sim_long %>%
                       filter(sample_size == sampsize) %>%
                       select(case, sample_size, year, all_of(resp))
                       trend_lmer(ss_dat, x = 'year', y = resp, ID = "case",
                                  random_type = random_type) %>%
                       filter(term == "Slope") %>%
                       mutate(effect_size = resp,
                              sample_size = sampsize)
                     })

  return(data.frame(boot_mod))
}

