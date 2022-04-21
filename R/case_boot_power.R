#' @include trend_lmer.R
#'
#' @title case_boot_power: Samples original data and simulate trends over a range of effect and sample sizes
#'
#' @description Creates a bootstrapped sample of plots with replacement to simulate
#' trends for a range of effect #' and sample sizes, and returns model output for
#' the specified effect, sample sizes and number of bootstraps specified by num_reps.
#' Only enabled for lmer(). Internal function used in power_sim().
#'
#' @importFrom dplyr filter left_join mutate right_join select slice_sample
#' @importFrom magrittr %>%
#' @importFrom stringr str_pad
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr map2_dfr
#'
#' @param data Data frame containing an ID column that identifies each sample unit
#' (e.g., Plot_Name), and at least one #' column with a response variable.
#' @param y Quoted response variable in the data frame. Must be numeric.
#' @param years Vector of years to run simulation out to. Default is 1:5 years.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name",
#' and assumes the first 4 characters #' are a park code.
#' @param random_type Specify "intercept" or "slope". The intercept option (default)
#' will fit a random intercept model with (1|ID) as random component. The slope option
#' will fit a random slope model with (1 + year|ID) as the random component.
#' @param num_reps Number of replicate bootstraps to run for each level of effect and
#' sample size. Default is 100 for faster testing. However, 500-1000 is the better number
#' for real analyses (note this could take many hours).
#' @param error_dist Either "nonpar" or "normal". If nonpar is chosen, must specify
#' a dataset of repeated measures, like from QA/QC sampling, to generate an error
#' distribution based on the data. If normal is chosen, then a normal distribution
#' will be used. This will be used to add sampling error to simulated trends that
#' are at an appropriate scale to the dataset.
#' @param sampling_data If error_dist = 'nonpar', specify a dataset that has repeated
#' measures for sites. Otherwise leave blank. The columns in the sampling data should
#' include a unique ID column that is specified via the ID argument, and two columns
#' for each sample named samp1 and samp2. The samp1 column is the first sample of
#' the data. The samp2 column is the replicate sample of the site.
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
#' @param save_data TRUE or FALSE. If TRUE, writes the simulated data to the working directory.
#' Helpful for troubleshooting.
#'
#' @examples
#' \dontrun{
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
#'  #--- Run function
#'  # Non-parametric sampling error
#'  sim_np <- forestTrends::case_boot_power(dat, y = 'y', ID = 'site', random_type = 'intercept',
#'              error_dist = 'nonpar', sampling_data = dat_qc_wide, pos_val = TRUE,
#'              effect_size = seq(-20, 20, 5), sample_size = c(10, 25, 50, 100), num_reps = 100)
#'
#'  # Normal sampling error allowing negative simulated values
#'  sim_norm <- forestTrends::case_boot_power(dat, y = 'y', ID = 'site', random_type = 'intercept',
#'                error_dist = 'normal', sampling_sd = 0.2, pos_val = FALSE,
#'                effect_size = seq(-20, 20, 5), sample_size = c(10, 25, 50, 100), num_reps = 100)
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
                            effect_size = seq(-50, 50, 5), sample_size = seq(10, 100, 10),
                            pos_val = TRUE, upper_val = NA, save_data = FALSE,
                            num_reps = 100#, chatty = TRUE
                            ){

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
  stopifnot(all(is.numeric(sample_size) & !is.na(sample_size)))
  stopifnot(all(is.numeric(effect_size) & !is.na(effect_size)))
  if(is.na(ID)){stop("Must specify ID variable to run function")}
  stopifnot(c(y, ID) %in% names(data))
  error_dist <- match.arg(error_dist)
  stopifnot(is.numeric(upper_val) | is.na(upper_val))

  # For error_dist = nonpar, create new distribution for sampling error as a percentage.
  # This is actually performed in the power_sim() function, so it's only
  # generated once, instead of for each bootstrap, but I left it here
  # in case this function is being used without power_sim()

  if(!exists('rvar')){
    rvar <- if(error_dist == 'nonpar'){
      sampling_data$diff <- (abs(sampling_data$samp1 - sampling_data$samp2))/
        ((sampling_data$samp1 + sampling_data$samp2)/2)
      function(n){fishmethods::remp(n, c(sampling_data$diff, -sampling_data$diff))}
    } else {function(n){rnorm(n, 0, sampling_sd)}} #need to specify # values to generate
    }

  # If years don't start at 1, rescale start at 1
  if(min(years) > 1){years <- years - min(years) + 1}

  # Set up bootstrap
  plots <- data.frame(ID = unique(data[,ID]))
  #colnames(plots) <- "ID" # bug handling for purrr::map
  n <- nrow(plots) # for strpad
  data$ID <- data[,ID]
  data <- data %>% select(ID, y) %>% mutate(year = 1)

  # Sample dataset with replacement to be the maximum sample size. This will be sliced by the smaller
  # sample sizes later to improve performance.
  samp_max <- slice_sample(data, n = max(sample_size, na.rm = T), replace = T)

  # Set up unique naming column, so plots selected more than once have a unique ID.
  samp_max$case <- as.factor(stringr::str_pad(rownames(samp_max), nchar(n), side ="left", pad = 0))
  samp_max$sample_size <- max(sample_size, na.rm = T)

  # Prepare vectors to iterate over
  sim_cols <- c(paste0("ysim", years[-1]))

  es_cols <- paste0("ysim",
                    ifelse(effect_size < 0, "_dec",
                           ifelse(effect_size > 0, "_inc", "_")), #nested ifelse faster than case_when
                    abs(effect_size))

  sim_mat <- expand.grid(sample_size = sample_size, effect_size = es_cols,
                         stringsAsFactors = F) %>% data.frame()

  # Build full wide dataset with years and number of samples for the for loop on simcols
  data_sim <- samp_max #data_samp
  data_sim[, sim_cols] <- as.numeric(NA_real_)
  data_sim <- data_sim %>% #mutate(ysim1 = y) %>%
    select(ID, year, case, sample_size, ysim1 = y, everything())

  # Build long-version of data_sim to bind simulated columns to at end of for loop
  # tried fill instead, but it's slower than this approach
  data_sim_long <- data_sim %>% select(-year) %>%
    pivot_longer(cols = -c(ID, sample_size, case),
                 names_to = "year",
                 values_to = "y") %>%
    mutate(year = as.numeric(substr(year, 5, 7))) %>% # up to 3 digit number of years
    right_join(., data_sim %>% select(ID, sample_size, case, ysim1),
               by = c("ID", "sample_size", "case")) %>% select(-y)

  # Nested for loop creates a dataframe that includes a column for each effect size, and simulates
  # trends by using the previous years trend, rather than time = 0 trend.
#
#    es <- effect_size[1]
#    col = sim_cols[1]
  for(es in effect_size){
    escol = paste0("ysim",
                   ifelse(es < 0, "_dec",
                          ifelse(es > 0, "_inc", "_")),
                   abs(es))

    linear_pred <- data_sim$ysim1 * (es/100)

    for(col in sim_cols){ # Vectorized approach to simulating consecutive trends based on previous year
      prevcol = which(names(data_sim) == col) - 1

      # data_sim[, col] <- data_sim[, prevcol] + linear_pred +
      #   (rvar(nrow(data_sim))/length(years))*data_sim[, prevcol] # error added as percent of prev. value/ # years

      data_sim[,col] <- (data_sim[, prevcol]+ linear_pred) * (1 + rvar(nrow(data_sim))/length(years))

      # Convert negative sim values to 0 if specified
      if(pos_val == TRUE){
        data_sim[, col][data_sim[, col] < 0] <- 0
      }

      if(!is.na(upper_val)){
        data_sim[, col][data_sim[, col] > upper_val] <- upper_val
      }

      }

    es_dat <- data_sim %>% select(-year) %>% #mutate(ysim1 = y) %>%
      pivot_longer(cols = c(ysim1, all_of(sim_cols)),
                   names_to = "year",
                   values_to = all_of(escol)) %>%
      mutate(year = as.numeric(substr(year, 5, 5))) %>%
      select(all_of(escol))

    data_sim_long[, escol] <- es_dat
  }


  # rbind slices dataset with nrow = max(sample_size) into smaller sample sizes
  sample_size_slices <- sample_size[sample_size < (max(sample_size))]

  full_dat <- rbind(data_sim_long,
                    map_dfr(sample_size_slices, function(n){
                      case_list <- unique(sort(data_sim_long$case))[1:n] %>% droplevels()
                      data_slice <- data_sim_long %>% filter(case %in% case_list) %>%
                        mutate(sample_size = n)})
  )

  if(save_data == TRUE){write.csv(full_dat,
                          paste0("simulated_dataset", y, "_",
                                 error_dist, "_", random_type, ".csv"), row.names = F)}

  # Run case bootstrap to determine if there's a significant trend for each n x es comb.
  boot_mod <- map2_dfr(sim_mat[,1], sim_mat[,2], .id = 'boot', #.progress = chatty,
              #                       .options = furrr::furrr_options(seed = TRUE),
                       function(sampsize, resp){
                         ss_dat <- full_dat %>% filter(sample_size == sampsize) %>%
                           select(case, sample_size, year, all_of(resp))

                         iter <- as.numeric(rownames(sim_mat[sim_mat$sample_size == sampsize &
                                                               sim_mat$effect_size == resp,]))

                         mod <- case_boot_lmer(ss_dat, x = 'year', y = resp, ID = 'case',
                                               num_reps = num_reps, random_type = random_type,
                                               chatty = FALSE)

                         mod2 <- mod %>% filter(term == "Slope") %>%
                           mutate(sample_size = sampsize,
                                  effect_size = resp,
                                  signif = ifelse(round(lower95, 4) > 0 | round(upper95, 4) < 0, 1, 0))
                       })

  return(data.frame(boot_mod))
}

