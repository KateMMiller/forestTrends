#' @include trend_fun.R
#'
#' @title case_boot_sample: bootstrap resampling function
#'
#' @description Bootstrap resampling function that creates a bootstrapped sample of plots with replacement and
#' returns model output from trend_fun() for that sample. This is mostly an internal function run within case_boot_lmer(),
#' but can be used as a stand alone function.
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable.
#' @param y Quoted response variable in the data frame.
#' @random_type intercept or slope. The intercept option (default) will fit a random intercept on plot with (1|Plot_Name) as
#' random component. The slope option will fit a random slope model with (1 + cycle|Plot_Name)
#' @param sample TRUE or FALSE. TRUE (default) will generate a bootstrapped sample of the specified data frame. FALSE will
#' run trend_fun() on the original dataset.
#' @param sample_num Used for iteration to indicate the replicate number of the bootstrap. Do not need to specify if not
#' running within case_boot_lmer().
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange left_join mutate
#' @importFrom stringr str_pad
#'
#' @examples
#' \dontrun{
#'
#' fake_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
#'                       cycle = rep(1:3, times = 9),
#'                       resp = runif(27, 0, 20))
#'
#' boot_ex <- case_boot_sample(fake_df, y = "resp", sample = TRUE)
#'
#' }
#'
#' @export

case_boot_sample <- function(df, y, random_type = c('intercept', 'slope'), sample = TRUE, sample_num = 1){

  if(!"Plot_Name" %in% names(df)){stop('Must have column named "Plot_Name" to run function')}
  if(!"cycle" %in% names(df)){stop('Must have column named "cycle" to run function')}
  if(missing(y)){stop("Must specify y variable to run function")}
  random_type <- match.arg(random_type)

  plots <- data.frame(Plot_Name = unique(df$Plot_Name))
  n <- nrow(plots)

  samp <- if(sample == TRUE){
    data.frame(Plot_Name = sample(plots$Plot_Name, n, replace = TRUE))
  } else {data.frame(plots)} %>%
    dplyr::arrange(Plot_Name)

  # set up unique naming column, so plots selected more than once have a unique ID.
  samp$case <- as.factor(stringr::str_pad(rownames(samp), nchar(n), side ="left", pad = 0))

  df_samp <- dplyr::left_join(samp, df[,c("Plot_Name", "cycle", y)], by = c("Plot_Name")) %>%
    dplyr::arrange(Plot_Name, case, cycle)

  mod <- suppressMessages(trend_fun(df_samp, y = y, random_type = random_type)) %>%
    dplyr::mutate(boot_num = ifelse(exists("sample_num"), sample_num, 1))

  chatty <- ifelse(exists("chatty"), chatty, TRUE)
  sample_num <- ifelse(exists("sample_num"), sample_num, 1)

  if(chatty == TRUE & (sample_num %% 10) == 0){cat(".")} #prints tick every 10 reps

  return(mod)
}

