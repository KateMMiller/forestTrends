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
#' @param x Quoted time variable for trend analysis. Default is "cycle", but can also model by year. Must be numeric.
#' @param y Quoted response variable in the data frame.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param model_type Options are "lmer" (Default) or "loess".
#' @param random_type Options are intercept or slope. Required if model type is "lmer". The intercept option (default) will
#' fit a random intercept on plot with (1|Plot_Name) as random component. The slope option will fit a random slope model
#' with (1 + cycle|Plot_Name).
#' @param span Numeric value that controls degree of smoothing. Used for model type "loess". If not specified and model type
#' is loess, an optimum span will be determined using fANCOVA::loess.as(). Note that user-specified spans are preferred. For
#' plots sampled every 4 years and year as the variable, span of 4 is appropriate. Similarly if cycle is used, span of 1 works well.
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
#' boot_ex <- case_boot_sample(fake_df, x = "cycle", y = "resp", ID = "Plot_Name",
#'   model_tyle = 'lmer', random_type = "intercept", sample = TRUE)
#'
#' boot_l <- case_boot_sample(fake_df, x = "cycle", y = "resp", ID = "Plot_Name",
#'   model_type = 'loess', span = 1, sample = TRUE)
#' }
#'
#' @export

case_boot_sample <- function(df, x = "cycle", y, ID = "Plot_Name", model_type = c("lmer", "loess"),
                             random_type = c("intercept", "slope"), span = NA_real_,
                             sample = TRUE, sample_num = 1){

  if(missing(df)){stop("Must specify df to run function")}
  if(missing(x)){stop("Must specify x variable to run function")}
  if(missing(y)){stop("Must specify y variable to run function")}
  if(missing(ID)){stop("Must specify ID variable to run function")}
  random_type <- match.arg(random_type)
  stopifnot(c(x, y, ID) %in% names(df))
  # stopifnot(is.numeric(df[,x]))
  # stopifnot(is.numeric(df[,y]))

  plots <- data.frame(Plot_Name = unique(df[,ID]))
  n <- nrow(plots)

  samp <- if(sample == TRUE){
    data.frame(Plot_Name = sample(plots$Plot_Name, n, replace = TRUE))
  } else {data.frame(plots)} %>%
    dplyr::arrange(Plot_Name)

  # set up unique naming column, so plots selected more than once have a unique ID.
  samp$case <- as.factor(stringr::str_pad(rownames(samp), nchar(n), side ="left", pad = 0))

  df_samp <- dplyr::left_join(samp, df[,c(ID, x, y)], by = c(ID)) %>%
    dplyr::arrange(case, x)

  mod <-
    if(model_type == "lmer"){
    suppressMessages(
      trend_fun(df_samp, x = x, y = y, ID = ID, random_type = random_type)) %>%
        dplyr::mutate(boot_num = ifelse(exists("sample_num"), sample_num, 1))
    } else if(model_type == "loess"){
      suppressMessages(trend_loess(df_samp, x = x, y = y, ID = ID, span = span))%>%
        dplyr::mutate(boot_num = ifelse(exists("sample_num"), sample_num, 1))
    }

  chatty <- ifelse(exists("chatty"), chatty, TRUE)
  sample_num <- ifelse(exists("sample_num"), sample_num, 1)

  if(chatty == TRUE & (sample_num %% 10) == 0){cat(".")} #prints tick every 10 reps

  return(mod)
}

