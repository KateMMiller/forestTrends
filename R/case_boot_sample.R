#' @include trend_lmer.R
#'
#' @title case_boot_sample: bootstrap resampling function
#'
#' @description Bootstrap resampling function that creates a bootstrapped sample of plots with replacement and
#' returns model output from trend_lmer() or trend_loess() for that sample. This is mostly an internal function run within
#' case_boot_lmer() or case_boot_loess(), but can be used as a stand alone function.
#'
#' @param df Data frame containing an ID column that identifies each sample unit (e.g., Plot_Name), a column containing a time
#' variable, and at least one column with a response variable.
#' @param x Quoted time variable for trend analysis. Default is "cycle", but can also model by year. Must be numeric.
#' @param y Quoted response variable in the data frame. Must be numeric.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param group Including a group variable, like "Unit_ID", will print that group to show progress in the console.
#' If not specified, will print the first 4 characters of the ID to the console, assuming the ID starts with a 4-letter park code.
#' @param model_type Options are "lmer" (Default) or "loess".
#' @param random_type For model_type = "lmer", specify "intercept", "slope", or "custom". The intercept option (default) will fit a random intercept model
#' with (1|ID) as random component. The slope option will fit a random slope model with (1 + x|ID) as the random component.
#' If "custom" is used, must also specify random_formula.
#' @param random_formula If random_type = "custom", specify the random effects formula for the model in quotes. Otherwise leave blank.
#' @param nest_var Quoted column name containing the higher level grouping variable for a nested random effect.
#' @param span numeric value that controls the degree of smoothing. Smaller values (e.g., 0.1) result in less smoothing,
#' and possibly over-fitting the curve. Higher values (e.g., 0.9) result is more smoothing and possibly under-fitting.
#' You can calculate the number of time steps to include in the smoothing window by dividing p/n, where p is number of plots
#' you want to be included per window and n is number of timesteps in the data. When plotting years, knowing that panels include
#' 4 years, it is generally safe to assume a linear response between 2 full cycles, and therefore use a span of 8/n. Note that
#' if you specify degree = 1, then loess assumes a linear relationship within each span. If no span is specified, then
#' fANCOVA::loess.as() will be used to determine the optimum span (Note: user specified is preferred).
#' @param degree order of polynomial to fit. Values of 1 (Default) is linear, 2 is quadratic, etc. Degrees of 1 or 2 are
#' generally recommended, depending on how wavy the line should to be.
#' @param sample TRUE or FALSE. TRUE (default) will generate a bootstrapped sample of the specified data frame. FALSE will
#' run trend_lmer() on the original dataset.
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
#'   model_type = 'lmer', random_type = "intercept", sample = TRUE)
#'
#' boot_l <- case_boot_sample(fake_df, x = "cycle", y = "resp", ID = "Plot_Name",
#'   model_type = 'loess', span = 1, sample = TRUE)
#' }
#'
#' @export

case_boot_sample <- function(df, x = "cycle", y, ID = "Plot_Name", group = NA,
                             model_type = c("lmer", "loess"),
                             random_type = c("intercept", "slope", "custom"), random_formula = NA,
                             nest_var = NA,
                             span = NA_real_, degree = 1, sample = TRUE, sample_num = 1){

  if(is.null(df)){stop("Must specify df to run function")}
  if(is.null(x)){stop("Must specify x variable to run function")}
  if(is.null(y)){stop("Must specify y variable to run function")}
  if(is.null(ID)){stop("Must specify ID variable to run function")}
  random_type <- match.arg(random_type)
  if(random_type == "custom" & is.na(random_formula)){stop("Must specify random formula of random_type = 'custom'")}
  if(model_type == "loess" & !is.na(random_formula)){
    warning("Specified model_type is loess. Ignoring specified random_formula.")}
  stopifnot(c(x, y, ID) %in% names(df))

  plots <- data.frame(ID = unique(df[,ID]))
  colnames(plots) <- "ID" # bug handling for purrr::map
  n <- nrow(plots)

  samp <- if(sample == TRUE){
    data.frame(ID = sample(plots$ID, n, replace = TRUE))
  } else {data.frame(plots)} %>%
    dplyr::arrange(ID)

  # Set up unique naming column, so plots selected more than once have a unique ID.
  samp$case <- as.factor(stringr::str_pad(rownames(samp), nchar(n), side ="left", pad = 0))

  # Make sure nested variable in random effects is included in df_samp
  cols <- if(!is.na(nest_var)){c(ID, x, y, nest_var)} else {c(ID, x, y)}

  df_samp <- dplyr::left_join(samp, df[,cols], by = c("ID" = ID)) %>%
    dplyr::arrange(case, x)

  # For custom formulas, have to replace smallest unit (e.g. Plot_Name) in formula
  # with case in the random_formula argument

  rand_form <- ifelse(random_type == "custom", gsub(ID, "case", random_formula), NA)

  mod <-
    if(model_type == "lmer"){
      suppressMessages(
        trend_lmer(df_samp, x = x, y = y, ID = "case",
                   random_type = random_type, random_formula = rand_form)) %>%
        dplyr::mutate(boot_num = ifelse(exists("sample_num"), sample_num, 1))
    } else if(model_type == "loess"){
      suppressMessages(
        trend_loess(df_samp, x = x, y = y, ID = "case", span = span, degree = degree)) %>%
        dplyr::mutate(boot_num = ifelse(exists("sample_num"), sample_num, 1))
    }

  #chatty <- ifelse(exists("chatty"), chatty, TRUE)
  sample_num <- ifelse(exists("sample_num"), sample_num, 1)

  #if(chatty == TRUE & (sample_num %% 10) == 0){cat(".")} #prints tick every 10 reps

  return(mod)
}

