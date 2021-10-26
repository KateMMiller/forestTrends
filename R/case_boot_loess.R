#' @include case_boot_sample.R
#'
#' @title case_boot_loess: run case bootstrap for loess smoother and return model output
#'
#' @description For each replicate, as specified by num_reps, function will generate a bootstrapped sample of data from the
#' original dataset, fit a loess model, and return the predicted values for mean response of each unique time step in the x variable,
#' along with 95 % confidence intervals for each of unique time step. The num_boots column is the number of bootstrapped samples
#' that successfully fit a loess model.
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable.
#' @param x Quoted time variable for trend analysis. Default is "cycle", but can also model by year. Must be numeric.
#' @param y Quoted response variable in the data frame.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param group Quote column containing a grouping variable, like "Unit_ID" for printing progress to console. If not specified,
#' will print the first 4 characters of the ID to the console, assuming the ID starts with a 4-letter park code.
#' @param span numeric value that controls the degree of smoothing. Smaller values (e.g., 0.1) result in less smoothing,
#' and possibly over-fitting the curve. Higher values (e.g., 0.9) result is more smoothing and possibly under-fitting.
#' You can calculate the number of time steps to include in the smoothing window by dividing p/n, where p is number of plots
#' you want to be included per window and n is number of timesteps in the data. When plotting years, knowing that panels include
#' 4 years, it is generally safe to assume a linear response between 2 full cycles, and therefore use a span of 8/n. Note that
#' if you specify degree = 1, then loess assumes a linear relationship within each span. If no span is specified, then
#' fANCOVA::loess.as() will be used to determine the optimum span (Note: user specified is preferred).
#' @param degree order of polynomial to fit. Values of 1 (Default) is linear, 2 is quadratic, etc. Degrees of 1 or 2 are
#' generally recommended, depending on how wavy the line should to be.
#' @param num_reps Number of replicates to run in the bootstrap
#' @param chatty TRUE or FALSE. TRUE (default) will print progress in the console, including the first four characters
#' in the Plot_Name and a tick for every other replicate of the bootstrap. FALSE will not print progress in console.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange left_join mutate select
#' @importFrom purrr map map_df
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#'
#' #----- Dataset with 1 park -----
#' fake_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
#'                       cycle = rep(1:3, times = 9),
#'                       resp = runif(27, 0, 20))
#'
#' boot1 <- case_boot_loess(fake_df, y = "resp", num_reps = 10, span = 0.9, chatty = TRUE)
#'
#' #----- Dataset with 2 parks iterating through each park with purrr -----
#' # Create fake dataset
#' library(tidyverse)
#' fake_2pk <- data.frame(Plot_Name = c(rep(paste0(rep("APRK-", 12), sprintf("%02d", 1:12)), each = 3),
#'                        rep(paste0(rep("BPRK-", 12), sprintf("%02d", 13:24)), each = 3)),
#'                        park = c(rep("APRK", 36), rep("BPRK", 36)),
#'                        cycle = rep(1:3, times = 24),
#'                        resp = runif(72, 0, 30))
#'
#' # Nest dataset by park
#' nested_df <- fake_2pk %>% mutate(grp = park) %>% group_by(park) %>% nest()
#'
#' # Run case_boot_loess on nested dataset
#' boot2 <- nested_df %>% mutate(
#'   model = map(data, ~case_boot_loess(., x = "cycle", y = "resp", ID = "Plot_Name",
#'                                     span = 0.95, group = "grp",
#'                                     num_reps = 100, chatty = TRUE)))
#'
#' # Compile results
#' boot_results <- boot2 %>% select(park, model) %>% unnest(model)
#'
#'
#' }
#'
#' @export

case_boot_loess <- function(df, x = "cycle", y, ID = "Plot_Name", group = NA,
                           span = NA_real_, degree = 1, num_reps, chatty = TRUE){

  if(is.null(df)){stop("Must specify df to run function")}
  if(is.null(x)){stop("Must specify x variable to run function")}
  if(is.null(y)){stop("Must specify y variable to run function")}
  if(is.null(ID)){stop("Must specify ID variable to run function")}
  if(is.null(num_reps)){stop("Must specify num_reps (number of replicates) for bootstrap")}
  stopifnot(c(x, y, ID) %in% names(df))
  # stopifnot(is.numeric(df[,x]))
  # stopifnot(is.numeric(df[,y]))

  pname1 <- substr(df[1, ID], 1, 4)

  grp <- ifelse(!is.na(group), paste(unique(df[,group])),
                                 paste(pname1))

  plots <- data.frame(unique(df[,1]))

  nplots <- nrow(unique(plots))

  if(chatty == TRUE){cat(grp)}

  real_mod <- suppressWarnings(case_boot_sample(df, x = x, y = y, ID = ID,
                                                sample = F, sample_num = 1,
                                                model_type = 'loess', span = span, degree = degree) %>%
                                 dplyr::select(-boot_num))
  span_use <- unique(real_mod$span)

 if(nplots > 6){
   boot_mod <-
    suppressWarnings(purrr::map_df(seq_len(num_reps),
                                   ~case_boot_sample(df, x = x, y = y, ID = ID,
                                                     sample = T, sample_num = .x,
                                                     model_type = 'loess', span = span_use, degree = degree)) %>%
    #select(-3) %>%
    tidyr::pivot_wider(names_from = term, values_from = estimate)) %>% data.frame()


    boot_CIs <- data.frame(t(apply(boot_mod, 2,
                                   quantile, probs = c(0.025, 0.975), na.rm = T)),
                           num_boots = sum((ifelse(is.na(boot_mod$span), 0, 1))))

    num_boot_fail <- num_reps - max(boot_CIs$num_boots)

    if(num_boot_fail > 0){
      warning(paste0(num_boot_fail, " bootstrapped samples failed to return a model fit."))}

    boot_CIs$term <- rownames(boot_CIs)

    colnames(boot_CIs) <- c("lower95", "upper95", "num_boots", "term")
    boot_CIs
 } else if(nplots <=6){
   warning("Fewer than 6 plots for case bootstrap. Returning data.frame with estimates but no CIs.")
   boot_CIs <- data.frame(term = real_mod$term, lower95 = NA, upper95 = NA, num_boots = NA)
   boot_CIs
   }

  results <- dplyr::left_join(real_mod, boot_CIs, by = "term") %>%
             dplyr::mutate(x = as.numeric(gsub("\\D", "", term))) %>%
             dplyr::select(term, x, estimate, lower95, upper95, span, num_boots) %>%
             dplyr::arrange(x)

  colnames(results) <- c('term', paste0(x), 'estimate', 'lower95', 'upper95', 'span', 'num_boots')

  if(chatty == TRUE){cat("Done", "\n")}
  return(results)
}

