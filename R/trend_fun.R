#' trend_fun: fit random intercept model and return results of interest
#'
#' @description fit random intercept model using Plot_Name, and return intercept, slope and predicted responses for
#' each cycle represented in the data. This is mostly an internal function run within case_boot_lmer(),
#' but can be used as a stand alone function.
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable.
#' @param y Quoted response variable in the data frame.
#'
#' @importFrom magrittr %>%
#' @importFrom lme4 lmer
#' @importFrom broom.mixed tidy
#' @importFrom dplyr filter select
#' @importFrom prediction find_data
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' fake_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
#'                       cycle = rep(1:3, times = 9),
#'                       resp = runif(27, 0, 20))
#'
#' mod <- trend_fun(fake_df, "resp")
#' }
#'
#' @export

trend_fun <- function(df, y){
  if(!"Plot_Name" %in% names(df)){stop('Must have column named "Plot_Name" to run function')}
  if(!"cycle" %in% names(df)){stop('Must have column named "cycle" to run function')}

  # Check that suggested package required for this function are installed
  if(!requireNamespace("magrittr", quietly = TRUE)){
    stop("Package 'magrittr' must be installed.", call. = FALSE)}

  if(!requireNamespace("lme4", quietly = TRUE)){
    stop("Package 'lme4' must be installed.", call. = FALSE)}

  if(!requireNamespace("broom.mixed", quietly = TRUE)){
    stop("Package 'broom.mixed' must be installed.", call. = FALSE)}

  if(!requireNamespace("prediction", quietly = TRUE)){
    stop("Package 'prediction' must be installed.", call. = FALSE)}

  if(!requireNamespace("dplyr", quietly = TRUE)){
    stop("Package 'dplyr' must be installed.", call. = FALSE)}

  # set up model
  mod_df <- data.frame(term = c("Intercept", "Slope"), estimate = NA_real_)
  pred_df <- data.frame(term = paste0("C", unique(df$cycle), "_response"),
                        estimate = NA_real_)

  tryCatch(
    {trend_form <- as.formula(paste0(y, "~ cycle + (1|Plot_Name)"))
    mod <- suppressMessages(lme4::lmer(trend_form, data = df))
    # fit model and clean up output
    mod_df <- broom.mixed::tidy(mod) %>% dplyr::filter(effect == 'fixed') %>%
      dplyr::select(term, estimate) %>% data.frame()
    mod_df$term[mod_df$term == "(Intercept)"] <- "Intercept"
    mod_df$term[mod_df$term == "cycle"] <- "Slope"
    # get predicted mean response for each cycle in the dataset
    # first have to create dataset of cycles
    new_df <- data.frame(cycle = sort(unique(prediction::find_data(mod)$cycle)))
    pred_df <- data.frame(term = paste0("C", new_df$cycle, "_response"),
                          estimate = predict(mod, newdata = new_df, type = 'response', re.form = NA))},

    error = function(e){warning("Model failed to fit, returning empty data.frame")}, #returns empty mod_df
    warning = function(w){"Model failed to fit, returning empty data.frame"} #returns empty mod_df
  )

  output <- rbind(mod_df, pred_df) %>%
    dplyr::mutate(isSingular = ifelse(is.na(estimate), NA_real_, as.numeric(lme4::isSingular(mod))))
  return(output)
}
