#' trend_lmer: fit random effects model with lmer and return results of interest
#'
#' @description fit random effects model in lmer, and return intercept, slope and predicted responses for each
#' time step (i.e. cycle or year) represented in the data. This is mostly an internal function run within case_boot_lmer(),
#' but can be used as a stand alone function.
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable.
#' @param x Quoted time variable for trend analysis. Default is "cycle", but can also model by year. Must be numeric.
#' @param y Quoted response variable in the data frame.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param random_type Specify intercept, slope, or custom. The intercept option (default) will fit a random intercept on plot with (1|Plot_Name) as
#' random component. The slope option will fit a random slope model with (1 + cycle|Plot_Name)
#' @param random_formula If random_type = "custom", must specify the random effects formula for the model in quotes. Otherwise leave blank.
#' @param nest_var Quote column containing a grouping variable for nested random effects.
#'
#' @importFrom magrittr %>%
#' @importFrom lme4 lmer
#' @importFrom broom.mixed tidy
#' @importFrom dplyr filter select
#' @importFrom prediction find_data
#'
#' @examples
#' \dontrun{
#'
#' park = rep(c("APRK", "BPRK", "CPRK"), each = 30)
#' plot_name = paste(park, sprintf("%02d", rep(c(1:10), each = 3)), sep = "-")
#' cycle = rep(1:3, times = 30)
#' resp = runif(90, 0, 10)
#'
#' test_df <- data.frame(park, plot_name, cycle, resp)
#' test_df$group = ifelse(as.numeric(substr(test_df$plot_name, 5, 6)) %% 2, "GRP.1", "GRP.2")
#'
#' # random slope model
#' mod <- trend_lmer(test_df, x = "cycle", y = "resp", ID = "plot_name", random_type = "slope")
#'
#' # custom random effects model for group = GRP.1 with plot nested within park
#'
#' mod2 <- trend_lmer(test_df %>% filter(group == "GRP.1"), x = "cycle", y = "resp", ID = "plot_name",
#'   random_type = "custom", random_formula = "(1|park/plot_name)")
#' }
#'
#' @export

trend_lmer <- function(df, x = "cycle", y, ID = "Plot_Name",
                       random_type = c("intercept", "slope", "custom"), random_formula = NA,
                       nest_var = NA){

  if(is.null(df)){stop("Must specify df to run function")}
  if(is.null(x)){stop("Must specify x variable to run function")}
  if(is.null(y)){stop("Must specify y variable to run function")}
  if(is.null(ID)){stop("Must specify ID variable to run function")}
  random_type <- match.arg(random_type)
  if(random_type == "custom" & is.na(random_formula)){stop("Must specify random formula of random_type = 'custom'")}
  stopifnot(c(x, y, ID) %in% names(df))
  stopifnot(is.numeric(df[,x]))
  stopifnot(is.numeric(df[,y]))

  # set up model
  mod_df <- data.frame(term = c("Intercept", "Slope"), estimate = NA_real_)
  pred_df <- data.frame(term = paste0(substr(x, 1, 1), unique(df[,x]), "_response"),
                        estimate = NA_real_)

  tryCatch(
    {trend_form <-
      switch(random_type,
              'intercept' =  as.formula(paste0(y, "~ ", x, "+ (1|", ID,")")),
              'slope' = as.formula(paste0(y, "~ ", x, " + (1 + ", x, "|", ID, ")")),
              'custom' = as.formula(paste0(y, "~ ", x, " + ", random_formula)))

      mod <- suppressMessages(lme4::lmer(trend_form, data = df))
      # fit model and clean up output
      mod_df <- broom.mixed::tidy(mod) %>% dplyr::filter(effect == 'fixed') %>%
        dplyr::select(term, estimate) %>% data.frame()
      mod_df$term[mod_df$term == "(Intercept)"] <- "Intercept"
      mod_df$term[mod_df$term == x] <- "Slope"
      # get predicted mean response for each cycle in the dataset
      # first have to create dataset of cycles
      new_df <- data.frame(x = sort(unique(prediction::find_data(mod)[, x])))
      names(new_df) <- c(x)
      pred_df <- data.frame(term = paste0(substr(x, 1, 1), new_df[,x], "_response"),
                            estimate = predict(mod, newdata = new_df, type = 'response', re.form = NA))},

    error = function(e){warning("Model failed to fit, returning empty data.frame")}, #returns empty mod_df
    warning = function(w){warning("Model failed to fit, returning empty data.frame")} #returns empty mod_df
  )

  output <- rbind(mod_df, pred_df) %>%
    dplyr::mutate(isSingular = ifelse(is.na(estimate), NA_real_, as.numeric(lme4::isSingular(mod))))

    return(output)
}
