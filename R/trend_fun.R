#' trend_fun: fit random intercept model and return results of interest
#'
#' @description fit random intercept model using Plot_Name, and return intercept, slope and predicted responses for
#' each cycle represented in the data. This is mostly an internal function run within case_boot_lmer(),
#' but can be used as a stand alone function.
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable.
#' @param x Quoted time variable for trend analysis. Default is "cycle", but can also model by year. Must be numeric.
#' @param y Quoted response variable in the data frame.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param random_type intercept or slope. The intercept option (default) will fit a random intercept on plot with (1|Plot_Name) as
#' random component. The slope option will fit a random slope model with (1 + cycle|Plot_Name)
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
#'                       group = "PARK",
#'                       cycle = rep(1:3, times = 9),
#'                       resp = runif(27, 0, 20))
#'
#' mod <- trend_fun(fake_df, x = "cycle", y = "resp", ID = "Plot_Name", random_type = "slope")
#' }
#'
#' @export

trend_fun <- function(df, x = "cycle", y, ID = "Plot_Name", random_type = c("intercept", "slope")){

  if(missing(df)){stop("Must specify df to run function")}
  if(missing(x)){stop("Must specify x variable to run function")}
  if(missing(y)){stop("Must specify y variable to run function")}
  if(missing(ID)){stop("Must specify ID variable to run function")}
  random_type <- match.arg(random_type)
  stopifnot(c(x, y, ID) %in% names(df))
  stopifnot(is.numeric(df[,x]))
  stopifnot(is.numeric(df[,y]))

  # set up model
  mod_df <- data.frame(term = c("Intercept", "Slope"), estimate = NA_real_)
  pred_df <- data.frame(term = paste0("C", unique(df[,x]), "_response"),
                        estimate = NA_real_)

  tryCatch(
    {trend_form <- if(random_type == 'intercept'){as.formula(paste0(y, "~ ", x, "+ (1|", ID,")"))
    } else if(random_type == 'slope'){as.formula(paste0(y, "~ ", x, " + (1 + ", x, "|", ID, ")"))}
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
