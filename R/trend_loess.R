#' trend_loess: fit loess model and return results of interest
#'
#' @description fit a loess model using Plot_Name, and return predicted responses for each time steps represented in the
#' data. This is mostly an internal function run within case_boot_lmer(), but can be used as a stand alone function.
#'
#' @param df Data frame containing a column called Plot_Name, a column called cycle, and a column with at least one
#' response variable.
#' @param x Quoted time variable for trend analysis. Default is "cycle", but can also model by year. Must be numeric.
#' @param y Quoted response variable in the data frame.
#' @param ID Quoted name of column containing site or plot IDs. Default is "Plot_Name", and assumes the first 4 characters
#' are a park code.
#' @param span numeric value that controls the degree of smoothing. Smaller values (e.g., 0.1) result in less smoothing,
#' and possibly over-fitting the curve. Higher values (e.g., 0.9) result is more smoothing and possibly under-fitting.
#' You can calculate the number of time steps to include in the smoothing window by dividing p/n, where p is number of plots
#' you want to be included per window and n is number of timesteps in the data. When plotting years, knowing that panels include
#' 4 years, it is generally safe to assume a linear response between 2 full cycles, and therefore use a span of 8/n. Note that
#' if you specify degree = 1, then loess assumes a linear relationship within each span. If no span is specified, then
#' fANCOVA::loess.as() will be used to determine the optimum span (Note: user specified is preferred).
#' @param degree order of polynomial to fit. Values of 1 (Default) is linear, 2 is quadratic, etc. Degrees of 1 or 2 are
#' generally recommended, depending on how wavy the line should to be.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom fANCOVA loess.as
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' fake_df <- data.frame(Plot_Name = rep(paste0(rep("PARK.", 9), 1:9), each = 3),
#'                       group = "PARK",
#'                       cycle = rep(1:3, times = 9),
#'                       resp = runif(27, 0, 20))
#'
#' fake_df$resp_trend <- fake_df$cycle * fake_df$cycle
#'
#' mod <- trend_loess(fake_df, x = "cycle", y = "resp", ID = "Plot_Name", span = 1)
#' }
#'
#' @export

trend_loess <- function(df, x = "cycle", y, ID = "Plot_Name", degree = 1, span = NA_real_){

  if(is.na(span) & !requireNamespace("fANCOVA", quietly = TRUE)){
    stop("Package 'fANCOVA' needed to find optimal span. Please install it.", call. = FALSE)
  }

  if(missing(df)){stop("Must specify df to run function")}
  if(missing(x)){stop("Must specify x variable to run function")}
  if(missing(y)){stop("Must specify y variable to run function")}
  if(missing(ID)){stop("Must specify ID variable to run function")}
  if(missing(span)){warning("Span not specified. Will use gcv to determine best span.")}
  stopifnot(c(x, y, ID) %in% names(df))
  stopifnot(is.numeric(df[,x]) | is.integer(df[,x]))
  stopifnot(is.numeric(df[,y]) | is.integer(df[,y]))

  span_use <-
    if(is.na(span)){
    suppressWarnings(fANCOVA::loess.as(x = df[,x], y = df[,y], degree = degree,
       criterion = c("gcv"), user.span = NULL, plot = FALSE)$pars$span)
  } else {span}

  # # set up model
  pred_df <- data.frame(term = paste0(substr(x, 1, 1), unique(df[,x]), "_response"),
                        estimate = NA_real_,
                        span = span_use)

  tryCatch({
    lform <- as.formula(paste0(y, "~ ", x))
    mod <- loess(lform, data = df, span = span_use, degree = degree)
    new_df <- data.frame(x = sort(unique(mod$x)))
    names(new_df) <- c(x)
    pred_df <- data.frame(term = paste0(substr(x, 1, 1), new_df[,x], "_response"),
                          estimate = predict(mod, newdata = new_df),
                          span = span_use)
    },
    error = function(e){warning("Model failed to fit, returning empty data.frame")}#, #returns empty mod_df
    #warning = function(w){warning("Loess had warning but fit a model.")} #returns empty mod_df
    )

  return(pred_df)
}
