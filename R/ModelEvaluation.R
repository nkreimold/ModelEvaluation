#' ModelEvaluation: Tools for Data Diagnosis, Exploration, Transformation
#'
#' ModelEvaluation provides data diagnosis, data exploration and transformation of variables
#' during data analysis.
#'
#' It has two main goals:
#'
#' \itemize{
#' \item When data is acquired, it is possible to judge whether data is erroneous
#' or to select a variable to be corrected or removed through data diagnosis.
#' \item Understand the distribution of data in the EDA process. We can also
#' understand the relationship between target variables and predictor variables
#' for the prediction model.
#' }
#'
#' To learn more about ModelEvaluation, start with the vignettes:
#' `browseVignettes(package = "ModelEvaluation")`
#'
#' @import dplyr dlookr
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "key", "value", "var1", "var2", "outlier", "outliers_cnt",
    "outliers_mean", "outliers_ratio", "p_value", "variable", "with_mean", "without_mean",
    "Q1", "Q3", "minus", "freq", "zero", "obs", "coef_corr", "method", "r.squared"))
}
