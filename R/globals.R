#' Global variables used in NSE functions
#' To avoid R CMD check notes about "no visible binding for global variable"
#' we declare these variables as global here.
#' @keywords internal
#' @noRd

utils::globalVariables(c(
  "observation",
  "output_type_id",
  "value",
  ".model",
  "target_end_date",
  "issue",
  "geo_value",
  "time_value",
  "lambda_guerrero",
  ".id",
  "wis",
  "model_id",
  ".mean",
  "lower",
  "upper",
  "95%",
  # fable_to_hub / forecasts_key
  "reference_date",
  "horizon",
  "output_type",
  # ground_truth_key
  "oracle_value",
  # forecasts_key / to_respilens / metadata_key
  "target",
  # metadata_key (package dataset)
  "loc_data",
  # get_fcast plot
  "q95",
  "q95_lower",
  "q95_upper"
))
