#' Convert into RespiLens JSON data.
#' 
#' Convert Hubverse-style data (using forecast data and corresponding ground truth data)
#' to RespiLens projections-style JSON. JSON output can be visualized on the RespiLens
#' site using the \href{https://www.respilens.com/myrespilens}{MyRespiLens} feature.
#' Script also includes internal helper function definitions: get_location_data(), build_metadata_key(), build_ground_truth_key(),
#' build_forecasts_key(), build_metadata_file().


#' @param location_abbreviation Character. Location abbreviation for the location you 
#' want to retrieve information on.
#' @param value_needed Character. One of "fips", "location_name", or "population".
#' @return The desired value (character or numeric).
get_location_data <- function(location_abbreviation, value_needed) {
  if (!(tolower(value_needed) %in% c('location', 'location_name', 'population'))) {
    stop(paste0("value_needed parameter must be one of 'fips', 'location_name', 'population'. ",
                "Received: '", value_needed, "'"))
  }
  val_type <- tolower(value_needed)
  result <- loc_data |> 
    dplyr::filter(abbreviation == location_abbreviation) |> 
    dplyr::pull(!!val_type) 
  # Safety check; throw error if location isn't in loc_data
  if (length(result) == 0) {
    stop(sprintf("Location abbreviation '%s' not found in loc_data reference table.", 
                 location_abbreviation))
  }
  return(result[1])
}


#' @param filtered_forecast_data Tibble. Forecast data filtered to contain only one location.
#' @return JSON-style named list structure to satisfy the RespiLens `metadata` key.
build_metadata_key <- function(filtered_forecast_data) {
  abbreviation <- toupper(filtered_forecast_data$location[1])
  # PATCH: remove 'peaks' keys for now
  filtered_forecast_data <- filtered_forecast_data |>
    dplyr::filter(!grepl("peak", target, ignore.case = TRUE))
  metadata <- list(
    location=get_location_data(abbreviation, 'location'),
    abbreviation=abbreviation,
    location_name=get_location_data(abbreviation, "location_name"),
    population=get_location_data(abbreviation, "population"),
    dataset="ACCIDDA Suite",
    series_type="projection",
    hubverse_keys=list(
      models=unique(filtered_forecast_data$model_id),
      targets=unique(filtered_forecast_data$target),
      horizons=as.character(unique(filtered_forecast_data$horizon)),
      output_types=unique(filtered_forecast_data$output_type)
    )
  )
  return(metadata)
}


#' @param filtered_gt_data Tibble. Ground truth data filtered to contain only one location.
#' @return JSON-style named list structure to satisfy the RespiLens `ground_truth` key.
build_ground_truth_key <- function(filtered_gt_data) {
  if (nrow(filtered_gt_data) == 0) {
    return(list(dates = list()))
  }
  pivot_truth <- filtered_gt_data |>
    dplyr::select(target_end_date, target, value) |>
    tidyr::pivot_wider(
      names_from = target, 
      values_from = value
    ) |>
    dplyr::arrange(target_end_date)
  ground_truth <- list(
    dates = as.character(pivot_truth$target_end_date)
  )
  target_names <- setdiff(names(pivot_truth), "target_end_date")
  
  for (target_col in target_names) {
    ground_truth[[target_col]] <- pivot_truth[[target_col]]
  }
  return(ground_truth)
}


#' @param filtered_forecast_data Tibble. Forecast data filtered to contain only one location.
#' @return JSON-style named list structure to satisfy the RespiLens `forecasts` key.
build_forecasts_key <- function(filtered_forecast_data) {
  # Build JSON structure with nesting
  forecasts <- list()
  groups <- filtered_forecast_data |>
    dplyr::group_by(reference_date, target, model_id, horizon, output_type) |>
    dplyr::group_split()
  for (grouped_df in groups) {
    ref_date    <- as.character(grouped_df$reference_date[1])
    target_name <- as.character(grouped_df$target[1])
    model_name  <- as.character(grouped_df$model_id[1])
    horizon_val <- as.character(grouped_df$horizon[1])
    out_type    <- grouped_df$output_type[1]
    if (is.null(forecasts[[ref_date]])) forecasts[[ref_date]] <- list()
    if (is.null(forecasts[[ref_date]][[target_name]])) forecasts[[ref_date]][[target_name]] <- list()
    if (is.null(forecasts[[ref_date]][[target_name]][[model_name]])) {
      forecasts[[ref_date]][[target_name]][[model_name]] <- list()
    }
    model_entry <- forecasts[[ref_date]][[target_name]][[model_name]]
    if (out_type == "quantile") {
      model_entry$type <- "quantile"
      if (is.null(model_entry$predictions)) model_entry$predictions <- list()
      model_entry$predictions[[horizon_val]] <- list(
        date = as.character(grouped_df$target_end_date[1]),
        quantiles = as.numeric(grouped_df$output_type_id),
        values = grouped_df$value
      )
    } else if (out_type == "pmf") {
      model_entry$type <- "pmf"
      if (is.null(model_entry$predictions)) model_entry$predictions <- list()
      model_entry$predictions[[horizon_val]] <- list(
        date = as.character(grouped_df$target_end_date[1]),
        categories = grouped_df$output_type_id,
        probabilities = grouped_df$value
      )
    } else {
      stop(sprintf("`output_type` must be 'quantile' or 'pmf', received '%s'", out_type))
    }
    forecasts[[ref_date]][[target_name]][[model_name]] <- model_entry
  }
  return(forecasts)
}


#' @param forecast_data Tibble. The forecast data originally passed into to_respilens() with a full location set.
#' @param unique_locs List. The locations for which there was data processed.
#' @returns JSON-style named list structure to satisfy the RespiLens metadata.json file (one per data dump).
build_metadata_file <- function(forecast_data, unique_locs) {
  all_models <- sort(unique(forecast_data$model_id))
  timestamp <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  active_loc_data <- loc_data |> 
    dplyr::filter(abbreviation %in% unique_locs)
  locations_list <- lapply(seq_len(nrow(active_loc_data)), function(i) {
    row <- active_loc_data[i, ]
    list(
      location      = as.character(row$location),
      abbreviation  = as.character(row$abbreviation),
      location_name = as.character(row$location_name),
      population    = as.numeric(row$population)
    )
  })
  return(list(
    last_updated = timestamp,
    models       = all_models,
    locations    = locations_list
  ))
}


#' @param hubcast_object List. A list containing 2 named items: 
#' \describe{
#'   \item{model_out_tbl}{A tibble (\code{tbl_df}) of model forecasts}
#'   \item{oracle_output}{A tibble (\code{tbl_df}) of ground truth data}
#' }
#' 
#' @return A named list with a single metadata JSON structure and one JSON structure per location.
to_respilens <- function(hubcast_object) {
  # Basic parameter and column validation
  if (!all(c("model_out_tbl", "oracle_output") %in% names(hubcast_object))) {
    stop("hubcast_object parameter must contain 'model_out_tbl' and 'oracle_output'")
  }
  forecast_data = hubcast_object$model_out_tbl
  ground_truth_data = hubcast_object$oracle_output
  forecast_req_columns = c(
    "model_id", "reference_date", "target", "horizon", "location",
    "target_end_date", "output_type", "output_type", "output_type_id", "value"
    )
  missing_cols <- setdiff(forecast_req_columns, names(forecast_data))
  if (length(missing_cols) > 0) {
    stop(paste("model_out_tbl is missing required columns:", 
               paste(missing_cols, collapse = ", ")))
  }
  ground_truth_req_columns = c(
    "location", "target_end_date", "target", "oracle_value"
  )
  missing_cols <- setdiff(ground_truth_req_columns, names(ground_truth_data))
  if (length(missing_cols) > 0) {
    stop(paste("oracle_output is missing required columns:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Coerce ground_truth_data into RespiLens column naming
  ground_truth_data <- ground_truth_data |>
    dplyr::select(
      location, 
      target_end_date, 
      target, 
      value=oracle_value
    ) 
  
  # Necessary forecast_data filtering
  # PATCH: removing 'peaks' keys for now (MyRespiLens doesn't support yet)
  if (any(grepl("peak", forecast_data$target, ignore.case = TRUE))) {
    warning("Notice: MyRespiLens does not yet support 'peak' targets. Excluding 'peak' targets from output.")
    forecast_data <- forecast_data |>
      dplyr::filter(!grepl("peak", target, ignore.case = TRUE))
  }
  # also removing output_type==sample (RespiLens doesn't plot this output_type; this will not change)
  forecast_data <- forecast_data |>
    dplyr::filter(output_type != "sample")
  
  # Begin building JSON output by location
  json_output <- list()
  unique_locs <- unique(forecast_data$location)
  for (loc in unique_locs) {
    current_loc_forecasts <- forecast_data |> dplyr::filter(location == loc)
    current_loc_gt <- ground_truth_data |> dplyr::filter(location == loc)
    metadata_key <- build_metadata_key(current_loc_forecasts)
    ground_truth_key <- build_ground_truth_key(current_loc_gt)
    forecasts_key <- build_forecasts_key(current_loc_forecasts)
    # Assign to output list
    file_key <- paste0(loc, ".json")
    json_output[[file_key]] <- list(
      metadata = metadata_key, 
      ground_truth = ground_truth_key, 
      forecasts = forecasts_key
    )
  }
  # Build single metadata.json file
  file_key <- "metadata.json"
  json_output[[file_key]] <- build_metadata_file(forecast_data, unique_locs)
  
  # Return output
  return(json_output)
}
