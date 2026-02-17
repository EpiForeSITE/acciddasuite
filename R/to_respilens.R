#' Build RespiLens metadata key from `model_out_tbl` data
#'
#' @param model_out_tbl Forecast tibble from `get_fcast()`.
#' @return Named list for RespiLens metadata key
metadata_key <- function(model_out_tbl) {
  # remove peak targets
  df <- model_out_tbl |>
    dplyr::filter(!grepl("peak", target, ignore.case = TRUE))

  abbr <- df$location[[1]]
  loc_row <- loc_data[loc_data$abbreviation == abbr, ]

  # safety check
  if (nrow(loc_row) == 0 || any(is.na(loc_row$location_name))) {
    stop("Location not found in loc_data.")
  }

  list(
    location = loc_row$location,
    abbreviation = loc_row$abbreviation,
    location_name = loc_row$location_name,
    population = loc_row$population,
    dataset = "ACCIDDA Suite",
    series_type = "projection",
    hubverse_keys = list(
      models = unique(df$model_id),
      targets = unique(df$target),
      horizons = as.character(unique(df$horizon)),
      output_types = unique(df$output_type)
    )
  )
}


#' @param oracle_output Ground truth tibble from `get_fcast()`.
#' @return JSON-style named list structure to satisfy the RespiLens `ground_truth` key.
ground_truth_key <- function(oracle_output) {
  if (nrow(oracle_output) == 0) {
    return(list(dates = list()))
  }
  pivot_truth <- oracle_output |>
    dplyr::select(target_end_date, target, oracle_value) |>
    tidyr::pivot_wider(
      names_from = target,
      values_from = oracle_value
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


#' @param model_out_tbl Forecast tibble from `get_fcast()`.
#' @return JSON-style named list structure to satisfy the RespiLens `forecasts` key.
forecasts_key <- function(model_out_tbl) {
  # Build JSON structure with nesting
  forecasts <- list()
  groups <- model_out_tbl |>
    dplyr::group_by(reference_date, target, model_id, horizon, output_type) |>
    dplyr::group_split()

  for (df in groups) {
    ref_date <- as.character(df$reference_date[1])
    target_name <- as.character(df$target[1])
    model_name <- as.character(df$model_id[1])
    horizon_val <- as.character(df$horizon[1])
    out_type <- df$output_type[1]

    if (is.null(forecasts[[ref_date]])) {
      forecasts[[ref_date]] <- list()
    }
    if (is.null(forecasts[[ref_date]][[target_name]])) {
      forecasts[[ref_date]][[target_name]] <- list()
    }
    if (is.null(forecasts[[ref_date]][[target_name]][[model_name]])) {
      forecasts[[ref_date]][[target_name]][[model_name]] <- list()
    }
    model_entry <- forecasts[[ref_date]][[target_name]][[model_name]]
    if (out_type == "quantile") {
      model_entry$type <- "quantile"
      if (is.null(model_entry$predictions)) {
        model_entry$predictions <- list()
      }
      model_entry$predictions[[horizon_val]] <- list(
        date = as.character(df$target_end_date[1]),
        quantiles = as.numeric(df$output_type_id),
        values = df$value
      )
    } else if (out_type == "pmf") {
      model_entry$type <- "pmf"
      if (is.null(model_entry$predictions)) {
        model_entry$predictions <- list()
      }
      model_entry$predictions[[horizon_val]] <- list(
        date = as.character(df$target_end_date[1]),
        categories = df$output_type_id,
        probabilities = df$value
      )
    } else {
      stop(sprintf(
        "`output_type` must be 'quantile' or 'pmf', received '%s'",
        out_type
      ))
    }
    forecasts[[ref_date]][[target_name]][[model_name]] <- model_entry
  }
  return(forecasts)
}


#' @param model_out_tbl Forecast tibble from `get_fcast()`.
#' @param unique_locs List. The locations for which there was data processed.
#' @returns JSON-style named list structure to satisfy the RespiLens metadata.json file (one per data dump).
metadata_file <- function(model_out_tbl, unique_locs) {
  all_models <- sort(unique(model_out_tbl$model_id))
  timestamp <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  active_loc_data <- loc_data |>
    dplyr::filter(abbreviation %in% unique_locs)
  locations_list <- lapply(seq_len(nrow(active_loc_data)), function(i) {
    row <- active_loc_data[i, ]
    list(
      location = as.character(row$location),
      abbreviation = as.character(row$abbreviation),
      location_name = as.character(row$location_name),
      population = as.numeric(row$population)
    )
  })
  return(list(
    last_updated = timestamp,
    models = all_models,
    locations = locations_list
  ))
}


#' @param accida_cast An object of class `accida_cast`, the output of `get_fcast()`.
#'
#' @return A named list with a single metadata JSON structure and one JSON structure per location.
to_respilens <- function(accida_cast) {
  #check it is of class `accida_cast`
  if (!inherits(accida_cast, "accida_cast")) {
    stop("Input must be an object of class 'accida_cast'.")
  }

  model_out_tbl = accida_cast$hubcast$model_out_tbl
  oracle_output = accida_cast$hubcast$oracle_output

  # Necessary model_out_tbl filtering
  # PATCH: removing 'peaks' keys for now (MyRespiLens doesn't support yet)
  if (any(grepl("peak", model_out_tbl$target, ignore.case = TRUE))) {
    warning(
      "Notice: MyRespiLens does not yet support 'peak' targets. Excluding 'peak' targets from output."
    )
    model_out_tbl <- model_out_tbl |>
      dplyr::filter(!grepl("peak", target, ignore.case = TRUE))
  }
  # also removing output_type==sample (RespiLens doesn't plot this output_type; this will not change)
  model_out_tbl <- model_out_tbl |>
    dplyr::filter(output_type != "sample")

  loc <- unique(model_out_tbl$location)

  if (length(loc) != 1) {
    stop("Expected exactly one location.")
  }

  return(
    list(
      metadata = metadata_key(model_out_tbl),
      ground_truth = ground_truth_key(oracle_output),
      forecasts = forecasts_key(model_out_tbl)
    )
  )
}
