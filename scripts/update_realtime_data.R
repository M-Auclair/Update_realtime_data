# script to fetch realtime WL data for all stations

library(tidyhydat)
library(dplyr)
library(lubridate)

options(tidyhydat.quiet = TRUE)

# 50 station consecutive failure stop
MAX_CONSEC_FAIL <- 50L

STATION_FILE <- "data/realtime_station_list.rds"
OUTPUT_FILE <- "data/realtime_WL_data.rds"

# local tz used to decide daily boundary for full stn list run
LOCAL_TZ <- Sys.getenv("LOCAL_TZ", unset = "America/Edmonton")

# Load station list
if(!file.exists("data/realtime_station_list.rds")) {
  stop("ERROR: data/realtime_station_list.rds not found.")
}

# read in active stations
stations_within_basin <- readRDS("data/realtime_station_list.rds")
#filter to stations measuring WL
stations_within_basin <- stations_within_basin %>%
  dplyr::filter(has_level == TRUE)

today_local <- as.Date(with_tz(Sys.time(), tzone = LOCAL_TZ))
existing_data <- NULL
last_full_refresh_date <- as.Date(NA)

if (file.exists(OUTPUT_FILE)) {
  existing_data <- readRDS(OUTPUT_FILE)
  # recover last full refresh date if available
  existing_attr <- attr(existing_data, "last_full_refresh_date")
  if (!is.null(existing_attr) && !is.na(existing_attr)) {
    last_full_refresh_date <- as.Date(existing_attr)
  }
}

# Full run if no prior output exists, no recorded full refresh data, or recorded date is before today 
is_full_run <- is.null(existing_data) ||
  is.na(last_full_refresh_date) ||
  last_full_refresh_date < today_local

if (is_full_run) {
  target_stations <- stations_within_basin %>%
    dplyr::select(STATION_NUMBER) %>%
    dplyr::distinct()
  run_mode <- "full"
} else {
  target_stations <- stations_within_basin %>%
    dplyr::filter(high_freq == TRUE) %>%
    dplyr::select(STATION_NUMBER) %>%
    dplyr::distinct()
  run_mode <- "high_freq_subset"
}

station_list <- unique(target_stations$STATION_NUMBER)
station_list <- station_list[!is.na(station_list)]

cat("Run mode:", run_mode, "\n")
cat("Timezone for daily logic:", LOCAL_TZ, "\n")
cat("Today (local):", as.character(today_local), "\n")
cat("Fetching realtime data for", length(station_list), "stations...\n")
cat("Start time:", as.character(Sys.time()), "\n\n")

# Fetch realtime data for selected stations
all_data <- list()
success_count <- 0
error_count <- 0
consec_fail <- 0L

for (i in seq_along(station_list)) {
  station <- station_list[i]
  
  if (i %% 50 == 0) {
    cat(sprintf("Progress: %d of %d stations processed\n", i, length(station_list)))
  }
  
  outcome <- tryCatch({
    station_data <- tidyhydat::realtime_ws(
      station_number = station,
      parameters = 46,  # Water level
      start_date = Sys.time() - hours(24),
      end_date = Sys.time()
    )
    
    if (nrow(station_data) > 0) {
      # Get most recent value
      latest <- station_data %>%
        dplyr::arrange(desc(Date)) %>%
        dplyr::slice(1) %>%
        dplyr::select(STATION_NUMBER, Value, Date)
      
      all_data[[station]] <- latest
      success_count <- success_count + 1
      "success"
    } else {
      "empty"
    }
  }, error = function(e) {
    error_count <<- error_count + 1
    "error"
  })
  
  if (identical(outcome, "success")) {
    consec_fail <- 0L
  } else {
    consec_fail <- consec_fail + 1L
    if (consec_fail >= MAX_CONSEC_FAIL) {
      stop(
        "Halting after ", MAX_CONSEC_FAIL,
        " consecutive station failures (errors or no rows). ",
        "Last station: ", station,
        ". Likely upstream outage or connectivity issue."
      )
    }
  }
}

cat("\nFetching complete!\n")
cat("Successfully retrieved data for", success_count, "stations\n")
cat("Errors encountered:", error_count, "stations\n")

if (length(all_data) == 0) {
  stop("No data was successfully retrieved for any stations")
}

new_data <- dplyr::bind_rows(all_data)

# Ensure data directory exists
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Build output according to run mode
if (run_mode == "full") {
  realtime_data <- new_data
  last_full_refresh_date_out <- today_local
} else {
  # subset mode: merge into existing file, replacing only updated stations
  if (is.null(existing_data)) {
    # safety fallback
    realtime_data <- new_data
    last_full_refresh_date_out <- today_local
  } else {
    untouched <- existing_data %>%
      dplyr::filter(!STATION_NUMBER %in% new_data$STATION_NUMBER)
    
    realtime_data <- dplyr::bind_rows(untouched, new_data)
    
    # preserve prior full refresh date
    prev_full <- attr(existing_data, "last_full_refresh_date")
    if (is.null(prev_full) || is.na(prev_full)) {
      last_full_refresh_date_out <- today_local
    } else {
      last_full_refresh_date_out <- as.Date(prev_full)
    }
  }
}

# Add metadata
attr(realtime_data, "last_updated") <- Sys.time()
attr(realtime_data, "total_stations_in_file") <- nrow(realtime_data)
attr(realtime_data, "stations_targeted_this_run") <- length(station_list)
attr(realtime_data, "successful_fetches") <- success_count
attr(realtime_data, "failed_fetches") <- error_count
attr(realtime_data, "run_mode") <- run_mode
attr(realtime_data, "last_full_refresh_date") <- as.character(last_full_refresh_date_out)
attr(realtime_data, "local_timezone_for_daily_logic") <- LOCAL_TZ

saveRDS(realtime_data, OUTPUT_FILE)

cat("\nData saved to:", OUTPUT_FILE, "\n")
cat("Total rows in output:", nrow(realtime_data), "\n")
cat("End time:", as.character(Sys.time()), "\n")