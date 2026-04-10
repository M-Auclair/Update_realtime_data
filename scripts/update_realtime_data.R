# script to fetch realtime WL data for all MRB stations

library(tidyhydat)
library(dplyr)
library(lubridate)

options(tidyhydat.quiet = TRUE)

# 50 station consecutive failure stop
MAX_CONSEC_FAIL <- 50L

# Load station list
if(!file.exists("data/realtime_station_list.rds")) {
  stop("ERROR: data/realtime_station_list.rds not found.")
}

# read in active stations
stations_within_basin <- readRDS("data/realtime_station_list.rds")
#filter to stations measuring WL
stations_within_basin <- stations_within_basin %>%
  dplyr::filter(has_level == TRUE)
station_list <- unique(stations_within_basin$STATION_NUMBER)

station_list <- station_list[!is.na(station_list)]

cat("Fetching realtime data for", length(station_list), "stations...\n")
cat("Start time:", as.character(Sys.time()), "\n\n")

# Fetch realtime data for all stations
all_data <- list()
success_count <- 0
error_count <- 0
consec_fail <- 0L

for(i in seq_along(station_list)) {
  station <- station_list[i]
  
  if(i %% 50 == 0) {
    cat(sprintf("Progress: %d of %d stations processed\n", i, length(station_list)))
  }
  
  outcome <- tryCatch({
    station_data <- tidyhydat::realtime_ws(
      station_number = station,
      parameters = 46,  # Water level
      start_date = Sys.time() - hours(24),
      end_date = Sys.time()
    )
    
    if(nrow(station_data) > 0) {
      # Get most recent value
      latest <- station_data %>%
        dplyr::arrange(desc(Date)) %>%
        dplyr::slice(1) %>%
        dplyr::select(STATION_NUMBER, Value, Date)
      
      all_data[[station]] <- latest
      success_count <- success_count + 1
      "success"
    } else{
      "empty"
    }
  }, error = function(e) {
    error_count <<- error_count + 1
    "error"
  })
  if(identical(outcome, "success")) {
    consec_fail <- 0L
  } else {
    consec_fail <- consec_fail + 1L
    if(consec_fail >= MAX_CONSEC_FAIL) {
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

# Combine all data
if(length(all_data) > 0) {
  realtime_data <- dplyr::bind_rows(all_data)
  
  # Add metadata
  attr(realtime_data, "last_updated") <- Sys.time()
  attr(realtime_data, "total_stations") <- length(station_list)
  attr(realtime_data, "successful_fetches") <- success_count
  attr(realtime_data, "failed_fetches") <- error_count
  
  # Save to RDS file
  output_file <- "data/realtime_WL_data.rds"
  
  # Create data directory if it doesn't exist
  if(!dir.exists("data")) {
    dir.create("data", recursive = TRUE)
  }
  
  saveRDS(realtime_data, output_file)
  
  cat("\nData saved to:", output_file, "\n")
  cat("Total rows:", nrow(realtime_data), "\n")
  cat("End time:", as.character(Sys.time()), "\n")
} else {
  stop("No data was successfully retrieved for any stations")
}

