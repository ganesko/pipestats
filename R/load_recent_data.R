#' Load Most Recent Gas Pipeline Incident Data from ZIP File
#' @description
#' This function loads the most recent gas pipeline incident data from 2010 to present day from a ZIP file downloaded from the Pipeline and Hazardous Materials Safety Administration's website and assigns it to a dataframe by default called "incident_gas_distribution_jan2010_present" in the user's global environment.
#'
#' @param zip_path Path to the ZIP file containing the .txt data file.
#' @param name Name of the dataframe to create in the global environment. Defaults to 'incident_gas_distribution_jan2010_present'. If name is changed, merge_data() will not use most recent data.
#' @return Invisibly returns NULL. The dataframe is assigned to the global environment.
#'
#' @examples
#' sample_zip <- system.file("extdata", "incident_sample.zip", package = "pipestats")
#' pull_recent_data(zip_path = sample_zip, name = "sample_data")
#' print(paste("The most recent incident in the example dataset occured on:",as.character(sample_data$LOCAL_DATETIME[[1]])))
#'
#' @import dplyr
#' @import lubridate
#' @export

# define function
pull_recent_data <- function(zip_path = NULL, name = "incident_gas_distribution_jan2010_present") {
  if (is.null(zip_path)) {
    stop("Automatic download disabled. Please provide a local ZIP path.")
  }

  if (!file.exists(zip_path)) {
    stop("ZIP file not found at: ", zip_path)
  }

  unzip_dir <- tempdir()
  unzip(zip_path, exdir = unzip_dir)
  txt_file <- list.files(unzip_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(txt_file) == 0) stop("No TXT file found in the ZIP archive.")

  correct_column_names <- as.character(correct_column_names[2, ])
  data <- read.delim(txt_file[1], sep = "\t", stringsAsFactors = FALSE)
  zip_column_names <- names(data)

  if (!identical(correct_column_names, zip_column_names)) {
    stop("Variables for data in ZIP file do not match expected variables.\nPlease download ZIP file from link titled \"Gas Distribution Incident Data - January 2010 to present (ZIP)\" from https://www.phmsa.dot.gov/data-and-statistics/pipeline/distribution-transmission-gathering-lng-and-liquid-accident-and-incident-data")
  }

  assign(name, data, envir = .GlobalEnv)

  message(paste0("Dataframe '", name, "' created in the global environment."))
  invisible(NULL)
}
