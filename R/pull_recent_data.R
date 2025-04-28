#' Pull Recent Gas Pipeline Incident Data from PHMSA Website
#' @description
#' This function pulls the most recent gas pipeline incident data from 2010 to present day from the Pipeline and Hazardous Materials Safety Administration website, saves the data as a .txt within the package, and returns the data as a dataframe, which the user can then save as a dataframe in their local environment.
#'
#' @return A dataframe of the most recent data
#'
#' @examples
#' data <- pull_recent_data()
#' print(paste("The most recent incident occurred on:",as.character(data$LOCAL_DATETIME[[1]])))
#'
#' @import httr
#' @import dplyr
#' @import lubridate
#' @export

# define function
pull_recent_data <- function() {
  set_config(config(http_version = 0))
  zip_url <- "https://www.phmsa.dot.gov/sites/phmsa.dot.gov/files/data_statistics/pipeline/incident_gas_distribution_jan2010_present.zip"
  zip_path <- file.path(tempdir(), "incident_data.zip")
  system(paste("curl -L", zip_url, "-o", zip_path))
  unzip(zip_path, exdir = getwd())
  recent_data <- read.delim("incident_gas_distribution_jan2010_present.txt")
  recent_data$LOCAL_DATETIME <- mdy_hm(recent_data$LOCAL_DATETIME)
  recent_data <- recent_data %>%
    arrange(desc(LOCAL_DATETIME))
  return(recent_data)
}
