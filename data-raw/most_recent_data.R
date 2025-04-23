## code to prepare `most_recent_data` dataset goes here

usethis::use_data(most_recent_data, overwrite = TRUE)
httr::set_config(httr::config(http_version = 1.1))
usethis::use_zip(
  "https://www.phmsa.dot.gov/sites/phmsa.dot.gov/files/data_statistics/pipeline/incident_gas_distribution_jan2010_present.zip",
  destdir = getwd()
)

