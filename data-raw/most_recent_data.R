## code to prepare `most_recent_data` dataset goes here

httr::set_config(httr::config(http_version = 2))
most_recent_data <- usethis::use_zip(
  "https://www.phmsa.dot.gov/sites/phmsa.dot.gov/files/data_statistics/pipeline/incident_gas_distribution_jan2010_present.zip",
  destdir = getwd()
)
usethis::use_data(most_recent_data, overwrite = TRUE)
