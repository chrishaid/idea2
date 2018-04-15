# Script to get and upload NWEA map data to BigQuery via Google Cloud Storage and fivetran
setwd("/jobs/silo/nwea_map")

readRenviron("/config/.Renviron")

Sys.setenv("GCS_AUTH_FILE" = "/config/gcs/kipp-chicago-silo-2-3789ce3e3415.json")

library(httr)
library(readr)
library(googleCloudStorageR)
library(dplyr)
library(snakecase)
library(stringr)
library(purrr)
library(futile.logger)


# set up logging
if(!dir.exists("logs")) dir.create("logs")
flog.threshold(TRACE)
flog.appender(appender.tee("logs/nwea_map.log"))

flog.info("Gathering credentials")

uid <- Sys.getenv("SILO_USER")
pwd <- Sys.getenv("SILO_PWD")
url <- "https://api.mapnwea.org/services/reporting/dex"

flog.info("Get zip file from NWEA endpoint")
resp <- GET(url = url, authenticate(user = uid, password = pwd, type = "basic" ))

status_message <- paste(http_status(resp), collapse = " | ")

if(status_code(resp)!=200) flog.error(sprintf("Response status code not 200.  Status is %s", status_message))

flog.info("Saving and unzippping files CDF file.")
temp <- tempdir()


file_path <- sprintf("%s/NWEA.zip", temp)

unzip_dir <- sprintf("%s/NWEA", temp)

bin <- content(resp, "raw")

writeBin(object = bin, con = file_path)

unzip(zipfile = file_path, exdir = unzip_dir)



flog.info("Inspect files for term names.")
students_file <- sprintf("%s/StudentsBySchool.csv", unzip_dir)

students_by_school <- read_csv(students_file, n_max = 5)

term_name <- unique(students_by_school$TermName) %>%
  to_parsed_case(sep_in = "\\-")

flog.info(sprintf("Term name is %s", term_name))

flog.info("Renaming files and uploading to Google Cloud Storage")
map_file_names <- list.files(unzip_dir)

map_table_names <- map_file_names %>%
  str_replace("\\.csv", "")

map_file_paths <- sprintf("%s/%s", unzip_dir, map_file_names)

map_cloud_file_names <- sprintf("%s/%s_%s.csv", map_table_names, map_table_names, term_name)

gcs_global_bucket("nwea_map")

gcs_results<- ftry(map2(map_file_paths,
                   map_cloud_file_names,
                   ~gcs_upload(file = .x, name = .y)))

flog.info("Uploads complete")

flog.info("Cleaning up files and directorys")
unlink(temp)

flog.info("Finished!")
