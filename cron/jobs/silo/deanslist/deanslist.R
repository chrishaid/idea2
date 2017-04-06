setwd("/jobs/silo/deanslist")

readRenviron("/config/.Renviron")

Sys.setenv("GCS_AUTH_FILE" = "/config/gcs/kipp-chicago-silo-2-3789ce3e3415.json")

library(silounloadr)
library(dplyr)
library(tidyr)
library(httr)
library(deanslistr)
library(futile.logger)
library(purrr)
library(googleCloudStorageR)
library(janitor)
library(stringr)
library(lubridate)

extract_school_name <- . %>% mutate(school_name = str_extract(school_name, "K.{2,4}$"))

# set up logging
if(!dir.exists("logs")) dir.create("logs")
flog.threshold(TRACE)
flog.appender(appender.tee("logs/deanslist.log"))

flog.info("Connecting to and pulling deanslist suspension data")


susp_list <- ftry(get_suspensions(domain = "kippchicago"))

flog.info("suspensions data successfully pulled for keys %s",
          paste(names(susp_list), collapse = "\n "))

flog.info("Setting global bucket on GCS to deanslist")

gcs_global_bucket("deanslist")

flog.info("Uploading suspension data to GCS")

susp_df <- bind_rows(susp_list) %>%
  clean_names() %>%
  extract_school_name()

susp_rows <- susp_df$penalties %>% 
  map_lgl(~ifelse('IsSuspension' %in% names(.x), 
                  grepl("Y",.[['IsSuspension']]), 
                  FALSE
  )
  )

susp_df_2<-
  susp_df  %>% 
  filter(susp_rows) %>%
  mutate(types = 
           penalties %>% map_chr(~paste(.x$PenaltyName, collapse = ", ")))

#function to use with object_function in gcs_upload call
f <- function(input, output) jsonlite::write_json(input, path = output, pretty = T, dataframe = "rows")

gcs_upload(susp_df_2, name = "suspensions/files/suspensions.json", 
           object_function = f,
           type = 'application/json')

flog.info("Pulling deanslist incidents data")


incid_list <- ftry(get_incidents(domain = "kippchicago"))

flog.info("Incident data successfully pulled for keys %s",
          paste(names(incid_list), collapse = "\n "))


flog.info("Uploading deanslist incidents data ot GCS")
incid_df <- bind_rows(incid_list) %>%
  clean_names %>%
  extract_school_name()

gcs_upload(incid_df, name = "incidents/files/incidents.json", 
           object_function = f,
           type = 'application/json')

flog.info("Pulling deanslist behavior data")



behav_list <- ftry(get_behaviors(domain = "kippchicago", sdt = "2016-08-22", edt=today()))


flog.info("Behavior data successfully pulled for keys %s",
          paste(names(behav_list), collapse = "\n "))


flog.info("Uploading deanslist behavior data ot GCS")
behav_df <- bind_rows(behav_list) %>%
  clean_names %>%
  extract_school_name()

gcs_upload(behav_df, name = "behaviors/files/behaviors.json", 
           object_function = f,
           type = 'application/json')

flog.info("All uploads complete.")
