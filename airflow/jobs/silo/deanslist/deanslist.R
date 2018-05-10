options(java.parameters = "-Xmx10g")
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

todays_date <- today()
#todays_date <- ymd("2016-10-21")
sy <- todays_date  %>% calc_academic_year(format = "short") %>% str_replace("-", "")

sy_start <- todays_date %>%
  calc_academic_year(format = "first_year") %>%
  sprintf("%s-08-01", .)

sy_end <- todays_date %>%
  calc_academic_year(format = "second_year") %>%
  sprintf("%s-07-31", .)


# set up logging
if(!dir.exists("logs")) dir.create("logs")
flog.threshold(TRACE)
flog.appender(appender.tee("logs/deanslist.log"))

flog.info("Connecting to and pulling deanslist suspension data")


susp_list <- ftry(get_suspensions(domain = "kippchicago", std = sy_start, edt=sy_end))

flog.info("suspensions data successfully pulled for keys %s",
          paste(names(susp_list), collapse = "\n "))

flog.info("Setting global bucket on GCS to deanslist")

gcs_global_bucket("deanslist")

flog.info("Uploading suspension data to GCS")

susp_df <- bind_rows(susp_list) %>%
  clean_names("old_janitor") %>%
  extract_school_name()

susp_rows <- susp_df$penalties %>%
  map_lgl(~ifelse('IsSuspension' %in% names(.x),
                  grepl("TRUE",.[['IsSuspension']]),
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

gcs_upload(susp_df_2, name = sprintf("suspensions/files/suspensions_%s.json", sy),
           object_function = f,
           type = 'application/json')

flog.info("Pulling deanslist incidents data")


incid_list <- ftry(get_incidents(domain = "kippchicago", std = sy_start, edt = sy_end))

flog.info("Incident data successfully pulled for keys %s",
          paste(names(incid_list), collapse = "\n "))


flog.info("Uploading deanslist incidents data ot GCS")
incid_df <- bind_rows(incid_list) %>%
  clean_names("old_janitor") %>%
  extract_school_name()

gcs_upload(incid_df, name = sprintf("incidents/files/incidents_%s.json", sy),
           object_function = f,
           type = 'application/json')

flog.info("Pulling deanslist behavior data")



behav_list <- ftry(get_behaviors(domain = "kippchicago",  sdt = sy_start, edt = sy_end))


flog.info("Behavior data successfully pulled for keys %s",
          paste(names(behav_list), collapse = "\n "))


flog.info("Uploading deanslist behavior data to GCS")
behav_df <- bind_rows(behav_list) %>%
  clean_names %>%
  extract_school_name()

gcs_upload(behav_df, name = sprintf("behaviors/files/behaviors_%s.json", sy),
           object_function = f,
           type = 'application/json')

flog.info("All uploads complete.")
