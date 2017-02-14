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
# set up logging

flog.threshold(TRACE)
flog.appender(appender.tee("logs/deanslist.log"))

flog.info("Connecting to and pulling deanslist data")


susp_list <- ftry(get_suspensions(domain = "kippchicago"))

flog.info("suspensions data successfully pulled for keys %s",
          paste(names(susp_list), collapse = "\n "))

flog.info("Begin unnesting suspesion data ")
safe_unnest <- safely(unnest)

susp_2 <- susp_list %>%
  purrr::map(~safe_unnest(.x, Penalties)) %>%
  purrr::transpose()

flog.info("Suspension data unnested")

ok <- susp_2$error %>% map_lgl(is_null)

flog.info("Unnestable tables are from:\n  %s", paste(names(ok)[ok], collapse = "\n  "))


if (length(names(ok)[!ok]) > 0) {
  flog.warn("Problems with tables from:\n  %s", paste(names(ok)[!ok], collapse = "\n  "))  
}


flog.info("Binding data frames together")
suspensions <- susp_2$result %>% keep(ok) %>% bind_rows()

flog.info("Extracing names, dates, and the like")
suspensions_2 <- suspensions %>%
  mutate(school = str_extract(school_name, "K.{2,3}$"),
         StartDate = ymd(StartDate),
         Month = month(StartDate, label = TRUE, abbr = TRUE),
         NumDays = as.integer(NumDays)) %>%
  filter(str_detect(PenaltyName, "Suspension")) %>% 
  select(SuspensionID, 
         school,
         StartDate, 
         NumDays, 
         Month, 
         StudentID, 
         Month, 
         PenaltyName, 
         Category,
         Infraction) %>%
  clean_names()



# Test loading into GCS bucket.
flog.info("Setting global bucket on GCS to deanslist")

gcs_global_bucket("deanslist")

flog.info("Uploading data to GCS")

ftry(gcs_upload(suspensions_2, name = "suspensions/files/suspensions.csv"))

flog.info("Upload Complete")
