setwd("/jobs/idea/observations")

readRenviron("/config/.Renviron")

# Load packages ####
library(dplyr)
library(silounloadr)
library(lubridate)
library(purrr)
library(stringr)
library(futile.logger)
library(forcats)
library(janitor)

# set up logging
flog.threshold(TRACE)
flog.appender(appender.tee("logs/observations.logs"))



flog.info("Connect to Silo/BQ")
bigrquery::set_service_token("/config/bq/kipp-chicago-silo-2-aa786970aefd.json")

flog.info("Get forms")
forms <- get_teachboost("forms", collect = TRUE) %>% 
  clean_names


flog.info("Get users")
users <- get_teachboost("users", collect = TRUE) %>% 
  clean_names

flog.info("Get groups (Schools)")
schools <- get_teachboost("groups", collect = TRUE) %>% 
  clean_names

flog.info("Get templates")
templates <- get_teachboost("templates", collect = TRUE) %>% 
  clean_names


