setwd("/jobs/idea/suspensions")

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
flog.appender(appender.tee("logs/suspensions.logs"))


flog.info("Load config and set other variables")

config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)

schools <- data_frame(schoolid = c(78102, 7810, 400146, 400163, 4001802, 400180),
                      schoolname = c("Ascend Primary", "Ascend Middle", "Create", "Bloom", "One Primary", "One Academy"),
                      schoolabbreviation =c("KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA"))

# Quick function that drops columns used for fivetran time stamping
drop_fivetran_cols <- . %>% select(-starts_with("_fivetran"))

bq_date <- sprintf("%s 00:00", config$FIRST_DAY)

flog.info("Connect to Silo/BQ")
bigrquery::set_service_token("/config/bq/kipp-chicago-silo-2-aa786970aefd.json")


flog.info("Get DL Supsesions")
susps <- get_deanslist("suspensions", collect = TRUE) %>%
  clean_names()

susps %>% 
  mutate()


flog.info("Telling Shiny Server to restart")
system('touch /srv/shiny-server/war/restart.txt')
