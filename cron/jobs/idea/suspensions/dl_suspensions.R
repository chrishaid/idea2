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



flog.info("Connect to Silo/BQ")
bigrquery::set_service_token("/config/bq/kipp-chicago-silo-2-aa786970aefd.json")


flog.info("Get DL Supsesions")
susps <- get_deanslist("suspensions", collect = TRUE) %>% 
  clean_names()

flog.info("Get Membership")
ps_md <-get_powerschool('ps_membership_defaults_deduped')
membership <- ps_md %>%
  filter(calendardate >= "2016-08-22 00:00") %>%
  group_by(schoolid, calendardate) %>%
  summarize(N = n())

flog.info("Calcualte ADM")
adm <- membership %>%
  collect() %>%
  summarize(adm = round(mean(N), 0)) 

schools <- tibble::tribble(
  ~schoolid, ~school_name, ~combined_name, ~school_full_name,
  78102, "KAP", "Ascend", "KIPP Ascend Primary",
  7810,  "KAMS", "Ascend", "KIPP Ascend Middle",
  400146, "KCCP", "Create", "KIPP Create College Prep",
  400163, "KBCP", "Bloom", "KIPP Bloom Collge Prep",
  4001802, "KOP", "One", "KIPP One Primary",
  400180, "KOA", "One", "KIPP One Academy"
) 



adm <- adm %>% inner_join(schools, by="schoolid")



flog.info("Extracting penalites nested field")
penalties <- 
  susps$penalties %>% purrr::map_df(~jsonlite::fromJSON(.x)) %>%
  clean_names() %>%
  select(suspensionid, 
         startdate, 
         enddate, 
         penaltyname
  ) %>%
  mutate(startdate = ymd(startdate),
         enddate = ymd(enddate)) %>%
  arrange(startdate) %>%
  #filter(!is.na(startdate)) %>%
  mutate(suspensionid = as.integer(suspensionid))

flog.info('Filtering to OSSs')
oss <- susps %>%
  inner_join(penalties %>% 
               filter(penaltyname == "Out of School Suspension"),
             by = "suspensionid")

flog.info('Calculating OSS Rates')


oss_2<-oss %>%  
  mutate(startdate = if_else(is.na(startdate), 
                             ymd_hms(oss$createts_date) +days(1), 
                             ymd_hms(sprintf("%s 00:00:00", startdate)))) %>%
  arrange(startdate) %>%
  mutate(month_1 = month(startdate, label = TRUE, abbr = TRUE),
         month = forcats::fct_inorder(as.character(month_1), ordered = TRUE)) %>%
  select(-month_1,
         student_number = studentschoolid, 
         student_first,
         student_last,
         school_name,
         month, 
         startdate, 
         infraction)



oss_rates<- oss_2 %>%
  mutate(month_year = floor_date(startdate, unit = "month")) %>%
  group_by(school_name, month, month_year) %>%
  summarize(N_susps = n()) %>%
  group_by(school_name) %>%
  mutate(
    N_susps = if_else(school_name == "KBCP" & month == "Dec",
                      N_susps + 22L, 
                      N_susps),
    cum_susps = cumsum(N_susps)
  ) %>%
  inner_join(adm, by = "school_name") %>%
  mutate(susp_rate = N_susps/adm*100,
         cum_susp_rate = cum_susps/adm*100)

flog.info('Filtering and calculating ISSs')
iss <- susps %>%
  inner_join(penalties %>% 
               filter(penaltyname == "In School Suspension"),
             by = "suspensionid")


iss_2<-iss %>%  
  mutate(startdate = if_else(is.na(startdate), 
                             ymd_hms(iss$createts_date) +days(1), 
                             ymd_hms(sprintf("%s 00:00:00", startdate)))) %>%
  arrange(startdate) %>%
  mutate(month_1 = month(startdate, label = TRUE, abbr = TRUE),
         month = forcats::fct_inorder(as.character(month_1), ordered = TRUE)) %>%
  select(-month_1,
         student_number = studentschoolid, 
         student_first,
         student_last,
         school_name,
         month, 
         startdate, 
         infraction)


iss_rates<- iss_2 %>%
  group_by(school_name, month) %>%
  summarize(N_susps = n()) %>%
  mutate(
    cum_susps = cumsum(N_susps)
  ) %>%
  inner_join(adm, by = "school_name") %>%
  mutate(susp_rate = N_susps/adm*100,
         cum_susp_rate = cum_susps/adm*100)

oss <- oss_2
iss <- iss_2

save(susps,
     penalties,
     oss,
     oss_rates,
     iss,
     iss_rates,
     adm,
     file = "/data/dl_suspensions.Rda")

flog.info("Telling Shiny Server to restart")
system('touch /srv/shiny-server/war/restart.txt')
