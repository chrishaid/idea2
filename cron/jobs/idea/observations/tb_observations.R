setwd("/jobs/idea/observations")

readRenviron("/config/.Renviron")

config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)

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


flog.info("Get roles")
roles <- get_teachboost("roles", collect = TRUE) %>% 
  clean_names


flog.info("Get groups (Schools)")
schools <- get_teachboost("groups", collect = TRUE) %>% 
  clean_names

flog.info("ID Admins from roles")
admins <- roles %>% filter(value %in% c("3", "2"), school_year==2016) %>%
  anti_join(schools %>% filter(name=="KIPP Chicago"), by= c("group_id" = "id")) %>%
  inner_join(users, by = c("user_id"="id")) %>%
  select(user_id, group_id) %>% unique()


flog.info("Get templates")
templates <- get_teachboost("templates", collect = TRUE) %>% 
  clean_names

flog.info("Munging observations from forms")
obs_types <- c("informal_obs",
               "classroom_walk",
               "other")

flog.info("Create forms_complete")
forms_complete <- forms %>%
  mutate(date = ymd_hms(date)) %>% 
  filter(date >= ymd(config$FIRST_DAY),
         type %in% obs_types
         ) %>%
  left_join(schools %>% select(group_ids = id, school = name),
            by = "group_ids") %>%
  left_join(users %>% select(author_ids = id, 
                             auth_name = full_name, 
                             auth_first = fname, 
                             auth_last =lname),
            by = "author_ids") %>%
  left_join(users %>% select(topic_ids = id, 
                             topic_name = full_name, 
                             topic_first = fname, 
                             topic_last = lname),
            by = "topic_ids") %>%
  filter(school != "KIPP Chicago") %>%
  arrange(date) %>%
  mutate(month = month(date, label = TRUE),
         month = fct_inorder(as.character(month), ordered = TRUE),
         school_abbrev = abbreviate(school, minlength = 3L))


flog.info("Create daily_tb")
daily_tb <- forms_complete %>%
  group_by(school_abbrev,author_ids, author_ids, auth_name, auth_last, auth_first, month, date) %>%
  summarise(n= n())

flog.info("Create monthly_tb")
monthly_tb <- daily_tb %>%
  summarise(n= n())
  

flog.info("Create teacher_daily_tb")
teacher_daily_tb <- forms_complete %>%
  group_by(school_abbrev, topic_ids, topic_name, topic_last, topic_first, month, date) %>%
  summarise(n= n())

flog.info("Create teacher_monthly_tb")
teacher_monthly_tb <- teacher_daily_tb %>%
  summarise(n= sum(n))

flog.info("Create teacher_ytd_tb")
teacher_ytd_tb <- teacher_monthly_tb %>%
  summarize(n = sum(n))


flog.info("Calculate last two weeks observations")
todays_date <- today()

last_two_weeks <-  floor_date(todays_date - weeks(1)) %--% todays_date

teacher_daily_tb <- teacher_daily_tb %>%
  dplyr::mutate(last_2_weeks = ymd(date) %within% last_two_weeks)

teacher_last_two_weeks <- 
  roles %>%
  filter(group_id!=26537, school_year==2016, value==0) %>%
  select(user_id, group_id) %>%
  left_join(users %>%
              select(user_id = id, full_name, last_name = lname, first_name = fname), 
            by ="user_id") %>%
  inner_join(schools %>% 
               filter(name != "KIPP Chicagao") %>%
               select(group_id = id, school_name = name),
             by = "group_id") %>%
  ungroup() %>%
  left_join(teacher_daily_tb %>%
              filter(last_2_weeks) %>%
              summarize(n=sum(n)) %>%
              ungroup() %>%
              select(topic_ids, n),
            by = c("user_id" = "topic_ids")) %>%
  mutate(n = if_else(is.na(n), 0L, n),
         school_abbrev = abbreviate(school_name, minlength = 3L)) %>%
  arrange(school_abbrev, desc(n), last_name)
  

flog.info("Saving tables to /data/tb_observations.Rda")
save(teacher_last_two_weeks,
     teacher_daily_tb,
     teacher_monthly_tb,
     teacher_ytd_tb,
     daily_tb,
     monthly_tb,
     forms_complete,
     file = "/data/tb_observations.Rda")

flog.info("Complete!")

