# Load packages ####
require(dplyr)
require(RSQLServer)
require(readr)
require(lubridate)
require(purrr)
require(stringr)
require(silounloadr)

setwd("/jobs/idea/attendance/data")
readRenviron("/config/.Renviron")

# Load config and set other variables ####
config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)


schools <- data_frame(schoolid = c(78102, 7810, 400146, 400163, 4001802, 400180),
                      schoolname = c("Ascend Primary", "Ascend Middle", "Create", "Bloom", "One Primary", "One Academy"),
                      schoolabbreviation =c("KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA"))

first_day <- config$FIRST_DAY

# Connect to Silo ####
# silo_ps_db <- src_sqlserver(server =  config$SILO_URL,
#                              database = config$SILO_DBNAME_PS,
#                              properties = list(user = config$SILO_USER,
#                                                password = config$SILO_PWD))

# Get students ####
#students <- tbl(silo_ps_db, "students") %>% collect()

students <- get_ps("students") %>% 
  collect()

# Get attendance ####
attendance <- tbl(silo_dbname_ps_mirror,
                  sql(sprintf("SELECT * FROM attendance WHERE ATT_DATE>='%s'",
                              first_day
                              )
                      )
                  ) %>%
  collect()


# Get membership ####
membership <- tbl(silo_dbname_ps_mirror,
                  sql(sprintf("SELECT * FROM membership WHERE CALENDARDATE>='%s'",
                           first_day
                           )
                      )
                  ) %>%
  collect(n = Inf)



# Join memeberhips and attendance ####
member_att <- membership  %>%
  select(STUDENTID,
         STUDENT_NUMBER,
         SCHOOLID,
         CALENDARDATE,
         GRADE_LEVEL,
         ATT_CALCCNTPRESENTABSENT
         ) %>%
  left_join(attendance %>%
              select(STUDENTID,
                     ATT_DATE,
                     ATT_CODE,
                     PRESENCE_STATUS_CD
                     ),
            by =c("STUDENTID",
                  "CALENDARDATE" = "ATT_DATE"))


# Pull member_att into memory ####
#member_att<-collect(member_att)


# light munging of member_att to get final attendance table ####
attend_student <- member_att %>%
  mutate(enrolled0 = 1,
         enrolled = ifelse(ATT_CODE == "D" & !is.na(ATT_CODE), 0, enrolled0),
         present0 = ifelse(is.na(ATT_CODE), 1, 0),
         present1 = ifelse(ATT_CODE %in%  c("A", "S"), 0, present0),
         present2 = ifelse(ATT_CODE == "H", 0.5, present1),
         present3 = ifelse(ATT_CODE %in% c("T", "E"), 1, present2),
         present = ifelse(is.na(present2), 1, present3),
         absent = (1 - present)*enrolled,
         date = ymd_hms(CALENDARDATE)) %>%
  left_join(students %>%
              select(STUDENTID = ID, LASTFIRST, HOME_ROOM),
            by="STUDENTID") %>%
  inner_join(schools, by=c("SCHOOLID" = "schoolid")) %>%
  select(STUDENTID,
         STUDENT_NUMBER,
         LASTFIRST,
         GRADE_LEVEL,
         SCHOOLID,
         schoolname,
         schoolabbreviation,
         HOME_ROOM,
         date,
         ATT_CODE,
         enrolled,
         present,
         absent)

names(attend_student) <- tolower(names(attend_student))

# Remove punction from home room names which can cause
# all kinds of issues with shiny.  Who knew?

attend_student <- attend_student %>%
  mutate(home_room2 = str_replace(home_room, "\\.", ""),
         home_room = str_replace(home_room2, "-", " ")) %>%
  select(-home_room2)


# Create daily and weekly attendance tables ####

# by date, school, grade
attend_date_school_grade <- attend_student %>%
  group_by(date, schoolabbreviation, grade_level) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent))

# by date, by school
attend_date_school <- attend_student %>%
  group_by(date, schoolabbreviation) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent))

# by date,  grade
attend_date_grade <- attend_student %>%
  group_by(date, schoolabbreviation, grade_level) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent))

# by date, grade,  home room
attend_date_grade_hr <- attend_student %>%
  group_by(date, schoolabbreviation, grade_level, home_room) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent))


# prep function ####
prep_att_tables <- . %>%
    mutate(pct_absent=absent/enrolled,
           pct_present=present/enrolled,
           pct_present_gte95 = pct_present >= .95,
           pct_present_gte96 = pct_present >= .96,
           pct_present_gte97 = pct_present >= .97,
           week_in_year=week(date)
           ) %>%
    group_by(schoolabbreviation) %>% #Week of calculations and labels
    mutate(week_in_sy = (floor_date(date, unit="week") - min(floor_date(date, unit="week")))/dweeks(1)+1,
           week_of_date=floor_date(date, unit="week") + days(1),
           week_of_date_short_label=sprintf(
             "%s %s",
             lubridate::month(week_of_date,label=TRUE, abbr=TRUE),
             lubridate::day(week_of_date)
             )
           ) %>%
  ungroup() %>%
  arrange(week_in_sy) %>% #resort
    mutate(week_of_date_short_label=factor(week_in_sy,
                                           labels=unique(week_of_date_short_label)
                                           )
           ) #Short Week  Label

# Apply prep function to attend tables ####
att_list <- list(attend_date_school_grade,
                 attend_date_school,
                 attend_date_grade,
                 attend_date_grade_hr) %>%
  map(prep_att_tables)




attend_date_school_grade  <- att_list[[1]]
attend_date_school  <- att_list[[2]]
attend_date_grade  <- att_list[[3]]
attend_date_grade_hr <- att_list[[4]]


# Weekly and YTD ADA ####
ada_weekly_school_grade <- attend_date_school_grade %>%
  group_by(schoolabbreviation, grade_level) %>%
  mutate(ytd_present = cumsum(present),
         ytd_enrolled = cumsum(enrolled),
         ytd_ada = ytd_present/ytd_enrolled*100
         ) %>%
  group_by(schoolabbreviation, grade_level, week_of_date, week_of_date_short_label) %>%
  mutate(cum_weekly_present = cumsum(present),
         cum_weekly_enrolled = cumsum(enrolled),
         weekly_ada = cum_weekly_present/cum_weekly_enrolled*100
  ) %>%
  filter(date == max(date))

ada_weekly_grade_hr <- attend_date_grade_hr %>%
  group_by(schoolabbreviation, grade_level, home_room) %>%
  mutate(ytd_present = cumsum(present),
         ytd_enrolled = cumsum(enrolled),
         ytd_ada = ytd_present/ytd_enrolled*100
  ) %>%
  group_by(schoolabbreviation, grade_level, home_room, week_of_date, week_of_date_short_label) %>%
  mutate(cum_weekly_present = cumsum(present),
         cum_weekly_enrolled = cumsum(enrolled),
         weekly_ada = cum_weekly_present/cum_weekly_enrolled*100
  ) %>%
  filter(date == max(date))


ada_weekly_school <- attend_date_school %>%
  group_by(schoolabbreviation) %>%
  mutate(ytd_present = cumsum(present),
         ytd_enrolled = cumsum(enrolled),
         ytd_ada = ytd_present/ytd_enrolled*100
  ) %>%
  group_by(schoolabbreviation, week_of_date, week_of_date_short_label) %>%
  mutate(cum_weekly_present = cumsum(present),
         cum_weekly_enrolled = cumsum(enrolled),
         weekly_ada = cum_weekly_present/cum_weekly_enrolled*100
  ) %>%
  filter(date == max(date))

# student_data
current_students<-collect(students %>% filter(ENROLL_STATUS==0))

names(current_students) <- tolower(names(current_students))

attend_student_ytd <- attend_student %>%
  semi_join(current_students,
            by = "student_number") %>%
  group_by(student_number,
           lastfirst,
           grade_level,
           schoolabbreviation) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent=sum(absent),
            ada = present/enrolled*100
            ) %>%
  group_by(schoolabbreviation, grade_level) %>%
  mutate(ada_rank = cume_dist(ada)) %>%
  arrange(schoolabbreviation, grade_level, ada_rank)






data_dir <- "/data/"

attendance_dir <- sprintf("%s/attendance/", data_dir)


save(attend_student,
     attend_student_ytd,
     attend_date_school_grade,
     attend_date_grade_hr,
     attend_date_school,
     attend_date_grade,
     ada_weekly_school_grade,
     ada_weekly_grade_hr,
     ada_weekly_school,
     file="/data/attendance.Rda")

# Tell shiny to restart ####
system('touch /srv/shiny-server/war/restart.txt')
