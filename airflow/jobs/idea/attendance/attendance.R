# Load packages ####
require(dplyr)
require(silounloadr)
require(readr)
require(lubridate)
require(purrr)
require(stringr)
require(futile.logger)

# set up logging
setwd("/jobs/idea/attendance")

if(!dir.exists("logs")) dir.create("logs")
flog.threshold(TRACE)
flog.appender(appender.tee("logs/attendance.log"))




readRenviron("/config/.Renviron")

flog.info("Load config and set other variables")

config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)


schools <- data_frame(schoolid = c(78102, 7810, 400146, 400163, 4001802, 400180),
                      schoolname = c("Ascend Primary", "Ascend Middle", "Create", "Bloom", "One Primary", "One Academy"),
                      schoolabbreviation =c("KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA"))

first_day <- config$FIRST_DAY
first_day_bq <- sprintf('%s 00:00', first_day)

sy<-silounloadr::calc_academic_year(today(), format = 'firstyear')

ps_sy_termid <- silounloadr::calc_ps_termid(sy) %>%
  str_extract("\\d{2}") %>%
  as.integer()

# Quick function that drops columns used for fivetran time stamping
drop_fivetran_cols <- . %>% select(-starts_with("_fivetran"))

#post joins lead to convoluted collumn names.  This fixes that
clean_bq_col_names <- function(.data) {
  old_names <- names(.data)
  new_names <- old_names %>% str_replace("^.{10}_", "")
  names(.data) <- new_names
  
  # return
  .data
}
flog.info("Connect to Silo/BQ")
bigrquery::set_service_token("/config/bq/kipp-chicago-silo-2-aa786970aefd.json")




flog.info("Get students table")

students <- get_powerschool("students") %>%
  drop_fivetran_cols() %>%
  select(id,
         student_number,
         lastfirst, 
         home_room,
         enroll_status) %>%
  collect()


flog.info("Get attendence table") 

attendance <- get_powerschool("attendance") %>%
  filter(yearid >= ps_sy_termid,
         att_mode_code == "ATT_ModeDaily") %>%
  drop_fivetran_cols() %>%
  select(schoolid, 
         studentid, 
         date = att_date,
         attendance_codeid,
         yearid) %>%
  collect()

flog.info("Get membership table")  

membership <- get_powerschool("ps_membership_defaults") %>%
  filter(yearid >= ps_sy_termid) %>%
  drop_fivetran_cols() %>%
  select(studentid,
         schoolid,
         date = calendardate,
         enrolled = studentmembership,
         grade_level,
         attendance = att_calccntpresentabsent) %>%
  collect(n = Inf)

flog.info("Get attendance_code table") 
att_code <- get_powerschool("attendance_code") %>%
  filter(yearid >= ps_sy_termid) %>%
  drop_fivetran_cols %>%
  select(id,
         yearid, 
         schoolid, 
         att_code,
         presence_status_cd) %>%
  collect()

flog.info("Join memeberhips and attendance")  

attendance_2 <- attendance %>%
  inner_join(att_code, by = c("attendance_codeid" = "id",
                              "yearid",
                              "schoolid"))


member_att <- membership  %>%
  left_join(attendance_2 %>%
              select(studentid,
                     date,
                     att_code,
                     presence_status_cd
              ),
            by =c("studentid",
                  "date"))





flog.info("Processing member_att table to get final attendance table")

attend_student <- member_att %>%
  mutate(enrolled0 = 1,
         enrolled = if_else(att_code == "D" & !is.na(att_code), 0, enrolled0),
         present0 = ifelse(is.na(att_code), 1, 0),
         present1 = ifelse(att_code %in%  c("A", "S"), 0, present0),
         present2 = ifelse(att_code == "H", 0.5, present1),
         present3 = ifelse(att_code %in% c("T", "E"), 1, present2),
         present = ifelse(is.na(present2), 1, present3),
         absent = (1 - present)*enrolled) %>%
  left_join(students %>%
              select(studentid = id, 
                     student_number,
                     lastfirst, 
                     home_room),
            by="studentid") %>%
  inner_join(schools, by=c("schoolid")) %>%
  select(studentid,
         student_number,
         lastfirst,
         grade_level,
         schoolid,
         schoolname,
         schoolabbreviation,
         home_room,
         date,
         att_code,
         enrolled,
         present,
         absent)


attend_student <- attend_student %>%
  mutate(home_room2 = str_replace(home_room, "\\.", ""),
         home_room = str_replace(home_room2, "-", " ")) %>%
  select(-home_room2)

flog.info("Create daily and weekly attendance tables.")

flog.debug("by date, school, grade")
attend_date_school_grade <- attend_student %>%
  group_by(date, schoolabbreviation, grade_level) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent))

flog.debug("by date, by school")
attend_date_school <- attend_student %>%
  group_by(date, schoolabbreviation) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent))

flog.debug("by date,  grade")
attend_date_grade <- attend_student %>%
  group_by(date, schoolabbreviation, grade_level) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent))

flog.debug("by date, grade,  home room")
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

flog.info("Apply prep function to attend tables.")

att_list <- list(attend_date_school_grade,
                 attend_date_school,
                 attend_date_grade,
                 attend_date_grade_hr) %>%
  map(prep_att_tables)




attend_date_school_grade  <- att_list[[1]]
attend_date_school  <- att_list[[2]]
attend_date_grade  <- att_list[[3]]
attend_date_grade_hr <- att_list[[4]]


flog.debug(" Weekly and YTD ADA")
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

flog.info("Process current student data")

current_students <- students %>% filter(enroll_status == 0)

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

 
data_dir <- "/data"

attendance_dir <- sprintf("%s/attendance/", data_dir)

flog.info("Save data to flat files in %s", attendance_dir)

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

flog.info("Telling shiny to restart.")
system('touch /srv/shiny-server/war/restart.txt')

flog.info("Script complete.")
