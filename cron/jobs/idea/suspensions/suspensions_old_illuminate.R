# Pull and munge suspension data from Illuminate_mirror

# Load packages ####
require(dplyr)
require(RSQLServer)
require(readr)
require(lubridate)
require(stringr)
require(silounloadr)

setwd("/jobs/idea/suspensions")

source("lib/helpers.R")



# Connect to Silo

config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)

silo_ill_db <- src_sqlserver(server =  config$SILO_URL,
                              database = config$SILO_DBNAME_ILL,
                              properties = list(user = config$SILO_USER,
                                                password = config$SILO_PWD))


message("Getting Students")
ill.students<-tbl(silo_ill_db, "students")


# First, get grades into students table via terms
ill.student_term_aff<-tbl(silo_ill_db, "student_term_aff")
ill.terms<-tbl(silo_ill_db, "terms")
ill.sessions<-tbl(silo_ill_db, "sessions")
# Get grade_levels
ill.grade_levels<-tbl(silo_ill_db, "grade_levels")


students_all<-select(ill.students, student_id, local_student_id, first_name, last_name) %>%
  mutate(stu_first_name=first_name, stu_last_name=last_name) %>%
  select(student_id, local_student_id, stu_first_name, stu_last_name)

session_terms <- filter(ill.sessions %>% collect(), academic_year==2016)  %>%
  left_join(ill.terms %>% collect(), by="session_id") %>%
  inner_join(ill.student_term_aff %>% collect(), by="term_id") %>%
  left_join(ill.grade_levels %>% collect(), by="grade_level_id")

students<-left_join(students_all %>% collect(), session_terms, by="student_id") %>%
  mutate(school_id=site_id) %>%
  select(student_id, local_student_id, stu_first_name, stu_last_name,
         grade_level=short_name, school_id)





message("Getting consequences")
ill.consequences<-tbl(silo_ill_db, "behavior_consequences") %>%
  filter(is.na(deleted_at))

message("Getting consqequence type")

ill.behavior_consequences<-tbl(silo_ill_db, "codes_behavior_consequences")

ill.behavior_desciptions<-tbl(silo_ill_db, "codes_behavior_descriptions") %>%
  select(description_id=code_id, description=code_translation)

message("Getting incidents")
ill.incidents<-tbl(silo_ill_db, "behavior_incidents") %>%
  select(incident_id, school_site_id, description_id) %>%
  left_join(ill.behavior_desciptions, by="description_id")

message("getting Participants")
ill.participants<-tbl(silo_ill_db, "behavior_participants") %>%
  left_join(ill.incidents, by="incident_id")


message("Getting users")
ill.users<-tbl(silo_ill_db, "users") %>%
  select(user_id,
         staff_first_name=first_name,
         staff_last_name=last_name)


ill.consequences2<-left_join(ill.consequences,
                             ill.behavior_consequences,
                             by=c("consequence_type_id"="code_id"))


ill.participants_consequences <- left_join(ill.participants %>% collect(),
                                           ill.consequences2 %>% collect(),
                                           by="participant_id") %>%
  left_join(students,
            by="student_id") %>%
  select(student_id,
         school_id,
         school_site_id,
         local_student_id,
         stu_first_name,
         stu_last_name,
         grade_level,
         description,
         user_id=created_by.x,
         date_assigned,
         code_id,
         code_key,
         code_translation,
         length=assigned_duration_days) %>%
  filter(code_id %in% c(74, 75, 79, 80)) %>%
  left_join(ill.users %>% collect(), by="user_id")



schools<-c("KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA")

susp<-collect(ill.participants_consequences) %>%
  mutate(school_site_id=ifelse(school_site_id==9999999, school_id, school_site_id),
         School=school_id_to_abbrev(school_site_id),
         School=factor(School, levels=schools),
         date_assigned = ymd_hms(date_assigned)
  ) %>%
  dplyr::rename(StudentID=student_id
  )


mons<-c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")

SY1314 <- interval(ymd("130801"), ymd("140731"))
SY1415 <- interval(ymd("140801"), ymd("150731"))
SY1516 <- interval(ymd("150801"), ymd("160731"))



susp_plot_data <- susp %>%
  mutate(Type=ifelse(code_id %in% c(75,79), "ISS", "Suspension")) %>%
  group_by(School, date_assigned, Type) %>%
  summarize(N=n()) %>%
  mutate(Month_Year=floor_date(date_assigned,"month"),
         SY_1 = "SY16-17",
         SY_2 = ifelse(date_assigned %within% SY1516, "SY15-16", SY_1),
         SY_3 = ifelse(date_assigned %within% SY1415, "SY14-15", SY_2),
         SY = ifelse(date_assigned %within% SY1314, "SY13-14", SY_3)
  ) %>%
  select(-SY_1, -SY_2, -SY_3) %>%
  ungroup %>%
  group_by(School, Month_Year, SY, Type) %>%
  summarize(N=sum(N)) %>%
  ungroup %>%
  group_by(School, SY, Type) %>%
  mutate(Cum_N=with_order(Month_Year, cumsum, N),
         Month=lubridate::month(Month_Year, label=TRUE, abbr = TRUE),
         Month=factor(as.character(Month), levels=mons, ordered=TRUE)) %>%
  as.data.frame




message("Saving suspensions tables to suspensions.Rdata")
save(susp, susp_plot_data, file="/data/suspensions.Rdata")

#message("Telling Shiny Server to restart")
system('touch /srv/shiny-server/war/restart.txt')




