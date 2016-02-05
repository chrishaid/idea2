require(dplyr)
require(ggplot2)
require(lubridate)

source("../lib/transfer_helpers.R")
#connect to illuminate database ####
message("Connect to Illuminate database")
ill_config<-as.list(read.dcf('../config/ill.dcf',all = TRUE))

message("Connect to Illuminate databese")
ill.src<-src_postgres(dbname=ill_config$dbname, 
                      host=ill_config$host, 
                      port=ill_config$port,
                      user=ill_config$user, 
                      password=ill_config$pwd)
message("Getting Students")
ill.students<-tbl(ill.src, "students")


# First, get grades into students table via terms
ill.student_term_aff<-tbl(ill.src, "student_term_aff")
ill.terms<-tbl(ill.src, "terms")
ill.sessions<-tbl(ill.src, "sessions")
# Get grade_levels
ill.grade_levels<-tbl(ill.src, "grade_levels")


students_all<-select(ill.students, student_id, local_student_id, first_name, last_name) %>% 
  mutate(stu_first_name=first_name, stu_last_name=last_name) %>%
  select(student_id, local_student_id, stu_first_name, stu_last_name)

session_terms<-filter(ill.sessions, academic_year==2015) %>% 
  left_join(ill.terms, by="session_id") %>%
  inner_join(ill.student_term_aff, by="term_id") %>%
  left_join(ill.grade_levels, by="grade_level_id")

students<-left_join(students_all, session_terms, by="student_id") %>%
  mutate(school_id=site_id) %>%
  select(student_id, local_student_id, stu_first_name, stu_last_name,
         grade_level=short_name, school_id)





message("Getting consequences")
ill.consequences<-tbl(ill.src, sql("select * from behavior.consequences")) %>%
  filter(is.na(deleted_at))

message("Getting consqequence type")

ill.behavior_consequences<-tbl(ill.src, sql("select * from codes.behavior_consequences")) 

ill.behavior_desciptions<-tbl(ill.src, sql("select * from codes.behavior_descriptions")) %>%
  select(description_id=code_id, description=code_translation)

message("Getting incidents")
ill.incidents<-tbl(ill.src, sql("select * from behavior.incidents")) %>%
  select(incident_id, school_site_id, description_id) %>%
  left_join(ill.behavior_desciptions, by="description_id")

message("getting Participants")
ill.participants<-tbl(ill.src, sql("select * from behavior.participants")) %>%
  left_join(ill.incidents, by="incident_id")


message("Getting users")
ill.users<-tbl(ill.src, "users") %>%
  select(user_id, 
         staff_first_name=first_name, 
         staff_last_name=last_name)


ill.consequences2<-left_join(ill.consequences, 
                             ill.behavior_consequences, 
                             by=c("consequence_type_id"="code_id")) 


ill.participants_consequences <- left_join(ill.participants, 
                                           ill.consequences2, 
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
  left_join(ill.users, by="user_id")



schools<-c("KAP", "KAMS", "KCCP", "KBCP")

susp<-collect(ill.participants_consequences) %>% 
  mutate(school_site_id=ifelse(school_site_id==9999999, school_id, school_site_id),
         School=school_abbrev(school_site_id),
         School=factor(School, levels=schools)
         ) %>%
  dplyr::rename(StudentID=student_id
  )


mons<-c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")

SY1314 <- interval(ymd("130801"), ymd("140731"))
SY1415 <- interval(ymd("140801"), ymd("150731"))



susp_plot_data <- susp %>% 
  mutate(Type=ifelse(code_id %in% c(75,79), "ISS", "Suspension")) %>%
  group_by(School, date_assigned, Type) %>% 
  summarize(N=n()) %>%
  mutate(Month_Year=floor_date(date_assigned,"month"),
         SY_1 = "SY15-16",
         SY_2 = ifelse(date_assigned %within% SY1415, "SY14-15", SY_1),
         SY = ifelse(date_assigned %within% SY1314, "SY13-14", SY_2)
         ) %>%
  select(-SY_1, -SY_2) %>%
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
save(susp, susp_plot_data, file="suspensions.Rdata")

message("Telling Shiny Server to restart")
system('touch ../restart.txt')
