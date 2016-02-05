# Illuminate Suspension tests

require(dplyr)
require(data.table)
require(lubridate)
#connect to illuminate database ####
message("Connect to Illuminate database")
ill_config<-as.list(read.dcf('../config/ill.dcf',all = TRUE))
ill.src<-src_postgres(dbname=ill_config$dbname, 
                  host=ill_config$host, 
                  port=ill_config$port,
                  user=ill_config$user, 
                  password=ill_config$pwd)

# get all Illuminate tables ####
message("Get Illuminate tables")
# Students
ill.students<-tbl(ill.src, "students")

# Get term
ill.student_term_aff<-tbl(ill.src, "student_term_aff")
ill.terms<-tbl(ill.src, "terms")
ill.sessions<-tbl(ill.src, "sessions")

# Get current enrollment
sqry<-sprintf("SELECT * FROM dd_currently_enrolled(to_date('%s','YY-MM-DD'))", today())

ill.dd_currently_enrolled<-tbl(ill.src, sql(sqry))


# Get grade_levels
ill.grade_levels<-tbl(ill.src, "grade_levels")

# Users
ill.users<-tbl(ill.src, "users")

# Discpline events
ill.student_discipline<-tbl(ill.src, "student_discipline")

# Discipline Violations
ill.student_discipline_violation<-tbl(ill.src, "student_discipline_violation")

# "codes.table_name" require select statements to pull into R
#sqry2<-'Select code_id AS "violation_code_id", code_translation AS "violation" from codes.discipline_violation'
#ill.codes_discipline_violation<-tbl(ill.src, sql(sqry2))

#Discipline consequences
ill.student_discipline_consquence<-tbl(ill.src, "student_discipline_consequence")

# Discipline actions
ill.codes_discipline_actions<-tbl(ill.src, sql('SELECT code_id AS "consequence_action_id", code_translation AS "consequence" FROM codes.discipline_actions'))

# Discipline descriptions
ill.codes_discipline_short_descriptons<-tbl(ill.src, 
                                            sql('SELECT code_id AS "short_description_id", code_translation AS "description"  from codes.discipline_short_description'))
# Dsicpline locations
ill.codes_discipline_location<-tbl(ill.src, 
                                            sql('SELECT code_id AS "location_id", code_translation AS "location"  from codes.discipline_location'))

# Munge illuminate tables  ####
message("Munging Illuminate Tables")

# First, get grades into students table via terms

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
         short_name, school_id)









# Munge Discpline ####

discipline_events <- select(ill.student_discipline,
                     discipline_id,
                     created_by,
                     discipline_datetime,
                     short_description_id,
                     other_short_description,
                     other_location,
                     location_id,
                     referred_by,
                     student_id,
                     site_id,
                     notes
                     ) %>% left_join(students, by="student_id")


discipline_consequence<-left_join(ill.student_discipline_consquence, 
                                  ill.codes_discipline_actions, 
                                  by="consequence_action_id")

discipline<-left_join(discipline_events,
                      discipline_consequence, 
                      by="discipline_id") %>% 
  left_join(ill.codes_discipline_short_descriptons, 
            by="short_description_id") %>%
  left_join(ill.codes_discipline_location, 
            by="location_id")



users<-mutate(ill.users, 
       ref_first_name=first_name, 
       ref_last_name=last_name,
       referred_by=user_id) %>%
  select(ref_first_name, ref_last_name, referred_by)

disc<-left_join(discipline, users, by="referred_by") %>%
  select(site_id, 
         school_id,
         short_name, 
         local_student_id, 
         stu_first_name, 
         stu_last_name, 
         ref_first_name,
         ref_last_name,
         discipline_datetime,
         description,
         other_short_description,
         location,
         other_location,
         consequence,
         number_of_days,
         consequence_notes
         )

# change to data table, change column names and save as .Rdata

message("Collecting discipline data and casting as data.table")
discipline.dt<-data.table(collect(disc))

message("Renaming schools and combining names")
discipline.dt[school_id==78102, School:="KAP"]
discipline.dt[school_id==7810, School:="KAMS"]
discipline.dt[school_id==400146, School:="KCCP"]
discipline.dt[school_id==400163, School:="KBCP"]

discipline.dt[,School:=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP"))]

discipline.dt[,Student:=paste(stu_last_name, 
                              stu_first_name,
                              sep=", ")]
discipline.dt[,"Referrer":=paste(ref_last_name, 
                              ref_first_name,
                              sep=", ")]


message("Renaming columns")

disc.dt<-discipline.dt[,
                           list(School,
                                  short_name,
                                  local_student_id,
                                  Student,
                                  discipline_datetime,
                                  Referrer,
                                  description,
                                  location,
                                  consequence,
                                  number_of_days)][order(School, 
                                                              short_name,
                                                              discipline_datetime,
                                                              Student)]
setnames(disc.dt, 
         c("local_student_id",
           "short_name",
           "discipline_datetime",
           "Referrer",
           "description",
           "location",
           "consequence",
           "number_of_days"
           ),
         
         c("Student ID",
           "Grade",
           "Date/Time",
           "Referred by",
           "Violation",
           "Location",
           "Consequence",
           "Length (Days)"
           )
         )


save(disc.dt, file="discipline.Rdata")
system('touch ../restart.txt')




