options(java.parameters = "-Xmx4g")

require(dplyr)
require(RSQLServer)
require(tidyr)
require(lubridate)
require(stringr)

setwd("/jobs/idea/assignments")

config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)

silo_ps_db <- src_sqlserver(server =  config$SILO_URL,
                            database = config$SILO_DBNAME_PS,
                            properties = list(user = config$SILO_USER,
                                              password = config$SILO_PWD))

pg_final_grades <- tbl(silo_ps_db, "pgfinalgrades")

#pg_assignments <- tbl(silo_ps_db, "pgassignments")
assignment <- tbl(silo_ps_db, "assignment")

students <- tbl(silo_ps_db, sql("SELECT * FROM students WHERE enroll_status = 0"))

assignment_score <- tbl(silo_ps_db, "assignmentscore")
assignment_section <- tbl(silo_ps_db, "assignmentsection")

section_scores_ids <- tbl(silo_ps_db, "SectionScoresID")

sections <- tbl(silo_ps_db, sql("SELECT * from sections where TERMID = 2600"))

cc <- tbl(silo_ps_db, sql("SELECT * FROM cc WHERE DATEENROLLED >='01-AUG-16'"))


pg_fg_cc <- pg_final_grades %>%
  select(SECTIONID,
         STUDENTID,
         FINALGRADENAME,
         GRADE,
         PERCENT) %>%
  left_join(cc, by = c("SECTIONID", "STUDENTID")) %>%
  #rename(SECTIONID= SECTIONID.x,
  #       STUDENTID = STUDENTID.x) %>%
  select(-ends_with(".y"))

grades_1<-pg_fg_cc %>%
  left_join(students %>%
              select(STUDENTID = ID,
                     STUDENT_NUMBER,
                     LASTFIRST,
                     GRADE_LEVEL,
                     HOME_ROOM
              ),
            by = "STUDENTID") %>%
  collect(n = Inf)

grades <- grades_1 %>%
  select( STUDENTID,
          LASTFIRST,
          GRADE_LEVEL,
          HOME_ROOM,
          SCHOOLID,
          DATEENROLLED,
          DATELEFT,
          TERMID,
          SECTION_NUMBER,
          COURSE_NUMBER,
          FINALGRADENAME,
          GRADE,
          PERCENT)

names(grades) <- tolower(names(grades))

# assignments


# assignments_1<-
#   section_scores_ids %>%
#   select(DCID, SECTIONID, STUDENTID) %>%
#   inner_join(section_scores_assignments, by = c("DCID" = "FDCID")) %>%
#   left_join(sections %>%
#               select(ID,
#                      GRADE_LEVEL,
#                      COURSE_NUMBER,
#                      TEACHER,
#                      TERMID), by = c("SECTIONID" = "ID")) %>%
#   left_join(pg_assignments, by = c("ASSIGNMENT" = "ID",
#                                    "SECTIONID" = "SECTIONID")) %>%
#   filter(DATEDUE >= "2016-06-15")

#assignments <- collect(assignments_1, n = Inf)

assignments <- assignment %>%
  inner_join(assignment_section,
             by = "ASSIGNMENTID", suffix = c("_assign", "_assign_sect")) %>%
  inner_join(assignment_score,
             by = "ASSIGNMENTSECTIONID", suffix = c("", "_score")) %>%
  left_join(sections %>%
              select(DCID,
                     GRADE_LEVEL,
                     COURSE_NUMBER,
                     TEACHER,
                     TERMID),
            by = c("SECTIONSDCID" = "DCID")) %>%
  collect(n = Inf)


names(assignments) <- tolower(names(assignments))


students <- collect(students)
names(students) <- tolower(names(students))

# join student data to assignment data ####
assignments_students <- assignments %>%
  inner_join(students,
             by = c("studentsdcid" = "dcid"))

grade_order <- rev(c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "F"))
grade_cols <- scales::brewer_pal("div", "RdYlGn")(length(grade_order))

grade_scale <- data_frame(grade_order, grade_cols)


quarter_dates <- as.POSIXlt(c("2016-08-22",
                              "2016-10-24",
                              "2017-01-23",
                              "2017-04-03",
                              "2017-06-16"))

quarters <- c("Q1", "Q2", "Q3", "Q4")

# Calculate summary data from assignments
calc_cums <-. %>%
  mutate(date = ymd_hms(duedate),
                     quarter = cut.POSIXt(date, breaks = quarter_dates, labels = quarters),
                     percent = as.double(scorepercent),
                     score = as.double(scorepoints),
                     exempt = as.logical(as.integer(isexempt)),
                     includeinfinalgrades = as.logical(iscountedinfinalgrade),
                     grade = factor(as.character(scorelettergrade), levels = grade_order, ordered = TRUE))  %>%
  filter(!is.na(percent),
         !is.na(quarter)
  ) %>%
  group_by(id, lastfirst, course_number, quarter) %>%
  mutate(cum_mean_score = dplyr::order_by(date, cummean(score)),
         cum_mean_percent = dplyr::order_by(date, cummean(percent)),
         weighted_points = weight*score,
         weighted_points_possible = weight*totalpointvalue,
         weighted_percent = weighted_points/weighted_points_possible,
         cum_weighted_points_possible = dplyr::order_by(date, cumsum(weighted_points_possible)),
         cum_weighted_points = dplyr::order_by(date, cumsum(weighted_points)),
         cum_weighted_avg = cum_weighted_points/cum_weighted_points_possible,
         cum_grade = cut(cum_weighted_avg, ordered_result = TRUE,
                         breaks= c(0,.69,.72,.76,.79,.82,.84,.89,.93,.97, 100.00),
                         labels =  grade_order)) %>%
  filter(n()>5) %>%
  inner_join(grade_scale, by = c("grade" = "grade_order"))


assignments <- calc_cums(assignments_students)

assignments %>% glimpse

save(assignments,
     file="/data/assignments.Rda")

system("touch /srv/shiny-server/assignments/restart.txt")
