# Load packages ####
require(dplyr)
require(RSQLServer)
require(readr)
require(lubridate)
require(purrr)
require(stringr)
require(mapvizieR)

setwd("/jobs/idea/map")

source("lib/helpers.R")

# Load config and set other variables ####

config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)


schools <- data_frame(schoolid = c(78102, 7810, 400146, 400163, 4001802, 400180),
                      schoolname = c("Ascend Primary", "Ascend Middle", "Create", "Bloom", "One Primary", "One Academy"),
                      schoolabbreviation =c("KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA"))

first_day <- config$FIRST_DAY

# Connect to Silo ####
silo_nwea_db <- src_sqlserver(server =  config$SILO_URL,
                             database = config$SILO_DBNAME_NWEA,
                             properties = list(user = config$SILO_USER,
                                               password = config$SILO_PWD))


# Pull map data
map_cdf <- tbl(silo_nwea_db,
               sql("SELECT * FROM MAP$comprehensive#plus_cps WHERE GrowthMeasureYN='TRUE'")
          )

map_cdf <- collect(map_cdf)

# Separate combined table into assessment results and roster

map_sep <- separate_cdf(map_cdf,district_name = "KIPP Chicago")

# create mapvizieR object for 2015

map_mv_15 <-
  mapvizieR(
    cdf = map_sep$cdf,
    roster = map_sep$roster,
     include_unsanctioned_windows = TRUE #,
    # norm_df_long = mapvizieR:::norms_students_wide_to_long(
    #                 student_growth_norms_2015
    #                )
    )

# map_mv_11 <-
#   mapvizieR(
#     cdf = map_sep$cdf,
#     roster = map_sep$roster,
#     include_unsanctioned_windows = TRUE,
#     norm_df_long = mapvizieR:::norms_students_wide_to_long(
#       student_growth_norms_2011
#     )
#   )

# Create summary objects

map_sum_15 <- summary(map_mv_15$growth_df)
#map_sum_11 <- summary(map_mv_11$growth_df)

# get current PowerSchool Roster
current_ps <- tbl(silo_nwea_db,
               sql("SELECT * FROM PS_mirror..Students WHERE Enroll_Status=0")
               )

current_ps <- collect(current_ps)

names(current_ps) <- tolower(names(current_ps))
# calculate students per grade

student_enrollment <- current_ps %>%
  group_by(schoolid, grade_level) %>%
  summarize(N = n()) %>%
  inner_join(schools, by = "schoolid") %>%
  rename(grade = grade_level)


# Calculate current students tested.
current_map_term <- map_mv_15$cdf %>%
  filter(teststartdate == max(teststartdate)) %>%
  select(termname) %>%
  unique() %>%
  .[[1]]

tested <- map_mv_15$cdf %>%
  filter(termname == current_map_term,
         growthmeasureyn) %>%
  group_by(schoolname, grade, measurementscale) %>%
  summarize(n_tested = n()) %>%
  mutate(schoolabbreviation = abbrev(schoolname, list(old = "KAPS", new = "KAP"))) %>%
  ungroup() %>%
  select(schoolabbreviation, grade, measurementscale, n_tested)

student_enrollment_tested <-
  tested %>%
  left_join(student_enrollment,
            by = c("schoolabbreviation", "grade")) %>%
  select(School = schoolabbreviation,
         Grade = grade,
         Subject = measurementscale,
         Enrolled = N,
         Tested = n_tested
         ) %>%
  mutate(Percent = Tested/Enrolled)


# Let's grap historical scores
hist_scores <- map_mv_15$cdf %>%
  inner_join(map_mv_15$roster %>%
               filter(implicit_cohort >= 2021) %>%
               select(termname, studentid, studentlastname,
                      studentfirstname, implicit_cohort, year_in_district),
             by = c("termname",  "studentid")) %>%
  mutate(SY = sprintf("%s-%s", map_year_academic, map_year_academic + 1),
         School = mapvizieR::abbrev(schoolname, list(old = "KAPS", new = "KAP")),
         tested_at_kipp = as.logical(tested_at_kipp)) %>%
  select(SY,
         School,
         Grade = grade,
         Season = fallwinterspring,
         Subject = measurementscale,
         ID = studentid,
         "First Name" = studentfirstname,
         "Last Name" = studentlastname,
         "RIT Score" = testritscore,
         "Percentile" = testpercentile,
         "Date Taken" = teststartdate,
         "Taken at KIPP?" = tested_at_kipp
  ) %>%
  arrange(desc(SY), Season, Subject, School, Grade)


save(map_mv_15,
     map_sum_15,
     current_ps,
     current_map_term,
     student_enrollment_tested,
     hist_scores,
     file="/data/map.Rda")

# Tell shiny to restart ####
system('touch /srv/shiny-server/map/restart.txt')
