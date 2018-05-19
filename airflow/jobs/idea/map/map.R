options(java.parameters = "-Xmx8g")

# Load packages ####
require(dplyr)
require(readr)
require(lubridate)
require(purrr)
require(stringr)
require(mapvizieR)
require(futile.logger)
require(silounloadr)

setwd("/jobs/idea/map")
readRenviron("/config/.Renviron")

# set up logging
if(!dir.exists("logs")) dir.create("logs")
flog.threshold(TRACE)
flog.appender(appender.tee("logs/map.logs"))

flog.info("Source helpers")
source("lib/helpers.R")

flog.info("Load config and set other variables")
config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)


schools <- data_frame(schoolid = c(78102, 7810, 400146, 400163, 4001802, 400180),
                      schoolname = c("Ascend Primary", "Ascend Middle", "KAC/KCCP", "Bloom", "One Primary", "One Academy"),
                      schoolabbreviation =c("KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA"))

first_day <- config$FIRST_DAY

first_four_years_ago <- floor_date(ymd(first_day) - years(4), unit = "week") %>%
  as.character() 

flog.info("Connect to Silo")
# silo_nwea_db <- src_sqlserver(server =  config$SILO_URL,
#                              database = config$SILO_DBNAME_NWEA,
#                              properties = list(user = config$SILO_USER,
#                                                password = config$SILO_PWD))


flog.info("Pull map data")
#map_cdf <- silounloadr::get_nwea_('MAP$comprehensive#plus_cps')
map_cdf <- silounloadr::get_nwea_map('cdf_combined_kipp_cps')

test_term_names <- map_cdf %>%
  select(term_name, test_start_date) %>%
  filter(test_start_date >= first_four_years_ago) %>%
  select(term_name) %>%
  distinct() %>%
  collect()

get_map_by_term <- function(termname) {
  map_cdf %>% filter(term_name == termname) %>% collect()
}

map_cdf_2 <- test_term_names$term_name %>%
  purrr::map_df(~get_map_by_term(.))
     
  
# map_cdf <- tbl(silo_nwea_db,
#                sql("SELECT * FROM MAP$comprehensive#plus_cps WHERE GrowthMeasureYN='TRUE'")
#           )

#map_cdf <- collect(map_cdf)


names(map_cdf_2) <- str_replace_all(names(map_cdf_2), "_", "") %>% tolower()


flog.info("Excluding Survey only and some light munging")
 map_cdf_3 <- map_cdf_2 %>%
  mutate(testtype = if_else(is.na(testtype), "Survey With Goals", testtype),
         testid = as.character(testid)) %>%
  filter(testtype == "Survey With Goals",
         growthmeasureyn == 'TRUE') %>%
  mutate(teststartdate = as.character(ymd(teststartdate)),
         testid = if_else(is.na(testid),
                          paste(studentid, measurementscale, teststartdate, testdurationminutes, sep = "_"),
                          testid))

flog.info("Separate combined table into assessment results and roster")

map_sep <- separate_cdf(map_cdf_3, district_name = "KIPP Chicago")

flog.info("Create mapvizieR object for 2015 norms")

map_sep$cdf <- map_sep$cdf %>%
  mutate(goal7name = NA, 
         goal7ritscore = NA, 
         goal7stderr = NA, 
         goal7range = NA, 
         goal7adjective = NA, 
         goal8name = NA, 
         goal8ritscore = NA, 
         goal8stderr = NA, 
         goal8range = NA, 
         goal8adjective = NA, 
         projectedproficiencystudy3 = NA, 
         projectedproficiencylevel3 = NA) %>%
  distinct()

map_sep$roster <- map_sep$roster %>% distinct()

map_mv_15 <-  
  mapvizieR(
    cdf = map_sep$cdf,
    roster = map_sep$roster,
     include_unsanctioned_windows = TRUE,
    verbose = TRUE
    )

flog.info("Create summary objects")

map_sum_15 <- summary(map_mv_15$growth_df)

flog.info("Get current PowerSchool Roster")


#stus <- silounloadr::get_ps("students")
#current_ps <- stus %>%
#  filter(ENROLL_STATUS == 0)
#               sql("SELECT * FROM PS_mirror..Students WHERE Enroll_Status=0")
#               )

current_ps <- get_powerschool("students") %>%
  select(studentid = student_number,
         schoolid, 
         grade_level,
         enroll_status) %>%
  filter(enroll_status == 0) %>%
  collect()
  
#current_ps <- collect(current_ps)

names(current_ps) <- tolower(names(current_ps))

flog.info("calculate students per grade")
student_enrollment <- current_ps %>%
  group_by(schoolid, grade_level) %>%
  summarize(N = n()) %>%
  inner_join(schools, by = "schoolid") %>%
  rename(grade = grade_level)


flog.info("Calculate current students tested")
current_map_term <- map_mv_15$cdf %>%
  ungroup() %>%
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


flog.info("Getting historical scores")
hist_scores <- map_mv_15$cdf %>%
  ungroup() %>%
  inner_join(map_mv_15$roster %>%
               ungroup() %>%
               filter(implicit_cohort >= 2021) %>%
               select(termname, studentid, studentlastname,
                      studentfirstname, implicit_cohort, year_in_district),
             by = c("termname",  "studentid")) %>% 
  inner_join(current_ps %>%
               select(studentid),
             by = "studentid") %>%
  mutate(SY = sprintf("%s-%s", map_year_academic, map_year_academic + 1),
         School = mapvizieR::abbrev(schoolname, list(old = "KAPS", new = "KAP")),
         tested_at_kipp = as.logical(testedatkipp)) %>% 
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

flog.info("Saving data to disk.")
save(map_mv_15,
     map_sum_15,
     current_ps,
     current_map_term,
     student_enrollment_tested,
     hist_scores,
     file="/data/map.Rda")

flog.info("Saving data to GCS.")
Sys.setenv("GCS_AUTH_FILE" = "/config/gcs/kipp-chicago-silo-2-3789ce3e3415.json")
library(googleCloudStorageR)

gcs_global_bucket("idea_map")

gcs_results <- gcs_save(#map_mv_15,
                        map_sum_15,
                        current_ps,
                        current_map_term,
                        student_enrollment_tested,
                        hist_scores,
                        file = "map.rda")




flog.info("Tell shiny to restart")
system('touch /srv/shiny-server/map/restart.txt')

flog.info("Complete!")
