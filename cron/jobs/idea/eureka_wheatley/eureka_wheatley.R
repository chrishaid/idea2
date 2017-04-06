  setwd("/jobs/idea/eureka_wheatley/")
  
  readRenviron("/config/.Renviron")
  
  # Load packages ####
  library(dplyr)
  library(silounloadr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(stringr)
  library(futile.logger)
  library(forcats)
  # set up logging
  
  if(!dir.exists("logs")) dir.create("logs")
  flog.threshold(TRACE)
  flog.appender(appender.tee("logs/eureka_wheatley.log"))
  
  
  flog.info("Load config and set other variables")
  
  config <- as.data.frame(read.dcf("/config/config.dcf"),
                          stringsAsFactors = FALSE)
  
  schools <- data_frame(schoolid = c(78102, 7810, 400146, 400163, 4001802, 400180),
                        school_name = c("Ascend Primary", "Ascend Middle", "Create", "Bloom", "One Primary", "One Academy"),
                        school_abbrev =c("KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA"))
  
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
  
  bq_date <- sprintf("%s 00:00", config$FIRST_DAY)
  
  flog.info("Connect to Silo/BQ")
  bigrquery::set_service_token("/config/bq/kipp-chicago-silo-2-aa786970aefd.json")
  
  flog.info("Get assessmetns table from BQ")
  
  assessments<-get_illuminate("assessments", collect = TRUE)
  
  flog.info("Get aggregate student responses data")
  agg_student_responses <- get_illuminate("agg_student_responses_deduped") %>%
    drop_fivetran_cols() %>%
    filter(date_taken >= bq_date) %>%
    collect(n = Inf)
  
  flog.info("Get aggregate student & standard responses data")
  agg_student_responses_standard <- get_illuminate("agg_student_responses_standard_deduped") %>%
    drop_fivetran_cols() %>%
  #  filter(date_taken >= bq_date) %>%
    collect(n = Inf)
  
  flog.info("Get standards")
  standards <- get_illuminate("standards", schema = "standards") %>%
    drop_fivetran_cols() %>%
    #  filter(date_taken >= bq_date) %>%
    collect(n = Inf)
  
  flog.info("Get student info (Illuminate)")
  students <- get_illuminate("students", "public", collect = TRUE)
  
  
  flog.info("Get student info (PowerSchool)")
  ps_stus <- get_ps("students") %>% 
    drop_fivetran_cols %>%
    select(
      LASTFIRST,
      GRADE_LEVEL,
      SCHOOLID,
      HOME_ROOM,
      STUDENT_NUMBER) %>%
    collect() %>% 
    janitor::clean_names()
  
  
  flog.info("Join student responses to assesments and it E/W assessments")
  
  # This regexp ids Wheatley and Eureka assessmsnts 
  
  title_regexp <- "(KIPP Wheatley-\\d{2}-\\d{2}-G.{1,2}-M{1,2})|(SY\\d{2}-\\d{2} Eureka G.{1,2} M\\d{1,2})|(Eureka G.{1,2} M\\d{1,2} Topic)"
  
  
  asr_assessemnts <- 
    agg_student_responses %>% 
    inner_join(assessments %>% drop_fivetran_cols,
               by = c("assessment_id"="assessment_id")) %>% 
    filter(str_detect(title, title_regexp))
  
  
  flog.info("Join asr_assessments to student data, interpeting test types and names")
  
  assessment_stus <- asr_assessemnts %>%
    inner_join(students %>% 
                 select(student_id, local_student_id) %>%
                 mutate(local_student_id = as.integer(local_student_id)), by = "student_id") %>%
    left_join(ps_stus %>%
                select(
                  lastfirst,
                  grade_level,
                  schoolid,
                  home_room,
                  student_number),
              by=c("local_student_id" = "student_number")) %>%
    mutate(curriculum = if_else(str_detect(title, "Eureka"), "Eureka", "Wheatley"),
           type1 = if_else(str_detect(title, "EOM"), "EOM", NULL),
           type2 = if_else(str_detect(title, "MM"), "MM", type1),
           type3 = if_else(str_detect(title, "Checkpoint"), "Checkpoint", type2),
           type = if_else(str_detect(title, "Topic"), "TA", type3),
           eom_type1 = if_else(str_detect(title, "SR"), "SR", NULL),
           eom_type = if_else(str_detect(title, "OER"), "OER", eom_type1),
           module = str_extract(title, "M\\d{1,2}"),
           type_number_2 = gsub("(.+)(Checkpoint|Topic) ([A-Z|0-9])",
                                "\\3",
                                title),
           type_number = if_else(str_detect(type, "TA|Checkpoint"), type_number_2, NULL),
           test_name_2 = paste(curriculum, grade_level, module, type, type_number, eom_type),
           test_name = str_replace_all(test_name_2, 
                                       pattern = c("NA" = "", 
                                                   " \\(Paper Administration\\)" = "")) %>%
             str_trim()
           ) %>%
    select(-type1, -type2, -type3, -eom_type1, -type_number_2, -test_name_2)
  
  flog.info("Calculating summary stats")
  assess_summary <- assessment_stus %>% 
    group_by(assessment_id, title, curriculum, module, type, eom_type, type_number, test_name, schoolid, grade_level) %>%
    summarize(date_taken_1 = min(date_taken),
              date_taken_2 = max(date_taken),
              n_students = n(),
              n_bm = sum(percent_correct >= 61),
              pct_bm = n_bm/n_students,
              avg_pct_correct = round(mean(percent_correct),1)) %>%
    group_by(test_name, schoolid, grade_level) %>%
    filter(assessment_id == max(assessment_id)) %>%
    ungroup() %>% 
    inner_join(schools, by = "schoolid") %>%
    ungroup() %>%
    arrange(type, grade_level, date_taken_2,  schoolid)
  
  flog.info("Selecting summary columns")
  assess_summary_2 <- assess_summary %>%
    select(assessment_id, grade_level, school=school_abbrev, curriculum, module, type, eom_type, title, test_name, 
           date_taken_1, date_taken_2, n_students, n_bm, pct_bm, avg_pct_correct)
  
  x  <- assess_summary_2 %>%
    filter(!is.na(module)) %>%
    ungroup %>%
    group_by(test_name) %>%
    filter(date_taken_1 == min(date_taken_1)) %>%
    ungroup %>%
    arrange(curriculum, grade_level, module, date_taken_1) %>%
    mutate(test_name_factor = forcats::fct_inorder(test_name))
  
  test_name_factor  <-levels(x$test_name_factor)
    
  flog.info("Finalizing summary data order")
  assesssment_summary <- assess_summary_2 %>%
    #filter(!is.na(module)) %>%
    mutate(test_name_factor = factor(test_name, levels = test_name_factor),
           test_name_short = str_extract(test_name, "M\\d.+")) 

  flog.info("Creating assessment_results_by_standard")
  
  assessment_results_by_standard <- agg_student_responses_standard %>%
    select(student_id, standard_id, 
           assessment_id,
           performance_band_id,
           performance_band_level,
           mastered,
           points,
           points_possible,
           number_of_questions) %>%
    inner_join(students %>% select(student_id), by = "student_id") %>%
    inner_join(standards %>% 
                 select(standard_id, 
                        parent_standard_id,
                        level, 
                        description,
                        custom_code),
               by = "standard_id")
  
  
  flog.info("Creating assessment id lookup table")
  short_names_assessemnt_id_lookup <- assesssment_summary %>%
    select(assessment_id, grade_level, curriculum, test_name_short) %>%
    distinct()
    
  flog.info("Creating standards summary table")
  standard_results_summary <- assessment_results_by_standard %>%
    inner_join(students %>% select(student_id, local_student_id) %>%
                 mutate(local_student_id  = as.integer(local_student_id)),
               by = "student_id") %>%
    left_join(ps_stus %>%
                select(
                  schoolid,
                  student_number),
              by=c("local_student_id" = "student_number")) %>%
    group_by(schoolid,
             assessment_id,
             standard_id,
             custom_code,
             description) %>%
    summarize(n_students = n(),
              sum_mastered = sum(as.integer(mastered)),
              pct_mastered = round(sum_mastered/n_students, 1),
              points_possible = sum(points_possible),
              sum_points = sum(points),
              pct_correct =round(sum_points/points_possible,1)) %>%
    inner_join(schools, by = "schoolid")
  
flog.info("Saving data.")
save(assesssment_summary,
     assessment_results_by_standard,
     short_names_assessemnt_id_lookup,
     standard_results_summary,
     file = "/data/eureka_wheatley.Rda")

flog.info("Done!")
