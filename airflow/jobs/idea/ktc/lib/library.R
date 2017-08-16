prep_class <- function(class_name = NULL) {

  .data <- contact

  if(!missing(class_name)) .data <- .data %>% filter(kipp_hs_class_c==class_name)

  .data %>%
    #filter(kipp_hs_class_c==class_name) %>%
    inner_join(enrollment_c, by = c("id" = "student_c")) %>%
    inner_join(account %>% select(school_c = id,
                                  school_name = name,
                                  type,
                                  adjusted_6_year_graduation_rate_c,
                                  adjusted_6_year_minority_graduation_rate_c),
               by = "school_c") %>%
    mutate(start_date = ymd(start_date_c),
           node_grade = ifelse(starting_grade_c <=8, 8, starting_grade_c),
           sy = calc_academic_year(start_date_c)) %>%
    arrange(id, node_grade) %>%
    select(id, last_name, first_name, kipp_hs_class_c, school_name, node_grade, start_date, type, status_c,
           adjusted_6_year_graduation_rate_c,
           adjusted_6_year_minority_graduation_rate_c)
}

hs_class <- function(current_grade){
  current_sy <- silounloadr::calc_academic_year(lubridate::today(),
                                              format = "second_year")
  years_to_graduate <- 12 - current_grade
  current_sy + years_to_graduate
}
