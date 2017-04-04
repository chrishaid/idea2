create_wide_table <- function(.data, grade = c(5,6,7,8),
                              curric = c("Wheatly", "Eureka"),
                              metric = c("avg_pct_correct",
                                         "pct_bm",
                                         "n_students",
                                         "n_bm")){

  .data %>%
  filter(grepl(curric, test_name_factor),
         grade_level == grade) %>%
  ungroup() %>%
  arrange(date_taken_1) %>%
  mutate(test_name_short = forcats::fct_reorder(test_name_short, date_taken_1),
       pct_bm = round(100*pct_bm,1),
       metric = metric) %>%
  select_(#"assessment_id",
         "grade_level",
         "school",
         "metric",
         "test_name_short",
         metric) %>%
  tidyr::spread_(key = "test_name_short", metric)
  }
