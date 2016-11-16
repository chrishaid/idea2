
# function takes prepped student data and returns timevis object
plot_timeline <- function(.data, students = NULL){

  if(!missing(students)) .data <- .data %>% filter(id %in% students)

  vis_data_1 <- .data %>%
    filter(type != "KIPP School") %>%
    group_by(id) %>%
    arrange(start_date) %>%
    mutate(end = lead(start_date),
           row_num = row_number(id),
           end = if_else(row_num == max(row_num) & is.na(end),
                         lubridate::today(),
                         end),
           id_row_num = sprintf("%s_%s", id, row_num),
           title = sprintf("%s\nType: %s\nStart: %s\nEnd %s\nStatus: %s",
                           school_name,
                           type,
                           start_date,
                           end,
                           status_c)
           )

 # Create status to color map
  color_map <- frame_data(
  ~status_c,           ~style,
  "Graduated",         "color: green;",
  "Transferred Out",   "color: gold;",
  "Withdrawn",         "color: red;",
  "Attending",         "color: blue;",
  "Matriculated",      "color: gray;",
  "Other",             "color: gray;",
  "Did Not Enroll",    "color: red",
  "Deferred",          "color: orange"
)

vis_data <- vis_data_1 %>%
  inner_join(color_map, by = "status_c") %>%
      select(id = id_row_num,
           start = start_date,
           end,
           content = school_name,
           group = id,
           title,
           style) %>%
    ungroup()

# Create group data frame
group_data <- .data %>%
  mutate(content = sprintf("%s, %s", last_name, first_name)) %>%
  select(id, content) %>%
  distinct() %>%
  arrange(content)

timevis::timevis(data = vis_data,
                 groups = group_data)
}


# Prep classes
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
