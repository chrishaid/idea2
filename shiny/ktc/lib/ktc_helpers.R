
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
  "Transferred out",   "color: gold;",
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
