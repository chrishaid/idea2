
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
  "Transferred out",   "color: red;",
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
                 groups = group_data,

                 options = list(
                                verticalScroll = TRUE,
                                horizontalScroll = FALSE,
                                zoomable = FALSE)
                 )
}

# function takes prepped student data and returns ggplot

hs_app <- function(.data, school_n) {

  data_c <- .data %>%
    ungroup %>%
    filter(school == school_n) #school specific

  ##order names and keeping color order
  order_name_col <- data_c %>%
    spread(key=status, value = n_applications) %>%
    arrange(submitted, no_status, wishlist) %>%
    select(name, color_name) %>%
    unique

  #setting name levels
  data_ordnames <- data_c %>%
    mutate(name = factor(name, levels = order_name_col[[1]]))

  #order statuses
  order_status <- .data %>%
    group_by(status) %>%
    summarise(N=sum(n_applications)) %>%
    arrange(N) %>%
    select(status) %>%
    unique()

  #setting status levels
  data_g <- data_ordnames %>%
    ungroup() %>%
    mutate(status = factor(status, levels = order_status[[1]]),
    label_n = ifelse(status %in% "submitted" & n_applications > 0,
                            n_applications, ""))

  ##plotting data
  data_g %>%
    #filter(counselor_n == counselor) %>%
    ggplot(aes(x = name, y= n_applications, fill= status)) +
    geom_bar(show.legend = TRUE,
             stat = "identity",
             color = "white") +
    geom_text(aes(label=label_n), size = 2.5, hjust = 2, color= "white", fontface="bold") +
    viridis::scale_fill_viridis(discrete = T, direction = -1, option = "D",
                                breaks = c("submitted",
                                           "wishlist",
                                           "in_progress",
                                           "withdrew_application",
                                           "incomplete",
                                           "no_status"),
                                labels = c("Submitted",
                                           "Wishlist",
                                           "In Progress",
                                           "Withdrew Application",
                                           "Incomplete",
                                           "No Status"),
                                drop = FALSE) +
    labs(x=NULL, y=NULL, fill = "Submission Status") +
    theme_bw() +
    theme(legend.justification = "top",
          legend.text = element_text(size=11),
          legend.title = element_text(size=11),
          axis.text.y = element_text(size = 9,
                                     color = order_name_col[[2]])) +
    coord_flip()
}

##Produces match ggplot####
match_plot <- function(match_data, accepted_data){
  ggplot() +
    geom_rect(data = match_data %>%
                filter(undermatch %in% "undermatched"),
              aes(linetype = "undermatched"),
              fill = "#255694",
              xmin = -Inf,
              xmax = Inf,
              ymin = -Inf,
              ymax = Inf) +
    geom_rect(data = match_data %>%
                filter(!undermatch %in% "undermatched"),
              aes(linetype = "matched"),
              fill = "#CFCCC1",
              xmin = -Inf,
              xmax = Inf,
              ymin = -Inf,
              ymax = Inf) +
    geom_dotplot(data = accepted_data %>%
                   filter(school_matches %in% "no_match",
                          name.x != name.y),
                 aes(x = plot,
                     y = ecc,
                     fill = school_matches),
                 binwidth = 5,
                 binaxis = "y",
                 stackdir = "center",
                 dotsize = 2) +
    geom_dotplot(data = accepted_data %>%
                   filter(school_matches %in% "match_school",
                          name.x != name.y),
                 aes(x = plot,
                     y = ecc,
                     fill = school_matches),
                 binwidth = 5,
                 binaxis = "y",
                 stackdir = "center",
                 dotsize = 2) +
    geom_dotplot(data = match_data,
                 aes(x = plot,
                     y = enroll_ecc,
                     fill = "enrolled"),
                 binwidth = 5,
                 binaxis = "y",
                 stackdir = "center",
                 dotsize = 2) +
    geom_hline(data = match_data %>%
                 filter(undermatch %in% "undermatched"),
               aes(yintercept = umatch_bound,
                   color = "umatch_bound"),
               linetype = 2) +
    geom_hline(data = match_data,
               aes(yintercept = 25),
               color = "white",
               alpha = 0.25) +
    geom_hline(data = match_data,
               aes(yintercept = 50),
               color = "white",
               alpha = 0.25) +
    geom_hline(data = match_data,
               aes(yintercept = 75),
               color = "white",
               alpha = 0.25) +
    facet_wrap("student_name", ncol = 6) +
    scale_color_manual("", values = c("umatch_bound" = "#C49A6C"),
                       breaks = "umatch_bound",
                       labels = "Undermatch Boundary") +
    scale_fill_manual("", values = c("enrolled" = "#E27425",
                                     "no_match" = "white",
                                     "match_school" = "#FEDA00"),
                      breaks = c("enrolled",
                                 "match_school",
                                 "no_match"),
                      labels = c("Enrollment",
                                 "Matching Colleges",
                                 "Non-matching Colleges")) +
    scale_x_discrete(breaks = c("school"),
                     labels = NULL) +
    scale_linetype_manual("", values = c("undermatched" = 0,
                                         "matched" = 0),
                          breaks = c("undermatched",
                                     "matched"),
                          labels = c("Undermatch",
                                     "Match"),
                          guide = guide_legend(override.aes =
                                            list(fill =
                                                c("undermatched" = "#255694",
                                                  "matched" = "#CFCCC1")))) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = c(0.6, 0.08),
          legend.margin = margin(t=-10, b=-10),
          legend.box = "horizontal") +
    labs(x = "College Acceptances",
         y = "Expected College Completion (%)")
}
