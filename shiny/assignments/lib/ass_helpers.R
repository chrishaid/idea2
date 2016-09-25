# Calc summary stats
calc_cums <- function(data) {

  grade_order <- rev(c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "F"))
  grade_cols <- scales::brewer_pal("div", "RdYlGn")(length(grade_order))

  grade_scale <- data_frame(grade_order = factor(grade_order, levels = grade_order),
                            grade_cols)

  quarter_dates <- as.POSIXlt(c("2016-08-22",
                                "2016-10-24",
                                "2017-01-23",
                                "2017-04-03",
                                "2017-06-16"))

  quarters <- c("Q1", "Q2", "Q3", "Q4")

  data %>%
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

}



# Function to set floor on scores for Fs
adj_fs <- function(ass_data, min_f = 65) {
  ass_data %>%
    mutate(score = ifelse(percent<min_f,
                          min_f/100*totalpointvalue,
                          score)
    )
}


# Fucntion for generating assignmnets bubble plot and grades
# histogram

ass_plot <- function(ass_data, course, quarter = "Q2"){

  quarter_in <- quarter

  ass_data <- ass_data %>% filter(quarter == quarter_in,
                                  course_number == course)


  # ass_data_max_dates <- ass_data %>%
  #   group_by(course_number, studentid, lastfirst) %>%
  #   filter(cum_weighted_points_possible == max(cum_weighted_points_possible),
  #          course_number == course)

  grade_order <- rev(c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "F"))
  grade_cols <- scales::brewer_pal("div", "RdYlGn")(length(grade_order))

  grade_scale <- data_frame(grade_order = factor(grade_order, levels = grade_order),
                            grade_cols)

  ass_data_max_dates <- ass_data %>%
    current_cum_grade

  ordered_names <- ass_data_max_dates %>% ungroup() %>%
    select(lastfirst, cum_weighted_avg) %>%
    arrange(cum_weighted_avg)

  ass_data_max_dates <- ass_data_max_dates %>% ungroup() %>%
    mutate(lastfirst = factor(lastfirst, levels = ordered_names$lastfirst))


  ass_data <- ass_data %>% ungroup() %>%
    mutate(lastfirst = factor(lastfirst, levels = ordered_names$lastfirst),
           date = ymd(date))

  min_date <- min(ass_data$date)
  max_date <- max(ass_data$date)

  mid_date <- min_date + (max_date - min_date)/2

  school <- stringr::str_extract(course, "kams|kccp|kbcp|koa|kop")  %>%  stringr::str_to_upper()
  grade <-  stringr::str_extract(course, "\\d{1}")
  cname <-  stringr::str_replace(course, ".+\\d{1}(.+)", "\\1") %>%  stringr::str_to_upper()

  title_text <- sprintf("%s | %s | %s",
                        school,
                        grade,
                        cname)


  p_scores <- ggplot(ass_data %>% filter(!is.na(lastfirst)),
                     aes(x=date, y=weighted_percent)) +
    geom_text(data = ass_data_max_dates,
              aes(y=0, x= mid_date,
                  label=round(cum_weighted_avg*100),
                  color=grade_cols
              ),
              alpha = .5,
              size = 12,
              vjust = 0,
              show.legend =FALSE) +
    geom_line(aes(group=id, y=cum_weighted_avg)) +
    geom_point(aes(color=grade_cols, size = weighted_points_possible, shape = exempt)) +
    facet_wrap(~lastfirst) +
    scale_color_identity("Grades",
                         labels = grade_scale$grade_order,
                         breaks = grade_scale$grade_cols,
                         guide = "legend") +
    scale_shape_manual(values=c(16,21)) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title_text,
         x = "Date",
         y = "Percent",
         color = "Grade",
         shape = "Exempt",
         size = "Relative\nWeight")


  p_histo<-ggplot(ass_data_max_dates,
                  aes(x=cum_grade)) +
    geom_bar(aes(fill = grade_cols)) +
    scale_fill_identity() +
    theme_bw() +
    labs(x = "Current Grade",
         y = "# of Students",
         title = title_text)

  out <- list()

  out$ass <- p_scores
  out$histo <- p_histo

  out
}


# function to get current grade calculation

current_cum_grade <- function(data) {
  grade_order <- rev(c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "F"))
  grade_cols <- scales::brewer_pal("div", "RdYlGn")(length(grade_order))

  grade_scale <- data_frame(grade_order = factor(grade_order, levels = grade_order),
                            grade_cols)

  data %>%
    group_by(course_number, id, lastfirst) %>%
    filter(ymd(date)==max(ymd(date))) %>%
    filter(assignmentid == max(assignmentid),
           !is.na(lastfirst)) %>%
    select(-starts_with("grade_cols")) %>%
    inner_join(grade_scale, by = c("cum_grade" = "grade_order"))

}
