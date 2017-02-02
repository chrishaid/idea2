enroll_attend_plot <- function(daily_data, weekly_data){

  weekly_ypos <- daily_data %>%
    group_by(name) %>%
    mutate(y_pos = min(value)) %>%
    select(name, week_of_date_short_label, y_pos) %>%
    unique()

  weekly_data <- weekly_data %>%
    left_join(weekly_ypos,
              by = c("name", "week_of_date_short_label"))

  single_day_weeks <- daily_data %>%
    select(date, week_of_date_short_label) %>%
    unique() %>%
    dplyr::count(week_of_date_short_label) %>%
    filter(n<2)

  if(nrow(single_day_weeks)>0){
    daily_data_multi_day_weeks <- daily_data %>%
      anti_join(single_day_weeks, by="week_of_date_short_label")

      p <- ggplot(daily_data_multi_day_weeks,
                  aes(y= value ,
                      x=day))
  } else {
    p <- ggplot(daily_data,
                aes(y= value ,
                    x=day))
         }

  p <- p +
    geom_step(direction="hv",
              aes(color=variable),
              size=1) +
    geom_point(data=filter(daily_data, variable=="present"),
               color="black",
               size=3) +
    geom_text(data=weekly_data,
              aes(x=max(wday(date)),
                  y=y_pos,
                  label=sprintf("YTD ADA: %s\nWeekly ADA: %s",
                                round(ytd_ada,1),
                                round(weekly_ada,1)),
                  color=threshold
              ),
              hjust=1,
              vjust=0,
              size=4,
              inherit.aes=FALSE,
              show.legend = FALSE

    ) +
    scale_x_continuous(breaks = c(2,3,4,5,6), labels=c("M","T","W","R","F")) + #Change numberd week days to lettered
    scale_y_continuous("# of Students") +
    scale_colour_manual( "", values=c("#439539", "#8D8685" , "black",  "#E27425", "#439539"),
                        labels = c("Enrolled", "96% of Enrolled", "Attended",  "Weekly < 96%", "Weekly ≥ 96%")
                        ) +
    facet_grid(name ~ week_of_date_short_label, scale = "free_y") +
    theme_bw() +
    theme(legend.position = "top") +
    labs(x="Day of Week")

  # return
  p
}


# student attendance histogram
student_histogram <- function(data) {
  p <- ggplot(data,
              aes(x = ada)) +
    geom_histogram(aes(fill = ada >= 96),
                   color = "white",
                   binwidth = 2) +
    viridis::scale_fill_viridis(discrete=TRUE) +
    facet_grid(schoolabbreviation ~ grade_level) +
    theme_bw()

  #return
  p
}

# Daily goal plot ####
att_daily_goal_plot <-  function(.data, start_date, end_date, goals, show_goals){

  date_interval <- ymd(start_date) %--% ymd(end_date)

 x <- .data %>%
   ungroup() %>%
   filter(date %within% date_interval) %>%
   left_join(goals, by = "schoolabbreviation") %>%
   mutate(att_goal = round(enrolled*goal),
          diff = present - att_goal,
          school_goal = sprintf("%s\nGoal = %s%%",
                                schoolabbreviation,
                                round(100*goal,1)
                                )
          )

  p <- ggplot(x, aes(x = date, y = diff)) +
    geom_hline(aes(yintercept = 0), color = "gray40") +
    geom_linerange(aes(ymin = 0, ymax = diff, color = diff<0),
                   size = 1) +
    geom_point(aes(color = diff<0),
               size = 1.5) +
    scale_color_manual(values = c("black", "red")) +
    facet_grid(school_goal ~ .) +
    theme_bw() +
    guides(color = "none") +
    labs(y = "Deviation from goal\n(Number of students)",
         x = "Date")

   if(show_goals) {
     p <- p +
       geom_text(data = x %>% filter(diff >= 0),
                 aes(label = round(pct_present*100,1)),
                 color = "black",
                 nudge_y = 3,
                 size=2.5,
                 check_overlap = TRUE) +
       geom_text(data = x %>% filter(diff<0) ,
                 aes(label = round(pct_present*100,1)),
                 color = "red",
                 nudge_y = -3,
                 size=2.5,
                 check_overlap = TRUE)
   }

 p

}
