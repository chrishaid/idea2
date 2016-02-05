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
                        labels = c("Enrolled", "97% of Enrolled", "Attended",  "Weekly < 97%", "Weekly ≥ 97%")
                        ) +
    facet_grid(name ~ week_of_date_short_label, scale = "free_y") +
    theme_bw() +
    theme(legend.position = "top") +
    labs(x="Day of Week")

  # return
  p
}
