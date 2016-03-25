summary_long_plot_2 <- function ()
{
    # growth_window_in <- growth_window
    #
    # if (by == "grade") {
    #     type <- "end_grade"
    #     x_var <- "SY"
    # }
    # else {
    #     mapvizieR_summary <- mapvizieR_summary %>%
    #     dplyr::group_by(end_schoolname,
    #         cohort_year) %>%
    #     dplyr::mutate(cohort = sprintf("%s\n(Current Grade: %s)",
    #         cohort_year, max(end_grade)), Grade = end_grade)
    #     type <- "cohort"
    #     x_var <- "end_grade"
    # }
    # x <- mapvizieR_summary %>% dplyr::ungroup() %>% dplyr::filter(growth_window ==
    #     growth_window_in, n_students >= n_cutoff) %>% dplyr::mutate(SY = sprintf("%s-%s",
    #     stringr::str_extract(end_map_year_academic, "\\d{2}$"),
    #     as.integer(stringr::str_extract(end_map_year_academic,
    #         "\\d{2}$")) + 1))

    data(diamonds)
    x <- diamonds
    p <- ggplot(x, aes(x = carat, y = price)) +
    #    geom_line(aes_(color = as.name(school_col), group = as.name(school_col))) +
        geom_point() #+

    #theme_bw(base_size = 10) #+
      #  theme(legend.position = "bottom", axis.text.x = element_text(size = 8,
      #      angle = 45, hjust = 1))
    #if (grepl("^pct_", metric) | x[metric] %in% c(0:1)) {
    #    p <- p + geom_text(aes_string(label = sprintf("round(%s*100)",
    #        metric), color = school_col), size = 3) + scale_y_continuous(labels = scales::percent)
    #}
    #else {
    #    p <- p + geom_text(aes_string(label = sprintf("round(%s)",
    #        metric), color = school_col), size = 2)
    #}
    # y_lab <- switch(metric, pct_typical = "% M/E Typical Growth",
    #     pct_accel_growth = "% M/E College Ready Growth", pct_negative = "% Pct Negative Change in RIT",
    #     end_pct_50th_pctl = "% ≥ 50th Percentile (End Season)",
    #     end_pct_75th_pctl = "% ≥ 75th Percentile (End Season)")
    # if (is.null(y_lab)) {
    #     if (grepl("testritscore", metric))
    #         y_lab <- "RIT Score"
    #     if (grepl("rit_growth", metric))
    #         y_lab <- "RIT Points"
    #     if (grepl("cgi", metric))
    #         y_lab <- "Standard Deviations"
    #     if (grepl("sgp", metric))
    #         y_lab <- "Student Growth Percentile"
    #     if (grepl("cgp", metric))
    #         y_lab <- "Cohort Growth Percentile"
    #     if (grepl("percentile", metric))
    #         y_lab <- "Percentile"
    #     if (is.null(y_lab))
    #         y_lab <- metric
    # }
    #p <- p + labs(y = y_lab)

    # if(type == "cohort") {
      p <- p + facet_grid(cut  ~ color)
    # } else {
    #  p <- p + facet_grid(measurementscale ~ end_grade)
    # }

    p

}
