
shinyServer(function(input, output, session) {

  ass_adj <- reactive({

          assignments %>%
            ungroup() %>%
            filter(course_number == input$course) %>%
            adj_fs(input$min_f) %>%
            select(-starts_with("grade_cols")) %>%
            calc_cums


      })

  selectedRows <- reactive({brushedPoints(p()$ass$data, input$plot_brush)})


  output$detail <-renderTable({
        req(input$plot_brush)

        all_brush <- selectedRows()



        all_brush %>%
          filter(course_number == input$course) %>%
          ungroup() %>%
          mutate(cum_weighted_avg = round(cum_weighted_avg*100,1),
                 Score = round(weighted_percent*100,1),
                 Date = format(date, format = "%b %e")) %>%
          select(Date,
                 Name = abbreviation, Score ,
                 "Cum Avg"= cum_weighted_avg,
                 "Cum Grade" = cum_grade) %>%
          arrange(Date)},

        # renderTable options
        include.rownames = FALSE,
        digits = 0
  )

  p <- reactive({ass_plot(ass_adj(),
                 quarter = input$quarter,
                 course = input$course)})


  output$bubbles <-renderPlot({p()$ass})


  output$histo <-renderPlot({p()$histo}, height = 200)

  output$tab <- renderTable({
    ass_adj() %>%
      current_cum_grade %>%
      group_by(cum_grade) %>%
      summarize(N_grade = n()) %>%
      mutate(pct = round(N_grade/sum(N_grade)*100)) %>%
      select("Letter Grade" = cum_grade,
             "# of Students Earning" = N_grade,
             "% of Students Earning" = pct)},
    include.rownames = FALSE,
    alignment = c("lrr"),
    digits = 0)


})
