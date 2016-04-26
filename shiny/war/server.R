# Attendance and enrollment

source('lib/attendance_helpers.R')

# ADA goals for each school
goals <- data_frame(schoolabbreviation =  c("KAP", "KAMS", "KCCP", "KBCP"),
                    goal = c(.955))


#options(shiny.reactlog = TRUE)
shinyServer(function(input, output, session) {

  # get attendance date interval
  att_date_range <- reactive(input$att_dates[[1]] %--% input$att_dates[[2]])



  # create dynamic panel
  output$school_grades_ui <- renderUI({

    school_grades <- ada_weekly_grade_hr %>%
      filter(schoolabbreviation == input$att_school) %>%
      select(grade_level)

    grades <- school_grades$grade_level %>% unique()

    selectInput("att_grades",
                 "Choose grade",
                 choices = as.character(grades),
                 selected = as.character(grades[1]),
                 multiple = FALSE)
    })

  # create daily data!
  ada_daily <- reactive({

    # Get data at school level
   if(input$att_level == "school") {


    daily_out <- attend_date_school %>%
      select(name = schoolabbreviation,
             date,
             week_in_sy,
             week_of_date,
             week_of_date_short_label,
             enrolled,
             present) %>%
      mutate(goal = round(.97*enrolled),
             day = wday(date)) %>%
      gather(key = "variable",
             value = "value", enrolled:goal)

   }

    # Munge data for Grade level
    # To do: create menu item for school
    if(input$att_level == "grade") {

      daily_out <- attend_date_grade %>%
        filter(schoolabbreviation == input$att_school) %>%
        select(name = grade_level,
               schoolabbreviation,
               date,
               week_in_sy,
               week_of_date,
               week_of_date_short_label,
               enrolled,
               present) %>%
        mutate(goal = round(.97*enrolled),
               day = wday(date)) %>%
        gather(key = "variable",
               value = "value", enrolled:goal)

    }

    if(input$att_level == "hr") {

      daily_out <- attend_date_grade_hr %>%
        filter(grade_level == as.numeric(input$att_grades),
               schoolabbreviation == input$att_school
               ) %>%
        select(name = home_room,
               grade_level,
               schoolabbreviation,
               date,
               week_in_sy,
               week_of_date,
               week_of_date_short_label,
               enrolled,
               present) %>%
        mutate(goal = round(.97*enrolled),
               day = wday(date)) %>%
        gather(key = "variable",
               value = "value", enrolled:goal)

    }

  # return
  daily_out

  })







  # ada_weekly reactive
  ada_weekly <- reactive({

    # if school
    if(input$att_level == "school") {
      weekly_out <- ada_weekly_school %>%
        mutate(name = schoolabbreviation,
               threshold = ifelse(weekly_ada>=.97*100,
                                  sprintf("Weekly ≥ %s%%", round(.97*100)),
                                  sprintf("Weekly < %s%%",  round(.97*100))
                                  )
              )
     }



    # if grade
    if(input$att_level == "grade") {

      weekly_out <- ada_weekly_school_grade %>%
      filter(schoolabbreviation == input$att_school) %>%
        mutate(name = grade_level,
               threshold = ifelse(weekly_ada>=.97*100,
                                  sprintf("Weekly ≥ %s%%", round(.97*100)),
                                  sprintf("Weekly < %s%%",  round(.97*100))
                                  )
              )
     }

    # if home room
    if(input$att_level == "hr") {


      weekly_out <- ada_weekly_grade_hr %>%
        filter(grade_level == as.numeric(input$att_grades),
               schoolabbreviation == input$att_school
               ) %>%
        mutate(name = home_room,
               threshold = ifelse(weekly_ada>=.97*100,
                                  sprintf("Weekly ≥ %s%%", round(.97*100)),
                                  sprintf("Weekly < %s%%",  round(.97*100))
                                  )
              )
   }

  #return
  weekly_out

  })






  # Step plot of enrollment and attendance
  output$step_plot <- renderPlot({

    # verify that initial grade level exists for school if att_level == 'hr'
    # you need to do this because all reactives are flushed  and regenerated
    # with input changes but the renderUI has to make a round trip to the client
    # in get that input value.  Server to client to generate grades then back
    # again from client to server with user (or initial) grade selections
    if(input$att_level == 'hr')
      req(input$att_grades %in% ada_daily()$grade_level)

    # collect reactive values
    att_int <- att_date_range()
    daily <-  ada_daily()
    weekly <- ada_weekly()

    # filter by date
    daily2 <- daily %>% filter(date %within% att_int)
    weekly2 <- weekly %>% filter(date %within% att_int)

    enroll_attend_plot(daily2, weekly2)



    })


  students_filtered <- reactive({attend_student_ytd %>%
                         filter(grade_level == input$stu_grade)
                       })

  output$student_histogram <- renderPlot(
    student_histogram(students_filtered())
    )

  output$students <- DT::renderDataTable({
                        x <- students_filtered()
                        x <- brushedPoints(x, input$plot_brush)
                        x %>%
                          ungroup() %>%
                          mutate(
                            ada = round(ada,1),
                            grade_level = as.integer(grade_level)) %>%
                          select(
                              Student = lastfirst,
                              Grade = grade_level,
                               School = schoolabbreviation,
                               'Days Enrolled' = enrolled,
                               'Days Present' = present,
                               ADA = ada)
                        },
                        rownames = FALSE,
                        options = list(
                                    dom = "t"
                                  )
                    )

  output$att_goal_plot <- renderPlot({
    att_daily_goal_plot(
      attend_date_school,
      input$att_dates[[1]],
      input$att_dates[[2]],
      goals = goals,
      show_goals = input$show_ada)
    })


  output$leaders <- DT::renderDataTable(
                      attend_student_ytd %>% ungroup() %>%
                        filter(ada_rank <= .1) %>%
                        mutate(ada = round(ada,1),
                        grade_level = as.integer(grade_level)) %>%
                        select(Student = lastfirst,
                               Grade = grade_level,
                               School = schoolabbreviation,
                               'Days Enrolled' = enrolled,
                               'Days Present' = present,
                               ADA = ada),
                        filter = "top",
                        rownames = FALSE)


# Transfers #################################################################

  output$transfer_plot <- renderPlot({

    
    todays_month <- month(today(), label = TRUE, abbr = TRUE) %>%
      factor(levels = month_order, ordered = TRUE)

    transfer_scale <- c(scales::brewer_pal("qual", palette = 3)(nrow(transfer_reasons)-3),
                        "gray",
                        "lightgray"
                        )


    ggplot(transfers_by_month_2 %>%
             filter(!(month > todays_month &
                      sy == "2015-2016")
                    ),
           aes(x = month, y=cum_transfers_2)) +
      geom_bar(aes(fill = factor(reason), y=cum_transfers_2),
                position = "stack",
                stat = "identity") +
      geom_segment(data = transfer_goals,
                   aes(x=1, xend=12,
                       y=monthly_goal, yend= yearly_goal),
                   alpha = .6,
                   color = "purple") +
      facet_grid(sy ~ school_name) +
      scale_fill_manual(values = transfer_scale) +
      theme_light() +
      theme(axis.text.x = element_text(angle=45, hjust = 1)) +
      labs(fill = "Transfer\nReason",
           x = "Month",
           y = "Cumulative transfers\n(count)")

    })



})
