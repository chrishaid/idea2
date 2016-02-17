# Attendance and enrollment

source('lib/attendance_helpers.R')

options(shiny.reactlog = TRUE)
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

  output$leaders <- DT::renderDataTable(attend_student_ytd)



})
