


shinyServer(function(input, output, session) {

  output$cdf <- DT::renderDataTable(
                  map_mv_15$cdf,
                  filter = "top",
                  rownames = FALSE)


  map_sum <- reactive({
    if(input$norm == "2015"){
      map_sum <- map_sum_15
    } else {
      map_sum <- map_sum_11
    }

     norm_in <- input$norm
     map_sum <- map_sum %>%
       mutate(School = stringr::str_extract(
                         end_schoolname,
                         "Ascend|Create|Bloom"
                         ),
               nwea_norms = norm_in
             ) %>%
       filter(
         School %in% input$school,
         cohort_year >= 2020,
         end_map_year_academic >= 2011,
         measurementscale %in% input$subject
       )

    map_sum
    })


  output$plot <- renderPlot({
                   x <- map_sum()

                   p <- summary_long_plot(
                          x,
                          growth_window = input$growth_window,
                          metric = input$metric,
                          school_col = "School",
                          by = input$type
                          )

                  p
                })

  output$brush_test <- renderPrint({
                          str(input$map_brush)
                          # x <- map_sum()
                          # out<-brushedPoints(x, input$map_brush)
                          # out
    })

})
