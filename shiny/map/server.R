
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

                   p <- mapvizieR::summary_long_plot(
                           x,
                           growth_window = input$growth_window,
                           metric = input$metric,
                           school_col = "School",
                           by = input$type
                          )

                  p
                })

  output$brush_test <- DT::renderDataTable({
                          if(is.null(input$map_brush)) return()

                           # get data
                           x <- map_sum()


                           x <- x %>%
                            group_by(
                              end_schoolname,
                              cohort_year
                              ) %>%
                            dplyr::mutate(
                              cohort = sprintf(
                                        "%s\n(Current Grade: %s)",
                                        cohort_year,
                                        max(end_grade)),
                              Grade = end_grade,
                              SY = sprintf(
                                    "%s-%s",
                                    stringr::str_extract(end_map_year_academic, "\\d{2}$"),
                                    as.integer(stringr::str_extract(end_map_year_academic,
                                                                    "\\d{2}$")) + 1))

                            x <- brushedPoints(x, input$map_brush)

                            x <- x %>% ungroup() %>%
                              filter(growth_window == input$growth_window) %>%
                              select(SY,
                                    "Window" = growth_window,
                                    School,
                                    Grade,
                                    "Subject" = measurementscale,
                                    N = n_students,
                                    starts_with("pct_"),
                                    contains("median"),
                                    contains("percentile")
                                    ) %>%
                              select(-median_cgi,
                                    -median_sgp,
                                    "start_percentile" = start_median_consistent_percentile,
                                    "end_percentile" = end_median_consistent_percentile)
                            names(x) <- stringr::str_replace_all(names(x), "_", "\n")


                            x

    },
    rownames = FALSE,
    options = list(
                autoWidth = TRUE,
                columnDefs = list(list(width = '10px', targets = "_all")),
                dom = 't')
    )

})
