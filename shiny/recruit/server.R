# Test server.R Script for tabbed attendance data usuing the DataTable javascript package
# implemented with Shiny Server.

# creates absoulute path to static resources for IDEA
addResourcePath('static', '/var/www/')

load('/data/recruiting.Rda')


#### Shiny Server output code ####
shinyServer(function(input, output) {

# Registrations
    output$plotRegs<-renderPlot({
      ggplot(reg_goals_actual, aes(x=date_eow, y=goal)) +
          geom_bar(stat="identity", fill = NA, color = "black") +
          geom_bar(aes(y=actual, fill=cols, color = cols), stat="identity") +
          geom_hline(aes(yintercept = overall_reg_goal)) +
          scale_fill_identity(guide="none") +
          scale_color_identity(guide="none") +
          facet_wrap(school ~ grade, scales = "free_y") +
          theme_bw() +
          labs(x = "Date",
               y = "Count")

      })



    output$reg_tbl <- renderText({
      if(is.null(input$reg_hover)) {

        #data_frame("Hover near bars for data" = NA)
      return("Hover near bars for data")
      } else {

      reg_data  <- nearPoints(reg_goals_actual, input$reg_hover,threshold = 20, maxpoints = 1)


      html_text <- sprintf(
        "<b>School:</b> %s<br>
         <b>Grade:</b> %s<br>
         <b>Date:</b> %s<br>
         <b>Goal:</b> %s<br>
         <b>Actual:</b> %s<br>
         <b>Overall Goal:</b> %s<br>
         <b>Percent of current goal:</b> %s%%<br>
         <b>Percent of overall goal:</b> %s%%<br>
         <b>Forgone/gained budget:</b> %s
        ",
        reg_data$school,
        reg_data$grade,
        reg_data$date_eow,
        reg_data$goal,
        reg_data$actual,
        reg_data$overall_reg_goal,
        round(100*reg_data$pct_of_goal,1),
        round(100*reg_data$actual/reg_data$overall_reg_goal,1),
        scales::dollar(7000*(reg_data$actual - reg_data$overall_reg_goal))
        )

      HTML(html_text)
      }
    })

# eifs
    output$plotEIFs<-renderPlot({
      ggplot(eif_goals_actual, aes(x=date_eow, y=goal)) +
          geom_bar(stat="identity", fill = NA, color = "black") +
          geom_bar(aes(y=actual, fill=cols, color = cols), stat="identity") +
          geom_hline(aes(yintercept = overall_eif_goal)) +
          scale_fill_identity(guide="none") +
          scale_color_identity(guide="none") +
          facet_wrap(school ~ grade, scales = "free_y") +
          theme_bw() +
          labs(x = "Date",
               y = "Count")

      })

      output$eif_tbl <- renderText({
        if(is.null(input$eif_hover)) {

          #data_frame("Hover near bars for data" = NA)
        return("Hover near bars for data")
        } else {

        eif_data  <- nearPoints(eif_goals_actual, input$eif_hover,threshold = 10, maxpoints = 1)


        html_text <- sprintf(
          "<b>School:</b> %s<br>
           <b>Grade:</b> %s<br>
           <b>Date:</b> %s<br>
           <b>Goal:</b> %s<br>
           <b>Actual:</b> %s<br>
           <b>Overall Goal:</b> %s<br>
           <b>Percent of current goal:</b> %s%%<br>
           <b>Percent of overall goal:</b> %s%%<br>
          ",
          eif_data$school,
          eif_data$grade,
          eif_data$date_eow,
          eif_data$goal,
          eif_data$actual,
          eif_data$overall_eif_goal,
          round(100*eif_data$pct_of_goal,1),
          round(100*eif_data$actual/eif_data$overall_eif_goal,1)
          )

        HTML(html_text)
        }
      })


})
