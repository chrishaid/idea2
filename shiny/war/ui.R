# Test ui.R Script for tabbed attendance data usuing the DataTable javascript package
# implemented with Shiny Server. This is derived from the war (weekly attendance
# report) data analysis project



lastXweeks <- ymd(as.character(floor_date(today() - weeks(6), unit="week")+1))
last6weeks <- as.character(lastXweeks)
thisweek <- as.character(today())
firstweek <- as.character(floor_date(min(attend_date_school_grade$week_of_date)))


restart_time<-file.info('restart.txt')$mtime

update_time_stamp <- lubridate::stamp("Attendance data last updated on Tuesday, September 14, 2001 at 4:41 pm")(restart_time)

header <- dashboardHeader(disable=TRUE)

sidebar <- dashboardSidebar(
  dateRangeInput("att_dates",
                 "Select dates:",
                 start = last6weeks,
                 end = thisweek,
                 min = firstweek,
                 max = thisweek,
                 format = "mm-dd-yyyy"
                ),
  radioButtons("att_level",
               "Select level:",
               choices = c("School" = "school",
                           "Grade" = "grade",
                           "Home Room" = "hr"),
               selected = "School"),
  conditionalPanel(condition = "input.att_level == 'grade' |
                                  input.att_level == 'hr'",
                   radioButtons("att_school",
                               "Select school:",
                               choices = schools,
                               selected = "KAMS")
                               ),
  conditionalPanel(condition = "input.att_level == 'hr'",
                   uiOutput("school_grades_ui"))
  )

  body <- dashboardBody(
    tabsetPanel(
      tabPanel("Daily Attendance",
            plotOutput("step_plot", height = 800)
          ),
        tabPanel("Students",
          fluidRow(column(3, selectInput("stu_grade",
                               "Select Grade",
                               choices  = 0:8,
                               selected = 5)),
                  column(9, plotOutput("student_histogram",
                                        height = 200,
                                        brush = brushOpts(
                                                  id = "plot_brush",
                                                  direction = "x")
                                        )
                        )
                  ),
          fluidRow(DT::dataTableOutput("students"))
          ),
        tabPanel("Deviation from Goals",
            fluidRow(
              column(8,
                plotOutput(
                  "att_goal_plot"
                )
              ),
              column(4,
                box(checkboxInput("show_ada",
                                    "Show ADA?",
                                    value = FALSE)
               )
              )
            )
        ),
        tabPanel("Leader Board",
          DT::dataTableOutput("leaders")
          )
        )
      )


shinyUI(



  dashboardPage(header, sidebar, body)
)
