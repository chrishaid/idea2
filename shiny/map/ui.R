
header <- dashboardHeader(disable=TRUE)

sidebar <- dashboardSidebar(
              # School selector
              selectInput(
                inputId = "school",
                label = "Schools:",
                choices = c("Ascend", "Create", "Bloom"),
                selected = c("Ascend", "Create", "Bloom"),
                multiple = TRUE
              ),
              # subject selector
              selectInput(
                inputId = "subject",
                label = "Subjects:",
                choices = c("Mathematics", "Reading", "General Science"),
                selected = c("Mathematics", "Reading", "General Science"),
                multiple = TRUE
              ),
              # growth window selector
              selectInput(
                inputId = "growth_window",
                label = "Growth Window:",
                choices = c("Spring to Spring",
                            "Spring to Winter",
                            "Fall to Spring",
                            "Fall to Winter",
                            "Fall to Fall",
                            "Winter to Winter",
                            "Winter to Spring"),
                selected = "Spring to Winter",
                multiple = FALSE
              ),
              # metric selector
              selectInput(
                inputId = "metric",
                label = "Metric",
                choices = c("% M/E Typical Growth" = "pct_typical",
                            "% M/E CR Growth" = "pct_accel_growth",
                            "% Negative RIT Change" = "pct_negative",
                            "% ≥ 50th Pctl" = "end_pct_50th_pctl",
                            "% ≥ 75th Pctl" = "end_pct_75th_pctl",
                            "Mean RIT Score" = "end_mean_testritscore",
                            "Median RIT Score" = "end_median_testritscore",
                            "Mean Student Attainment Percentile"  = "end_median_consistent_percentile",
                            "Mean Student Growth Percentile" = "mean_sgp",
                            "Cohort Growth Percentile" = "cgp"),
                selected = "pct_typical",
                multiple = FALSE
              ),
              # Grade vs cohort selector
              selectInput(
                inputId = "type",
                label = "View by:",
                choices = c("Grade" = "grade",
                            "Cohort (Class of . . . )" = "cohort"),
                selected = "grade",
                multiple = FALSE
              ),
              # Norms selector
              selectInput(
                inputId = "norm",
                label = "Norms:",
                choices = c("2015",
                            "2011"),
                selected = "2015",
                multiple = FALSE
              )



            )

body <- dashboardBody(
          tabsetPanel(
            tabPanel("Vizualization",
              fluidRow(
                plotOutput(
                  "plot",
                   height = 500,
                   brush = brushOpts(
                     id = "map_brush"
                     )
                )
              ),
              fluidRow(
                verbatimTextOutput("brush_test")
                )
            ),
            tabPanel("Historical Scores",
              DT::dataTableOutput("cdf")
            )
          )
        )


shinyUI(
  dashboardPage(header, sidebar, body)
)
