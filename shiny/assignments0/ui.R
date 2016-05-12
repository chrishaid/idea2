
header <- dashboardHeader(disable=TRUE)

sidebar <- dashboardSidebar(
  selectInput("quarter", "Quarter", choices = quarters, selected = "Q4"),

  selectInput("course", "Course", choices = courses, selected = "kams8ela"),

  sliderInput("min_f", "Minimum F", 0, min = 0, max = 69, step=5),

  div(tableOutput("detail"), style = "font-size:60%")
            )

body <- dashboardBody(
                plotOutput("bubbles",
                            height = 800,
                            brush = brushOpts(
                                      id = "plot_brush",
                                      direction = "x"
                                      )
                )
        )



shinyUI(
  dashboardPage(header, sidebar, body)
)
