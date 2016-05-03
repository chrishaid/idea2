# Registration and Applicatons Shiny US

library(shiny)
library(shinydashboard)


header <- dashboardHeader(disable=TRUE)

sidebar <- dashboardSidebar(
  width = 131,
  sidebarMenu(
    menuItem("Registrations",
            tabName = "registration",
            selected = TRUE,
            icon = icon("child")
            ),

    menuItem("EIFs",
             tabName = "eif",
             icon = icon("file-text-o")
             )
  )

)

body <- dashboardBody(

  tabItems(
    tabItem("registration",
      h4("Student Regristration Goals and Results"),
      fluidRow(
        column(
          7,
          plotOutput("plotRegs",
                     hover="reg_hover",
                     height = 600
                    )
          ),
        column(5,box(htmlOutput("reg_tbl")))
      )
    ),
    tabItem("eif",
      h4("Enrollment Intent Form Goals and Results"),
      fluidRow(
        column(
          7,
          plotOutput("plotEIFs",
                     hover="eif_hover",
                     height = 600
                    )
          ),
        column(5,box(htmlOutput("eif_tbl")))
      )

    )
  )
)




shinyUI(
  dashboardPage(header, sidebar, body)
)
