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
    tabItem("eif")
  )
)




shinyUI(
  dashboardPage(header, sidebar, body)
)
