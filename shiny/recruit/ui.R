# Registration and Applicatons Shiny US

library(shiny)
#library(shinysky)


shinyUI(
  fluidPage(
    tags$head( 
      tags$link(href='static/css/dataTables.tableTools.css', rel="stylesheet", type="text/css"), 
      tags$script(src='static/js/jquery.dataTables.js'),
      tags$script(src='static/js/dataTables.tableTools.js')
    ), 
    div(class="container-fluid",
        tabsetPanel(#type="pills",
          tabPanel(title="Applications",
                   h4("YTD Applications"),
                   fluidRow(column(7,
                                   div(class="alert alert-info",
                                     checkboxInput("focusGrades",
                                                 "Show Focus Grades Only",
                                                 TRUE)
                                     )
                                   )
                            ),
                   fluidRow(
                     
                     column(5, 
                            dataTableOutput("tblApps")
                     ),
                    column(7, 
                           plotOutput("plotApps")
                           )
                   )
                  ),
          tabPanel(title="Registrations",
                   h4("YTD Registrations"),
                   
                   fluidRow(
                     
                     column(5, 
                            dataTableOutput("tblRegs")
                            ),
                     column(7, 
                            plotOutput("plotRegs")
                            )
                     )
                   )
          )
        )
  )
)
                              