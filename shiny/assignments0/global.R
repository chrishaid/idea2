# global.R for session
message("Loading libraries for global environment")

require(shinydashboard)
require(shiny)
#require(Cairo)
require(lubridate)
require(ggplot2)
require(dplyr)
require(tidyr)



# Shiny options
#options(shiny.usecairo = TRUE)

# load attendance data
load("/data/assignments.Rda")

source("lib/ass_helpers.R")

quarters <- c("Q1", "Q2", "Q3", "Q4")

courses <- sort(unique(assignments$course_number))
