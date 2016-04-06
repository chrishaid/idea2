# global.R for session
message("Loading libraries for global environment")
require(shiny)
library(Cairo)
require(lubridate)
require(ggplot2)
require(dplyr)
require(tidyr)
require(shinydashboard)

options(shiny.usecairo=TRUE) 
# Shiny options
options(shiny.usecairo = TRUE)

# load attendance data
load("/data/attendance.Rda")

schools <- unique(ada_weekly_school$schoolabbreviation)

grades <- list(KAP = 0:5,
               KAMS = 6:8,
               KCCP = 5:8,
               KBCP = 5:7)
