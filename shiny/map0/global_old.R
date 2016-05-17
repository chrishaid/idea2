# global.R for session
message("Loading libraries for global environment")

require(shinydashboard)
require(shiny)
require(Cairo)
require(lubridate)
require(ggplot2)
require(dplyr)
require(tidyr)



# Shiny options
#options(shiny.usecairo = TRUE)

# load attendance data
load("/data/map.Rda")

#source("helper.R")
