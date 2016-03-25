# global.R for session
message("Loading libraries for global environment")
require(shiny)
require(Cairo)
require(lubridate)
require(ggplot2)
require(dplyr)
require(tidyr)
require(shinydashboard)
require(mapvizieR)

# Shiny options
options(shiny.usecairo = TRUE)

# load attendance data
load("/data/map.Rda")
