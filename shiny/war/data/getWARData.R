system('touch start.txt')
message('Sourcing daily attendence data')
source('data/Attendance.R')

message('Sourcing students enrolled on 1 Oct 2013')
source('data/Enrolled.131001.R')
message('Sourcing students enrolled on 1 Oct 2014')
source('data/Enrolled.141001.R')
message('Sourcing students enrolled on 1 Oct 2015')
source('data/Enrolled.151001.R')


message('Telling Shiny Server to reload')
system('touch restart.txt')