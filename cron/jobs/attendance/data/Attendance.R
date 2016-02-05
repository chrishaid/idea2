# Script to retreive Attendance from KIPP Chicago's PowerSchool instance

message("Loading required libraries . . . ")
rm(list=ls())
#options( java.parameters = "-Xmx3G" )
require(ETLUtils)
library(RJDBC)
library(lubridate)

# Connect to server using JDBC Connection (note: requires a VPN connection to be open to psshostingvpn.poowerschool.com)

message("Logging into server . . . ")


drvr <- JDBC("oracle.jdbc.driver.OracleDriver", "/var/lib/jdbc/ojdbc6.jar","") # define driver
drvr <- JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
             classPath = "/var/lib/jdbc/ojdbc7.jar",identifier.quote = "") # define driver


pspw <- as.list(read.dcf("config/ps.dcf", all=TRUE)) #read DCF with configuration settings

# Construct SQL statement based today's date
date.first  <- pspw$FIRST_DAY # first day of school year
date.second <- lubridate::today() # 


message("Constructin SQL statement . . . ")
sql.statement<-paste("SELECT 
    m.schoolid, 
                     m.grade_level,
                     m.calendardate, 
                     m.STUDENT_NUMBER AS StudentID, 
                     m.lastfirst,
                     CASE 
                     WHEN m.enroll_status=0 THEN 1 
                     ELSE 0
                     END AS Currently_Enrolled,
                     m.Enrolled,
                     m.home_room,
                     a.Att_Code,
                     a.Description as AttDescr,
                     a.Presence_Status_CD,
                     a.COURSE_CREDIT_POINTS,
                     CASE 
                     WHEN  a.Presence_Status_CD = 'Absent' THEN a.COURSE_CREDIT_POINTS 
                     ELSE 0 
                     END as Absent
                     FROM (
                     SELECT
                     psmd.SchoolID,  
                     psmd.grade_level, 
                     psmd.calendardate, 
                     psmd.studentid,
                     s.STUDENT_NUMBER,
                     s.LASTFIRST, 
                     s.enroll_status,
                     1 as Enrolled,
                     s.home_room
                     FROM PS_Membership_Defaults psmd
                     LEFT JOIN students s ON psmd.StudentID = s.id
                     Where 	calendardate >= TO_DATE('",date.first,"','yyyy-mm-dd')
                     AND  calendardate <= TO_DATE('",date.second,"','yyyy-mm-dd')
                     ) m
                     LEFT JOIN (
                     SELECT 
                     att.schoolid, 
                     att.StudentID,
                     att.Att_Date,
                     attc.Att_Code,
                     attc.Description,
                     attc.Presence_Status_CD,
                     attc.COURSE_CREDIT_POINTS
                     FROM Attendance att
                     INNER JOIN Attendance_Code attc ON att.Attendance_CodeID = attc.ID
                     WHERE 
                     att.Att_Mode_Code = 'ATT_ModeDaily'
                     AND att.Att_Date >= TO_DATE('",date.first,"','yyyy-mm-dd')
                     AND att.Att_Date <= TO_DATE('",date.second,"','yyyy-mm-dd')
                     AND (attc.att_code = 'A' OR attc.att_code = 'S' or attc.att_code = 'X' or attc.att_code = 'H')
                     ) a
                     ON m.STUDENTID = a.studentid AND m.calendardate =a.Att_Date AND m.schoolID = a.schoolid
                     ORDER BY schoolid, grade_level, calendardate",sep="")
  
#Execture qurey and return to data frame   

message("Getting attendance data from PowerSchool . . . ")



Attendance <- read.jdbc.ffdf(query=sql.statement,
                             dbConnect.args = list( 
                               drv=drvr,
                               url=pspw$SERVER,
                               user=pspw$UID,
                               password=pspw$PWD
                             ),
                             first.rows=100,
                             next.rows=1000,
                             VERBOSE=TRUE
)






message("Writing attendance data to data/Attendance.csv")
write.csv(Attendance, file="data/Attendance.csv", row.names=FALSE)
message("Deleting Attendance object to clear memory")
rm(Attendance)



