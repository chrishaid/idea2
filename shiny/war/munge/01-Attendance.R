# Munching/processing script of Attendance table for Weekly Attendence Report
#Attendance<-as.data.table(Attendance)

message("Munging main Attendance table . . . ")
# Assign school initals based on school id

Attendance <- Attendance %>% 
  filter(ymd_hms(CALENDARDATE)!=mdy('01/09/2015')) %>% # day removed by CPS due to cold tempuratures
  mutate(School=school_abbrev(SCHOOLID),
         School=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP")),
         Grade=factor(GRADE_LEVEL, 
                            levels=c(0,1,2,3,4,5,6,7,8),
                            labels=c("K",1,2,3,4,5,6,7,8))
         ) %>% 
  select(SchoolID=SCHOOLID,
         Grade,
         Home_Room=HOME_ROOM,
         Date=CALENDARDATE,
         StudentID=STUDENTID,
         Student=LASTFIRST,
         CurrentStatus=CURRENTLY_ENROLLED,
         Enrolled=ENROLLED,
         AttCode=ATT_CODE,
         AttDescr=ATTDESCR,
         PresenceStatusCode=PRESENCE_STATUS_CD,
         CourseCreditPoints=COURSE_CREDIT_POINTS,
         Absent=ABSENT,
         School
         ) %>%
  mutate(Date=ymd_hms(Date)) 



