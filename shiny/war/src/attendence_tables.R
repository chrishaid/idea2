#require(ProjectTemplate)

#if(!"Attendance" %in% project.info$data) load.project()

##### Daily Attendence ####

# By Grade

DailyEnrollAttendByGrade <-  Attendance %>% 
  data.frame %>%
  group_by(Date, School, Grade) %>%
  dplyr::summarize(Enrolled=sum(Enrolled), Absent=sum(Absent))

# By School
DailyEnrollAttend <- DailyEnrollAttendByGrade %>%
  dplyr::summarize(Enrolled=sum(Enrolled), Absent=sum(Absent))

# By HR

DailyEnrollAttendByHR <-  Attendance %>% 
  data.frame %>%
  group_by(Date, School, Grade, Home_Room) %>%
  dplyr::summarize(Enrolled=sum(Enrolled), Absent=sum(Absent))




DEA.list<-list(DailyEnrollAttend=DailyEnrollAttend, 
               DailyEnrollAttendByGrade=DailyEnrollAttendByGrade,
               DailyEnrollAttendByHR=DailyEnrollAttendByHR)

DEA.list<-lapply(DEA.list, function(x) x<- prep_Att_Summary_Tables(x))


DailyEnrollAttend<-DEA.list$DailyEnrollAttend
DailyEnrollAttendByGrade<-DEA.list$DailyEnrollAttendByGrade
DailyEnrollAttendByHR<-DEA.list$DailyEnrollAttendByHR


#### Calculate Attendance rate by week by school ####

AttRateByWeekBySchool <- DailyEnrollAttend %>%
  group_by(School, WeekOfShortDateLabel) %>%
  dplyr::summarize(AttRate=sum(Present)/sum(Enrolled)*100) %>%
  arrange(School)

AttRateYTDBySchool <- DailyEnrollAttend %>%
  group_by(School) %>%
  dplyr::summarize(AttRate=sum(Present)/sum(Enrolled)*100) %>%
  mutate(WeekOfShortLabel="YTD Each School") %>%
  arrange(School)


AttRateByWeekBySchool.table<-cast(AttRateByWeekBySchool, 
                                  WeekOfShortDateLabel ~ School, 
                                  value="AttRate")


AttRateYTDBySchool<-cast(AttRateYTDBySchool, 
                         WeekOfShortLabel ~ School, 
                         value="AttRate") %>%
  dplyr::rename(WeekOfShortDateLabel=WeekOfShortLabel)
#setnames(AttRateYTDBySchool, c("WeekOfShortDateLabel", "KAP", "KAMS", "KCCP", "KBCP"))




AttRateYTDRegion<-data.frame(WeekOfShortDateLabel="YTD Region", 
                             KAP= sum(DailyEnrollAttend$Present)/sum(DailyEnrollAttend$Enrolled)*100, 
                             KAMS=NA,
                             KCCP=NA, 
                             KBCP=NA)
DailyEnrollAttend_KACP<-filter(DailyEnrollAttend, School %in% c("KAMS", "KAP"))
AttRateYTDKAPKAMS<-data.frame(WeekOfShortDateLabel="YTD KACP", 
                              KAP=sum(DailyEnrollAttend_KACP$Present)/sum(DailyEnrollAttend_KACP$Enrolled)*100, 
                              KAMS=NA,
                              KCCP=NA, 
                              KBCP=NA)


AttRateByWeekBySchool.table<-rbind(AttRateByWeekBySchool.table,
                                   AttRateYTDBySchool,AttRateYTDKAPKAMS, 
                                   AttRateYTDRegion)
AttRateByWeekBySchool.table[,c(2:5)] <- round(AttRateByWeekBySchool.table[,c(2:5)],1)  

#a better column title
names(AttRateByWeekBySchool.table)[names(AttRateByWeekBySchool.table)=="WeekOfShortDateLabel"]<-"Week of"

#setnames(AttRateByWeekBySchool.table, 1, "Week of") 


#### Attendance by Student by School ####

AttByStudentBySchool<-Attendance %>% data.frame %>%
  filter(CurrentStatus==1) %>%
  group_by(StudentID,Student, School, Grade) %>%
  dplyr::summarize(Absences=sum(Absent), 
            ADA=round((1-(sum(Absent)/sum(Enrolled)))*100,1)) %>%
  group_by(School, Grade) %>% data.frame %>%
  mutate(N=n(), 
         Pctl=rank(ADA)/N) %>%
  select(-N) %>%
  filter(Pctl<=.10) %>%
  arrange(School, Grade, ADA)
              
# AttByStudentBySchool<-
#   copy(Attendance[CurrentStatus==1,list(Absences=sum(Absent), 
#                                        ADA=round((1-(sum(Absent)/sum(Enrolled)))*100,1)), 
#                  by=list(StudentID,Student, 
#                          School, 
#                          Grade)
#                  ][,list(StudentID,Student,
#                          ADA, 
#                          Absences,
#                          Pctl=rank(ADA)/.N),
#                    by=list(School, 
#                            Grade)
#                    ][Pctl<=.10][order(School, Grade, ADA)]
#   )

# ADA over previous 28 days

ADA_28<- Attendance %>% data.frame %>%
  dplyr::filter(Date>=ymd(as.character(today() - days(28)))) %>%
  group_by(StudentID) %>%
  dplyr::summarize( 
         ADA_28=round(100*(1-(sum(Absent)/n())),1)) %>%
  select(StudentID, ADA_28)

#  Attendance[Date>=ymd(as.character(today() - days(28)))][,list(ADA_28=round(100*(1-(sum(Absent)/.N)),1)), 
#                                                          by=list(StudentID)]

#setkey(ADA_28.dt, StudentID)
#setkey(AttByStudentBySchool, StudentID)

AttByStudentBySchool <- left_join(AttByStudentBySchool, ADA_28, by="StudentID") %>%
  arrange(School, ADA, ADA_28, Grade, Student) %>%
  select(School, 
         Grade, 
         Student, 
         ADA, 
         "ADA (prior month)" = ADA_28, 
         Absences) 

#AttByStudentBySchool<-copy(ADA_28.dt[AttByStudentBySchool][order(School, Grade, ADA)][,list(School, Grade, Student, ADA, ADA_28, Absences)])


#setnames(AttByStudentBySchool, c("School", "Grade", "Student","ADA", "ADA (prior month)"  ,"Absences"))








